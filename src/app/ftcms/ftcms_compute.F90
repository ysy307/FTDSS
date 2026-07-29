!> Implementation overview
!>
!> Algorithm:
!> - Volume-weighted smoothing of element-wise state variables to nodes
!> - Solving the global linear system
!> - L2 projection (lumped mass) for nodal gradient calculations
!> - Evaluation of water and vapor fluxes based on the Darcy law
submodule(app_ftcms) ftcms_compute
    use :: core_types_topology_system_topology, only:type_system_topology
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    use :: module_core, only:type_matrix_bsr, type_matrix_info
    implicit none

    !> Verify that the linear solve returns an increment that actually satisfies
    !> the assembled system. Cheap (one extra matrix-vector product) and silent
    !> unless the defect exceeds the threshold.
    logical, parameter :: LINEAR_RESIDUAL_CHECK = .true.
    real(real64), parameter :: LINEAR_RESIDUAL_WARN = 1.0d-8
contains

    !> Smooth element-wise state variables to nodal values
    module subroutine update_variables_ftcms(self)
        implicit none
        class(type_ftcms), intent(inout) :: self

        integer(int32) :: i_node, i_elem, j
        integer(int32) :: num_nodes, num_neighbors, material_id
        integer(int32), pointer, contiguous :: element_list(:)
        type(type_state), allocatable :: states(:)
        real(real64) :: elem_qw, elem_qi, elem_qa, elem_qv
        real(real64) :: elem_vol
        real(real64) :: sum_vol
        real(real64) :: sum_qw_vol, sum_qi_vol, sum_qa_vol, sum_qv_vol
        integer(int32) :: num_threads, tid

        call self%control%profiler_start(PROFILER_TYPES%SETUP)
        call self%domain%get_num_nodes(num_nodes)
        num_threads = omp_get_max_threads()
        allocate (states(num_threads))

        !$OMP PARALLEL DEFAULT(NONE) &
        !$OMP SHARED(self, num_nodes, states) &
        !$OMP PRIVATE(i_node, element_list, num_neighbors, j, i_elem, &
        !$OMP         elem_vol, material_id, elem_qw, elem_qi, elem_qa, elem_qv, &
        !$OMP         sum_vol, sum_qw_vol, sum_qi_vol, sum_qa_vol, sum_qv_vol, &
        !$OMP         tid)
        tid = omp_get_thread_num() + 1
        nullify (element_list)
        !$OMP DO
        do i_node = 1, num_nodes
            call self%domain%element_adjacency%get_list(i_node, element_list)
            sum_vol = 0.0d0
            sum_qw_vol = 0.0d0
            sum_qi_vol = 0.0d0
            sum_qa_vol = 0.0d0
            sum_qv_vol = 0.0d0
            if (associated(element_list)) then
                num_neighbors = size(element_list)
                do j = 1, num_neighbors
                    i_elem = element_list(j)
                    call self%domain%calc_measure(i_elem, elem_vol)
                    call self%domain%get_material_id(i_elem, material_id)
                    call self%set_state(i_node, i_elem, states(tid))
                    call states(tid)%get(water_content=elem_qw, ice_content=elem_qi, &
                                         air_content=elem_qa, vapor_content=elem_qv)
                    sum_vol = sum_vol + elem_vol
                    sum_qw_vol = sum_qw_vol + (elem_qw * elem_vol)
                    sum_qi_vol = sum_qi_vol + (elem_qi * elem_vol)
                    sum_qa_vol = sum_qa_vol + (elem_qa * elem_vol)
                    sum_qv_vol = sum_qv_vol + (elem_qv * elem_vol)
                end do
            end if
            if (abs(sum_vol) > epsilon(1.0d0)) then
                call self%Qw%set_current(i_node, sum_qw_vol / sum_vol)
                call self%Qi%set_current(i_node, sum_qi_vol / sum_vol)
                call self%Qa%set_current(i_node, sum_qa_vol / sum_vol)
                call self%Qv%set_current(i_node, sum_qv_vol / sum_vol)
            else
                call self%Qw%set_current(i_node, 0.0d0)
                call self%Qi%set_current(i_node, 0.0d0)
                call self%Qa%set_current(i_node, 0.0d0)
                call self%Qv%set_current(i_node, 0.0d0)
            end if
        end do
        !$OMP END DO
        !$OMP END PARALLEL

        if (allocated(states)) deallocate (states)
        call self%control%profiler_stop(PROFILER_TYPES%SETUP)
    end subroutine update_variables_ftcms

    !> Solve the global linear system
    module subroutine solve_ftcms(self)
        implicit none
        class(type_ftcms), intent(inout) :: self

        class(abst_matrix), pointer :: K_ptr => null()
        type(type_vector_dp), pointer :: F_ptr => null()
        type(type_vector_dp), pointer :: du_ptr => null()
        real(real64), pointer :: tmp_data_ptr(:) => null()
        type(type_constant_id), pointer :: coupling_mode_ptr => null()
        type(type_system_topology) :: topology
        type(type_constant_id) :: active_physics
        integer(int32) :: sys_id, i, matrix_ierr
        logical :: linear_failed
        character(len=16) :: solve_phase

        ! Symmetric Jacobi equilibration locals (A~ = D A D, b~ = D b, du = D y)
        real(real64), allocatable :: equil_scale(:)
        real(real64), pointer :: dudat(:)

        call self%control%profiler_start(PROFILER_TYPES%SOLVE)

        if (self%control%is_staggered()) then
            if (self%current_physics_id > 0) then
                active_physics = PHYSICS_TYPES%to_object(self%current_physics_id)
            else
                active_physics = PHYSICS_TYPES%HYDRAULIC
                do i = 1, PHYSICS_TYPES%NUM_ID
                    if (self%control%is_physics_active(PHYSICS_TYPES%to_object(i))) then
                        active_physics = PHYSICS_TYPES%to_object(i)
                        exit
                    end if
                end do
            end if
            K_ptr => self%K%get_matrix(active_physics)
            sys_id = active_physics%ID
            F_ptr => self%F%get_vector(sys_id)
            du_ptr => self%du%get_vector(sys_id)
            solve_phase = trim(active_physics%name)
        else
            K_ptr => self%K%get_matrix()
            F_ptr => self%F%get_vector()
            du_ptr => self%du%get_vector()
            sys_id = 1
            solve_phase = 'monolithic'
        end if

        if (.not. (associated(K_ptr) .and. associated(F_ptr) .and. associated(du_ptr))) then
            call self%control%profiler_stop(PROFILER_TYPES%SOLVE)
            return
        end if

        if (associated(du_ptr)) tmp_data_ptr => du_ptr%get_data()
        if (.not. associated(tmp_data_ptr)) then
            call self%control%get_coupling_mode(coupling_mode_ptr)
            call self%domain%export_topology(topology)
            call self%du%initialize(topology, coupling_mode_ptr)
            if (self%control%is_staggered()) then
                du_ptr => self%du%get_vector(sys_id)
            else
                du_ptr => self%du%get_vector()
            end if
        end if

        call du_ptr%zero()

        ! Symmetric Jacobi equilibration of the (ill-conditioned) coupled system.
        ! The T/p column-scale disparity (dH/dT ~ 1e6 vs dH/dp ~ 1e-3, cond ~ 1e13)
        ! otherwise makes even the direct solver return O(0.1) relative residuals.
        ! Solve (D A D)(D^-1 du) = D b; du is unscaled (du = D y) after the solve.
        call jacobi_equilibrate_bsr(K_ptr, F_ptr, equil_scale)
        matrix_ierr = MATRIX_STATUS%SUCCESS%ID
        call K_ptr%commit_to_mkl(matrix_ierr)

        linear_failed = .false.
        if (self%control%is_staggered()) then
            if (active_physics%ID == PHYSICS_TYPES%THERMAL%ID .and. allocated(self%solver_thermal)) then
                call self%solver_thermal%solve(K_ptr, F_ptr, du_ptr)
                call self%solver_thermal%check()
                if (.not. self%solver_thermal%is_success()) linear_failed = .true.
            else
                call self%solver%solve(K_ptr, F_ptr, du_ptr)
                call self%solver%check()
                if (.not. self%solver%is_success()) linear_failed = .true.
            end if
        else
            call self%solver%solve(K_ptr, F_ptr, du_ptr)
            call self%solver%check()
            if (.not. self%solver%is_success()) linear_failed = .true.
        end if

        if (linear_failed) then
            write (*, '(A,A,A)') '   [LINEAR-PHASE] failed phase=', trim(solve_phase), &
                ': solver did not converge'
        end if

        ! Did the solver actually solve the system it was handed? Every other
        ! diagnostic downstream - the line search, the residual gate, the
        ! finite-difference tangent audit - assumes it did, so the assumption is
        ! checked here rather than inferred. Measured on the equilibrated system,
        ! which is the one passed to the solver.
        if (LINEAR_RESIDUAL_CHECK .and. .not. linear_failed) then
            block
                type(type_vector_dp) :: product_work
                real(real64), pointer :: product_data(:), rhs_data(:)
                real(real64) :: norm_defect, norm_rhs
                integer(int32) :: matvec_ierr

                nullify (product_data, rhs_data)
                ! get_size() reports nodes, not degrees of freedom; the coupled
                ! vector carries one entry per (node, physics).
                rhs_data => F_ptr%get_data()
                if (associated(rhs_data)) call product_work%initialize(size(rhs_data))
                matvec_ierr = 0
                call matvec(K_ptr, du_ptr, product_work, matvec_ierr)
                product_data => product_work%get_data()
                if (associated(product_data) .and. associated(rhs_data)) then
                    norm_defect = sqrt(sum((product_data - rhs_data)**2))
                    norm_rhs = sqrt(sum(rhs_data**2))
                    if (norm_rhs > 0.0d0 .and. norm_defect > LINEAR_RESIDUAL_WARN * norm_rhs) then
                        write (*, '(A,ES11.3,A,ES11.3)') '   [LINEAR] inaccurate solve: ||K du - F||/||F|| = ', &
                            norm_defect / norm_rhs, ', ||F|| = ', norm_rhs
                    end if
                end if
                nullify (product_data, rhs_data)
                call product_work%destroy()
            end block
        end if

        ! Recover the pore-pressure increment from the total-potential one:
        ! du_p = du_g + c du_T (see transform_to_total_potential). Done before
        ! the equilibration unscaling would be wrong - the stored increments are
        ! still in the scaled variable there - so it follows it below.

        ! Unscale the equilibrated solution: the solver returned y = D^-1 du, so the
        ! physical increment is du = D y.
        if (allocated(equil_scale)) then
            nullify (dudat)
            dudat => du_ptr%get_data()
            if (associated(dudat)) then
                if (size(dudat) == size(equil_scale)) dudat(:) = dudat(:) * equil_scale(:)
            end if
        end if

        if (allocated(self%cryo_slope) .and. .not. self%control%is_staggered()) then
            block
                real(real64), pointer :: increment(:)
                integer(int32) :: thermal_offset, hydraulic_offset, node, num_nodes_total
                integer(int32) :: num_dofs_per_node

                nullify (increment)
                increment => du_ptr%get_data()
                call self%domain%get_num_nodes(num_nodes_total)
                if (associated(increment) .and. size(self%cryo_slope) == num_nodes_total) then
                    num_dofs_per_node = size(increment) / max(1, num_nodes_total)
                    if (num_dofs_per_node == PHYSICS_TYPES%NUM_ID .or. num_dofs_per_node == 2) then
                        thermal_offset = PHYSICS_TYPES%THERMAL%ID
                        hydraulic_offset = PHYSICS_TYPES%HYDRAULIC%ID
                        do node = 1, num_nodes_total
                            increment((node - 1) * num_dofs_per_node + hydraulic_offset) = &
                                increment((node - 1) * num_dofs_per_node + hydraulic_offset) + &
                                self%cryo_slope(node) * increment((node - 1) * num_dofs_per_node + thermal_offset)
                        end do
                    end if
                end if
                nullify (increment)
            end block
        end if

        call self%control%profiler_stop(PROFILER_TYPES%SOLVE)
    end subroutine solve_ftcms

    !> Symmetric Jacobi (diagonal) equilibration of a BSR linear system in place:
    !> with D_i = 1/sqrt(|A_ii|), form A <- D A D and b <- D b, returning D so the
    !> solution can be unscaled as x = D y. This restores conditioning of the strongly
    !> unit-disparate coupled (T, p_w) system (cond ~ 1e13 -> O(1)) so that even a
    !> direct solver returns an accurate increment. No-op for non-BSR matrices.
    subroutine jacobi_equilibrate_bsr(K, F, D)
        implicit none
        class(abst_matrix), intent(inout) :: K
        class(type_vector_dp), intent(inout) :: F
        real(real64), allocatable, intent(inout) :: D(:)

        integer(int32), pointer :: ptr(:), ind(:)
        real(real64), pointer :: val(:, :, :)
        real(real64), pointer :: fdat(:)
        type(type_matrix_info) :: info
        integer(int32) :: nb, n_brows, n, i, kb, r, c, bc, g

        if (allocated(D)) deallocate (D)

        select type (K)
        type is (type_matrix_bsr)
            ptr => K%get_ptr()
            ind => K%get_ind()
            val => K%get_val()
            call K%get_info(info)
            if (.not. (associated(ptr) .and. associated(ind) .and. associated(val))) return
            nb = info%num_block_rows
            n_brows = size(ptr) - 1
            n = n_brows * nb
            if (n <= 0) return
            allocate (D(n))
            D = 1.0d0

            ! D_i = 1 / sqrt(|A_ii|) from the diagonal blocks
            do i = 1, n_brows
                do kb = ptr(i), ptr(i + 1) - 1
                    if (ind(kb) == i) then
                        do r = 1, nb
                            g = (i - 1) * nb + r
                            if (abs(val(r, r, kb)) > 0.0d0) D(g) = 1.0d0 / sqrt(abs(val(r, r, kb)))
                        end do
                    end if
                end do
            end do

            ! A <- D A D (scale every stored block entry)
            do i = 1, n_brows
                do kb = ptr(i), ptr(i + 1) - 1
                    bc = ind(kb)
                    do c = 1, nb
                        do r = 1, nb
                            val(r, c, kb) = val(r, c, kb) * D((i - 1) * nb + r) * D((bc - 1) * nb + c)
                        end do
                    end do
                end do
            end do

            ! b <- D b
            nullify (fdat)
            fdat => F%get_data()
            if (associated(fdat)) then
                if (size(fdat) == n) fdat(:) = fdat(:) * D(:)
            end if
        end select
    end subroutine jacobi_equilibrate_bsr

    !> Implementation strategy: replicate exactly the geometry evaluations
    !> the direct projection performed per call -- same element order, same
    !> Gauss-point order, same calc_shape_function invocations, and the same
    !> shape_weight = psi_k * (w_p * det J_p) product and nodal-volume
    !> accumulation order -- and store the results. Because the cached
    !> values are produced by the identical instruction sequence, a
    !> cache-based projection reproduces the direct projection bit for bit.
    !> Memory: entry_ptr/num_gauss/num_nodes_elem O(E) int32,
    !> shape_weight O(E_gp) and dpsi_dx O(dim * E_gp) real64,
    !> nodal_vol O(N_nd) real64.
    module subroutine initialize_gradient_geometry_cache(self, domain)
        implicit none
        class(type_gradient_geometry_cache), intent(inout) :: self
        type(type_domain), intent(in) :: domain

        class(abst_fe), pointer :: fe
        integer(int32), dimension(:), pointer, contiguous :: p_conn
        real(real64), allocatable :: node_coords(:, :)
        real(real64), allocatable :: psi(:)
        real(real64), allocatable :: dpsi_dx(:, :)
        real(real64), pointer, contiguous, dimension(:) :: fe_weights
        type(type_coordinate_dp), pointer, contiguous, dimension(:) :: fe_gauss_pts
        real(real64) :: det_j, w_vol, shape_weight
        type(type_coordinate_dp) :: r
        integer(int32) :: num_elements, num_total_nodes, dim
        integer(int32) :: n_nodes_elem, n_gauss
        integer(int32) :: i, p, k, entry, total_entries

        call self%destroy()
        nullify (fe, p_conn, fe_weights, fe_gauss_pts)

        call domain%get_num_fe(num_elements)
        call domain%get_num_nodes(num_total_nodes)
        call domain%get_computation_dimension(dim)
        if (num_elements <= 0 .or. num_total_nodes <= 0 .or. dim <= 0) return

        self%num_elements = num_elements
        self%num_nodes = num_total_nodes
        self%dim = dim

        allocate (self%entry_ptr(num_elements + 1))
        allocate (self%num_gauss(num_elements))
        allocate (self%num_nodes_elem(num_elements))

        ! Pass 1: per-element sizes and entry offsets
        self%entry_ptr(1) = 1
        do i = 1, num_elements
            call domain%get_fe(i, fe)
            call fe%get_num_nodes(n_nodes_elem)
            call fe%get_num_gauss(n_gauss)
            self%num_nodes_elem(i) = n_nodes_elem
            self%num_gauss(i) = n_gauss
            self%entry_ptr(i + 1) = self%entry_ptr(i) + n_gauss * n_nodes_elem
        end do
        total_entries = self%entry_ptr(num_elements + 1) - 1

        allocate (self%shape_weight(max(total_entries, 1)))
        allocate (self%dpsi_dx(dim, max(total_entries, 1)))
        allocate (self%nodal_vol(num_total_nodes))
        self%nodal_vol(:) = 0.0d0

        allocate (psi(20), dpsi_dx(dim, 20))

        ! Pass 2: evaluate and store the Gauss-point geometry, accumulating
        ! the lumped nodal volume in the projection's original order.
        do i = 1, num_elements
            call domain%get_fe(i, fe)
            call domain%get_fe_connectivity(i, p_conn)
            n_nodes_elem = self%num_nodes_elem(i)
            n_gauss = self%num_gauss(i)
            call fe%get_weight(fe_weights)
            call fe%get_gauss(fe_gauss_pts)
            call domain%get_fe_coordinate(i, node_coords)
            entry = self%entry_ptr(i)
            do p = 1, n_gauss
                r = fe_gauss_pts(p)
                call fe%calc_shape_function(r, node_coords, psi=psi(1:n_nodes_elem), &
                                            dpsi_dx=dpsi_dx(:, 1:n_nodes_elem), determinant_jacobian=det_j)
                w_vol = fe_weights(p) * det_j
                do k = 1, n_nodes_elem
                    shape_weight = psi(k) * w_vol
                    self%shape_weight(entry) = shape_weight
                    self%dpsi_dx(:, entry) = dpsi_dx(:, k)
                    self%nodal_vol(p_conn(k)) = self%nodal_vol(p_conn(k)) + shape_weight
                    entry = entry + 1
                end do
            end do
            nullify (p_conn, fe_weights, fe_gauss_pts, fe)
        end do

    end subroutine initialize_gradient_geometry_cache

    module subroutine destroy_gradient_geometry_cache(self)
        implicit none
        class(type_gradient_geometry_cache), intent(inout) :: self

        if (allocated(self%entry_ptr)) deallocate (self%entry_ptr)
        if (allocated(self%num_gauss)) deallocate (self%num_gauss)
        if (allocated(self%num_nodes_elem)) deallocate (self%num_nodes_elem)
        if (allocated(self%shape_weight)) deallocate (self%shape_weight)
        if (allocated(self%dpsi_dx)) deallocate (self%dpsi_dx)
        if (allocated(self%nodal_vol)) deallocate (self%nodal_vol)
        self%num_elements = 0
        self%num_nodes = 0
        self%dim = 0
    end subroutine destroy_gradient_geometry_cache

    !> Calculate nodal gradient of a scalar field
    !>
    !> Implementation: cache-based L2 (lumped mass) projection as an
    !> OpenMP-parallel node-centric gather. For each node, contributions are
    !> gathered from its adjacent elements' Gauss points using the static
    !> geometry cache (self%gradient_cache, built once at initialization);
    !> only the field-dependent gather and accumulation run per call.
    !>
    !> Bit-identity with the serial element-scatter version: in the scatter,
    !> each grad component of node j accumulates its terms in the order
    !> "adjacent elements ascending, Gauss points ascending" (elements are
    !> processed in ascending index order and each element contributes to
    !> node j exactly once per Gauss point). The node->element adjacency
    !> lists elements in ascending order, so the gather reproduces exactly
    !> the same addition sequence per node; the per-Gauss-point gradient is
    !> recomputed by the identical instruction sequence and the final
    !> division uses the same cached lumped volume. Each iteration writes
    !> only its own node entry, so the result is schedule-independent and
    !> bit-identical to the serial projection.
    module subroutine calc_gradient_ftcms(self, values_vec, grad)
        implicit none
        class(type_ftcms), intent(inout) :: self
        real(real64), intent(in) :: values_vec(:)
        type(type_coordinate_array_dp), intent(inout) :: grad

        integer(int32), dimension(:), pointer, contiguous :: p_conn
        integer(int32), dimension(:), pointer, contiguous :: element_list
        real(real64) :: elem_u(20)
        real(real64) :: gauss_grad(3)
        real(real64) :: acc(3)
        real(real64) :: shape_weight
        integer(int32) :: num_elements, num_total_nodes, dim
        integer(int32) :: n_nodes_elem, n_gauss
        integer(int32) :: i_node, j, i, p, k, k_local, entry, entry0
        integer(int32) :: d, computation_type_id
        real(real64) :: grad_component
        logical :: has_x, has_y, has_z
        logical :: second_component_is_z
        type(type_constant_id), pointer :: computation_type => null()

        num_elements = self%gradient_cache%num_elements
        num_total_nodes = self%gradient_cache%num_nodes
        dim = self%gradient_cache%dim
        call grad%zero()
        if (num_elements <= 0) return

        call self%domain%get_computation_type(computation_type)
        computation_type_id = computation_type%ID
        has_x = allocated(grad%x)
        second_component_is_z = computation_type_id == COMP_TYPES%XZ_2D%ID
        has_y = dim >= 2 .and. .not. second_component_is_z .and. allocated(grad%y)
        has_z = dim >= 3 .and. allocated(grad%z)
        if (second_component_is_z) has_z = allocated(grad%z)

        !$OMP PARALLEL DEFAULT(NONE) &
        !$OMP SHARED(self, values_vec, grad, num_total_nodes, dim, has_x, has_y, has_z, second_component_is_z) &
        !$OMP PRIVATE(i_node, j, i, p, k, k_local, d, entry, entry0, &
        !$OMP         p_conn, element_list, elem_u, gauss_grad, acc, &
        !$OMP         shape_weight, grad_component, n_nodes_elem, n_gauss)
        nullify (p_conn, element_list)
        !$OMP DO
        do i_node = 1, num_total_nodes
            call self%domain%element_adjacency%get_list(i_node, element_list)
            if (.not. associated(element_list)) cycle ! isolated node: keep zero gradient

            acc = 0.0d0
            do j = 1, size(element_list)
                i = element_list(j)
                call self%domain%get_fe_connectivity(i, p_conn)
                n_nodes_elem = self%gradient_cache%num_nodes_elem(i)
                n_gauss = self%gradient_cache%num_gauss(i)
                elem_u(1:n_nodes_elem) = values_vec(p_conn(1:n_nodes_elem))
                ! Local index of this node within the element connectivity
                k_local = 0
                do k = 1, n_nodes_elem
                    if (p_conn(k) == i_node) then
                        k_local = k
                        exit
                    end if
                end do
                nullify (p_conn)
                if (k_local == 0) cycle ! adjacency/connectivity mismatch: no contribution

                entry0 = self%gradient_cache%entry_ptr(i)
                do p = 1, n_gauss
                    entry = entry0 + (p - 1) * n_nodes_elem
                    gauss_grad = 0.0d0
                    do d = 1, dim
                        grad_component = 0.0d0
                        do k = 1, n_nodes_elem
                            grad_component = grad_component + elem_u(k) * self%gradient_cache%dpsi_dx(d, entry + k - 1)
                        end do
                        gauss_grad(d) = grad_component
                    end do
                    shape_weight = self%gradient_cache%shape_weight(entry + k_local - 1)
                    if (has_x) acc(1) = acc(1) + shape_weight * gauss_grad(1)
                    if (has_y) acc(2) = acc(2) + shape_weight * gauss_grad(2)
                    if (has_z) then
                        if (second_component_is_z) then
                            acc(3) = acc(3) + shape_weight * gauss_grad(2)
                        else
                            acc(3) = acc(3) + shape_weight * gauss_grad(3)
                        end if
                    end if
                end do
            end do
            nullify (element_list)

            if (self%gradient_cache%nodal_vol(i_node) > epsilon(1.0d0)) then
                if (has_x) grad%x(i_node) = acc(1) / self%gradient_cache%nodal_vol(i_node)
                if (has_y) grad%y(i_node) = acc(2) / self%gradient_cache%nodal_vol(i_node)
                if (has_z) grad%z(i_node) = acc(3) / self%gradient_cache%nodal_vol(i_node)
            else
                if (has_x) grad%x(i_node) = acc(1)
                if (has_y) grad%y(i_node) = acc(2)
                if (has_z) grad%z(i_node) = acc(3)
            end if
        end do
        !$OMP END DO
        !$OMP END PARALLEL
    end subroutine calc_gradient_ftcms

    !> Calculate temperature gradient
    module subroutine calc_gradient_temperature_ftcms(self)
        implicit none
        class(type_ftcms), intent(inout) :: self
        real(real64), pointer, contiguous, dimension(:) :: temperature => null()
        type(type_coordinate_array_dp), pointer :: grad_T => null()
        if (.not. self%control%is_physics_active(PHYSICS_TYPES%THERMAL)) return
        call self%temperature%get_current(temperature)
        call self%temperature%get_current_gradient(grad_T)
        if (associated(grad_T)) call self%calc_gradient(temperature, grad_T)
    end subroutine calc_gradient_temperature_ftcms

    !> Calculate pressure gradient
    module subroutine calc_gradient_pressure_ftcms(self)
        implicit none
        class(type_ftcms), intent(inout) :: self
        real(real64), pointer, contiguous, dimension(:) :: pressure => null()
        type(type_coordinate_array_dp), pointer :: grad_P => null()
        if (.not. self%control%is_physics_active(PHYSICS_TYPES%HYDRAULIC)) return
        call self%pressure%get_current(pressure)
        call self%pressure%get_current_gradient(grad_P)
        if (associated(grad_P)) call self%calc_gradient(pressure, grad_P)
    end subroutine calc_gradient_pressure_ftcms

    !> Calculate liquid water flux vector
    module subroutine calc_water_flux_ftcms(self, material_id, state, grad_T, grad_P, water_flux)
        implicit none
        class(type_ftcms), intent(inout) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        type(type_coordinate_dp), intent(in) :: grad_T, grad_P
        type(type_coordinate_dp), intent(inout) :: water_flux
        type(type_constant_id), pointer :: computation_type => null()
        real(real64) :: K_wT, K_wP_raw, K_wP, rho_w, gravity_term
        call self%domain%get_computation_type(computation_type)
        call self%hydraulic%calc_K_wT(material_id, state, K_wT)
        call self%hydraulic%calc_K_wP(material_id, state, K_wP_raw)
        call self%thermal%calc_density_water(state, rho_w)
        K_wP = merge(K_wP_raw / (rho_w * g), 0.0d0, rho_w > tiny(1.0d0))
        gravity_term = K_wP_raw
        select case (computation_type%ID)
        case (COMP_TYPES%XY_2D%ID)
            water_flux%x = -K_wT * grad_T%x - K_wP * grad_P%x
            water_flux%y = -K_wT * grad_T%y - K_wP * grad_P%y
            water_flux%z = 0.0d0
        case (COMP_TYPES%XZ_2D%ID)
            water_flux%x = -K_wT * grad_T%x - K_wP * grad_P%x
            water_flux%y = 0.0d0
            water_flux%z = -K_wT * grad_T%z - K_wP * grad_P%z - gravity_term
        case (COMP_TYPES%XYZ_3D%ID)
            water_flux%x = -K_wT * grad_T%x - K_wP * grad_P%x
            water_flux%y = -K_wT * grad_T%y - K_wP * grad_P%y
            water_flux%z = -K_wT * grad_T%z - K_wP * grad_P%z - gravity_term
        end select
    end subroutine calc_water_flux_ftcms

    !> Calculate water vapor flux vector
    module subroutine calc_vapor_flux_ftcms(self, material_id, state, grad_T, grad_P, water_flux)
        implicit none
        class(type_ftcms), intent(inout) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        type(type_coordinate_dp), intent(in) :: grad_T, grad_P
        type(type_coordinate_dp), intent(inout) :: water_flux
        type(type_constant_id), pointer :: computation_type => null()
        real(real64) :: K_vT, K_vP_raw, K_vP, rho_w
        call self%domain%get_computation_type(computation_type)
        if (.not. self%hydraulic%is_vapor_transport_enabled()) then
            call water_flux%set(0.0d0, 0.0d0, 0.0d0)
            return
        end if
        call self%hydraulic%calc_K_vT(material_id, state, K_vT)
        call self%hydraulic%calc_K_vP(material_id, state, K_vP_raw)
        call self%thermal%calc_density_water(state, rho_w)
        K_vP = merge(K_vP_raw / (rho_w * g), 0.0d0, rho_w > tiny(1.0d0))
        select case (computation_type%ID)
        case (COMP_TYPES%XY_2D%ID)
            water_flux%x = -K_vT * grad_T%x - K_vP * grad_P%x
            water_flux%y = -K_vT * grad_T%y - K_vP * grad_P%y
            water_flux%z = 0.0d0
        case (COMP_TYPES%XZ_2D%ID)
            water_flux%x = -K_vT * grad_T%x - K_vP * grad_P%x
            water_flux%y = 0.0d0
            water_flux%z = -K_vT * grad_T%z - K_vP * grad_P%z
        case (COMP_TYPES%XYZ_3D%ID)
            water_flux%x = -K_vT * grad_T%x - K_vP * grad_P%x
            water_flux%y = -K_vT * grad_T%y - K_vP * grad_P%y
            water_flux%z = -K_vT * grad_T%z - K_vP * grad_P%z
        end select
    end subroutine calc_vapor_flux_ftcms

end submodule ftcms_compute
