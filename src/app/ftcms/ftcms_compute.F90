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
        type(type_constant_id) :: active_physics
        integer(int32) :: sys_id, i
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

        block
            real(real64), pointer :: tmp_data_ptr(:) => null()
            if (associated(du_ptr)) tmp_data_ptr => du_ptr%get_data()
            if (.not. associated(tmp_data_ptr)) then
                block
                    type(type_constant_id), pointer :: coupling_mode_ptr
                    nullify (coupling_mode_ptr)
                    call self%control%get_coupling_mode(coupling_mode_ptr)
                    block
                        type(type_system_topology) :: topology
                        call self%domain%export_topology(topology)
                        call self%du%initialize(topology, coupling_mode_ptr)
                    end block
                end block
                if (self%control%is_staggered()) then
                    du_ptr => self%du%get_vector(sys_id)
                else
                    du_ptr => self%du%get_vector()
                end if
            end if
        end block

        call du_ptr%zero()

        ! Symmetric Jacobi equilibration of the (ill-conditioned) coupled system.
        ! The T/p column-scale disparity (dH/dT ~ 1e6 vs dH/dp ~ 1e-3, cond ~ 1e13)
        ! otherwise makes even the direct solver return O(0.1) relative residuals.
        ! Solve (D A D)(D^-1 du) = D b; du is unscaled (du = D y) after the solve.
        call jacobi_equilibrate_bsr(K_ptr, F_ptr, equil_scale)

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

        ! Unscale the equilibrated solution: the solver returned y = D^-1 du, so the
        ! physical increment is du = D y.
        if (allocated(equil_scale)) then
            nullify (dudat)
            dudat => du_ptr%get_data()
            if (associated(dudat)) then
                if (size(dudat) == size(equil_scale)) dudat(:) = dudat(:) * equil_scale(:)
            end if
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

    !> Calculate nodal gradient of a scalar field
    module subroutine calc_gradient_ftcms(self, values_vec, grad)
        implicit none
        class(type_ftcms), intent(inout) :: self
        real(real64), intent(in) :: values_vec(:)
        type(type_coordinate_array_dp), intent(inout) :: grad

        class(abst_fe), pointer :: fe
        integer(int32), dimension(:), pointer, contiguous :: p_conn
        real(real64), allocatable :: elem_u(:)
        real(real64), allocatable :: node_coords(:, :)
        real(real64), allocatable :: psi(:)
        real(real64), allocatable :: dpsi_dx(:, :)
        real(real64), pointer, contiguous, dimension(:) :: fe_weights
        type(type_coordinate_dp), pointer, contiguous, dimension(:) :: fe_gauss_pts
        real(real64), allocatable :: nodal_vol(:)
        real(real64) :: det_j
        real(real64) :: gauss_grad(3)
        real(real64) :: w_vol, shape_weight
        type(type_coordinate_dp) :: r
        integer(int32) :: num_elements, num_total_nodes, dim
        integer(int32) :: n_nodes_elem, n_gauss
        integer(int32) :: i, p, k, d, global_nid
        real(real64) :: grad_component

        nullify (fe, p_conn, fe_weights, fe_gauss_pts)
        call self%domain%get_num_fe(num_elements)
        call self%domain%get_num_nodes(num_total_nodes)
        call self%domain%get_computation_dimension(dim)
        call grad%zero()

        if (allocated(nodal_vol)) deallocate (nodal_vol)
        allocate (nodal_vol(num_total_nodes))
        nodal_vol(:) = 0.0d0

        allocate (elem_u(20), psi(20), dpsi_dx(dim, 20))

        do i = 1, num_elements
            call self%domain%get_fe(i, fe)
            call self%domain%get_fe_connectivity(i, p_conn)
            call fe%get_num_nodes(n_nodes_elem)
            call fe%get_num_gauss(n_gauss)
            call fe%get_weight(fe_weights)
            call fe%get_gauss(fe_gauss_pts)
            elem_u(1:n_nodes_elem) = values_vec(p_conn(1:n_nodes_elem))
            call self%domain%get_fe_coordinate(i, node_coords)
            do p = 1, n_gauss
                r = fe_gauss_pts(p)
                call fe%calc_shape_function(r, node_coords, psi=psi(1:n_nodes_elem), &
                                            dpsi_dx=dpsi_dx(:, 1:n_nodes_elem), determinant_jacobian=det_j)
                w_vol = fe_weights(p) * det_j
                gauss_grad = 0.0d0
                do d = 1, dim
                    grad_component = 0.0d0
                    do k = 1, n_nodes_elem
                        grad_component = grad_component + elem_u(k) * dpsi_dx(d, k)
                    end do
                    gauss_grad(d) = grad_component
                end do
                do k = 1, n_nodes_elem
                    global_nid = p_conn(k)
                    shape_weight = psi(k) * w_vol
                    nodal_vol(global_nid) = nodal_vol(global_nid) + shape_weight
                    if (allocated(grad%x)) grad%x(global_nid) = grad%x(global_nid) + shape_weight * gauss_grad(1)
                    if (dim >= 2 .and. allocated(grad%y)) grad%y(global_nid) = grad%y(global_nid) + shape_weight * gauss_grad(2)
                    if (dim >= 3 .and. allocated(grad%z)) grad%z(global_nid) = grad%z(global_nid) + shape_weight * gauss_grad(3)
                end do
            end do
        end do
        do k = 1, num_total_nodes
            if (nodal_vol(k) > epsilon(1.0d0)) then
                if (allocated(grad%x)) grad%x(k) = grad%x(k) / nodal_vol(k)
                if (allocated(grad%y)) grad%y(k) = grad%y(k) / nodal_vol(k)
                if (allocated(grad%z)) grad%z(k) = grad%z(k) / nodal_vol(k)
            end if
        end do
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
