!> Implementation overview
!>
!> Algorithm:
!> - Volume-weighted smoothing of element-wise state variables to nodes
!> - Solving the global linear system
!> - L2 projection (lumped mass) for nodal gradient calculations
!> - Evaluation of water and vapor fluxes based on the Darcy law
submodule(app_ftcms) ftcms_compute
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    implicit none
contains

    !> Smooth element-wise state variables to nodal values
    !>
    !> Mathematical definition:
    !> - Computes volume-weighted average of state variables at each node
    !>
    !> Assumptions:
    !> - Element adjacency lists and volumes are precomputed
    !>
    !> Numerical guarantee:
    !> - Preserves total volume conceptually via weighted sum
    !>
    !> Computational complexity:
    !> - Memory: \(O(N_\mathrm{threads})\)
    !> - Arithmetic: \(O(N_\mathrm{nodes} \times N_\mathrm{neighbors})\)
    !>
    !> Failure behavior:
    !> - Assigns zero if isolated node is encountered
    module subroutine update_variables_ftcms(self)
        implicit none
        !> Main application object
        class(type_ftcms), intent(inout) :: self

        integer(int32) :: i_node, i_elem, j
        integer(int32) :: num_nodes, num_neighbors, material_id
        integer(int32), pointer, contiguous :: element_list(:)

        ! Thread-local state variables for parallelization
        ! Managed as an array to avoid pointer initialization issues with SAVE attribute in PRIVATE variables
        type(type_state), allocatable :: states(:)

        ! Temporary storage for elemental values
        real(real64) :: elem_qw, elem_qi, elem_qi_seg, elem_qa, elem_qv
        real(real64) :: elem_vol

        ! Accumulators for volume-weighted sum
        real(real64) :: sum_vol
        real(real64) :: sum_qw_vol, sum_qi_vol, sum_qi_seg_vol, sum_qa_vol, sum_qv_vol

        ! OpenMP variables
        integer(int32) :: num_threads, tid

        call self%control%profiler_start(PROFILER_TYPES%SETUP)

        call self%domain%get_num_nodes(num_nodes)

        ! Allocate work arrays for the maximum number of threads
        num_threads = omp_get_max_threads()
        allocate (states(num_threads))

        ! ----------------------------------------------------------------------
        ! Nodal loop (Parallelized)
        ! ----------------------------------------------------------------------
        !$OMP PARALLEL DEFAULT(NONE) &
        !$OMP SHARED(self, num_nodes, states) &
        !$OMP PRIVATE(i_node, element_list, num_neighbors, j, i_elem, &
        !$OMP         elem_vol, material_id, elem_qw, elem_qi, elem_qi_seg, elem_qa, elem_qv, &
        !$OMP         sum_vol, sum_qw_vol, sum_qi_vol, sum_qi_seg_vol, sum_qa_vol, sum_qv_vol, &
        !$OMP         tid)

        ! Get thread ID (1-based)
        tid = omp_get_thread_num() + 1

        ! Initialize pointer variable for safety
        nullify (element_list)

        !$OMP DO
        do i_node = 1, num_nodes

            ! 1. Retrieve adjacent element list
            call self%domain%element_adjacency%get_list(i_node, element_list)

            ! Initialize accumulators
            sum_vol = 0.0d0
            sum_qw_vol = 0.0d0
            sum_qi_vol = 0.0d0
            sum_qi_seg_vol = 0.0d0
            sum_qa_vol = 0.0d0
            sum_qv_vol = 0.0d0

            if (associated(element_list)) then
                num_neighbors = size(element_list)

                ! 2. Loop over adjacent elements
                do j = 1, num_neighbors
                    i_elem = element_list(j)

                    ! Get element volume for weighting
                    call self%domain%calc_measure(i_elem, elem_vol)

                    ! Update and retrieve state variables using thread-specific state
                    call self%domain%get_material_id(i_elem, material_id)
                    call self%set_state(i_node, i_elem, states(tid))

                    ! Retrieve volumetric contents for each phase from the state
                    call states(tid)%get(water_content=elem_qw, ice_content=elem_qi, &
                                         air_content=elem_qa, vapor_content=elem_qv, &
                                         ice_content_seg=elem_qi_seg)

                    ! Weighted addition
                    sum_vol = sum_vol + elem_vol
                    sum_qw_vol = sum_qw_vol + (elem_qw * elem_vol)
                    sum_qi_vol = sum_qi_vol + (elem_qi * elem_vol)
                    sum_qi_seg_vol = sum_qi_seg_vol + (elem_qi_seg * elem_vol)
                    sum_qa_vol = sum_qa_vol + (elem_qa * elem_vol)
                    sum_qv_vol = sum_qv_vol + (elem_qv * elem_vol)
                end do
            end if

            ! 3. Normalize and store at node (assumes setter is thread-safe for different i_node)
            if (abs(sum_vol) > epsilon(1.0d0)) then
                call self%Qw%set_current(i_node, sum_qw_vol / sum_vol)
                call self%Qi%set_current(i_node, sum_qi_vol / sum_vol)
                call self%Qi_seg%set_current(i_node, sum_qi_seg_vol / sum_vol)
                call self%Qa%set_current(i_node, sum_qa_vol / sum_vol)
                call self%Qv%set_current(i_node, sum_qv_vol / sum_vol)
            else
                ! Handle isolated nodes
                call self%Qw%set_current(i_node, 0.0d0)
                call self%Qi%set_current(i_node, 0.0d0)
                call self%Qi_seg%set_current(i_node, 0.0d0)
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
    !>
    !> Mathematical definition:
    !> - Solves \( \mathbf{K} \Delta \mathbf{u} = \mathbf{F} \)
    !>
    !> Assumptions:
    !> - Matrix and vectors are properly assembled
    !>
    !> Numerical guarantee:
    !> - No theoretical error bound available
    !>
    !> Computational complexity:
    !> - Depends on the chosen solver
    !>
    !> Failure behavior:
    !> - Halts execution if solver fails
    module subroutine solve_ftcms(self, frozen_physics)
        implicit none
        !> Main application object
        class(type_ftcms), intent(inout) :: self
        type(type_constant_id), intent(in), optional :: frozen_physics

        class(abst_matrix), pointer :: K_ptr => null()
        type(type_vector_dp), pointer :: F_ptr => null()
        type(type_vector_dp), pointer :: du_ptr => null()
        type(type_vector_dp) :: lin_res_vec
        integer(int32) :: num_total_dofs
        character(len=16) :: solve_phase

        call self%control%profiler_start(PROFILER_TYPES%SOLVE)

        block
            type(type_constant_id) :: active_physics
            integer(int32) :: sys_id

            if (self%control%is_staggered()) then
                ! In staggered mode, frozen_physics names the physics that is held
                ! fixed; the active solve targets the OTHER physics.
                if (present(frozen_physics) .and. frozen_physics%ID == PHYSICS_TYPES%HYDRAULIC%ID) then
                    active_physics = PHYSICS_TYPES%THERMAL
                else
                    active_physics = PHYSICS_TYPES%HYDRAULIC
                end if
                K_ptr => self%K%get_matrix(active_physics)
                sys_id = active_physics%ID
                F_ptr => self%F%get_vector(sys_id)
                du_ptr => self%du%get_vector(sys_id)
            else
                K_ptr => self%K%get_matrix()
                F_ptr => self%F%get_vector()
                du_ptr => self%du%get_vector()
            end if

            if (.not. associated(du_ptr%get_data())) then
                block
                    type(type_constant_id), pointer :: coupling_mode_ptr
                    nullify (coupling_mode_ptr)
                    call self%control%get_coupling_mode(coupling_mode_ptr)
                    call self%du%initialize(self%domain, coupling_mode_ptr)
                end block
                if (self%control%is_staggered()) then
                    du_ptr => self%du%get_vector(sys_id)
                else
                    du_ptr => self%du%get_vector()
                end if
            end if
        end block

        call self%domain%get_total_dofs(num_total_dofs)

        solve_phase = 'none'
        if (present(frozen_physics)) solve_phase = trim(frozen_physics%name)

        call du_ptr%zero()
        if (present(frozen_physics) .and. frozen_physics%ID == PHYSICS_TYPES%HYDRAULIC%ID &
            .and. allocated(self%solver_thermal)) then
            block
                real(real64), pointer :: dbg_F(:) => null()
                dbg_F => F_ptr%get_data()
                if (associated(dbg_F)) then
                    write (*, '(A,ES12.4,A,ES12.4)') '[DBG-F_T] max=', maxval(abs(dbg_F)), ', norm2=', norm2(dbg_F)
                    nullify (dbg_F)
                end if
            end block
            call self%solver_thermal%solve(K_ptr, F_ptr, du_ptr)
            call self%solver_thermal%check()
            if (.not. self%solver_thermal%is_success()) then
                write (*, '(A,A,A)') '   [LINEAR-PHASE] failed frozen=', trim(solve_phase), &
                    ': thermal solver did not converge'
                call self%control%profiler_stop(PROFILER_TYPES%SOLVE)
                return
            end if
        else
            call self%solver%solve(K_ptr, F_ptr, du_ptr)
            call self%solver%check()
            if (.not. self%solver%is_success()) then
                write (*, '(A,A,A)') '   [LINEAR-PHASE] failed frozen=', trim(solve_phase), &
                    ': hydraulic solver did not converge'
                call self%control%profiler_stop(PROFILER_TYPES%SOLVE)
                return
            end if
        end if

        call self%control%profiler_stop(PROFILER_TYPES%SOLVE)

    end subroutine solve_ftcms

    !> Calculate nodal gradient of a scalar field
    !>
    !> Mathematical definition:
    !> - Uses L2 projection with a lumped mass approach
    !>
    !> Assumptions:
    !> - Nodal values are provided for the entire domain
    !>
    !> Numerical guarantee:
    !> - No theoretical error bound available
    !>
    !> Computational complexity:
    !> - Memory: \(O(N_{nodes})\)
    !> - Arithmetic: \(O(N_{elems} \times N_{gauss})\)
    !>
    !> Failure behavior:
    !> - Returns without error
    module subroutine calc_gradient_ftcms(self, values_vec, grad)
        implicit none
        !> Main application object
        class(type_ftcms), intent(inout) :: self
        !> Scalar field nodal values
        !> Not modified
        real(real64), intent(in) :: values_vec(:)
        !> Calculated nodal gradient vector
        !> Overwritten on exit
        type(type_coordinate_array_dp), intent(inout) :: grad

        class(abst_fe), pointer :: fe
        integer(int32), dimension(:), pointer, contiguous :: p_conn

        ! Element data arrays
        real(real64), allocatable :: elem_u(:)
        real(real64), allocatable :: node_coords(:, :)
        real(real64), allocatable :: psi(:)
        real(real64), allocatable :: dpsi_dx(:, :)

        ! FE information cache
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

        ! Initialize all pointers to null
        nullify (fe)
        nullify (p_conn)
        nullify (fe_weights)
        nullify (fe_gauss_pts)

        call self%domain%get_num_fe(num_elements)
        call self%domain%get_num_nodes(num_total_nodes)
        call self%domain%get_computation_dimension(dim)

        call grad%zero()

        if (allocated(nodal_vol)) deallocate (nodal_vol)
        allocate (nodal_vol(num_total_nodes))
        nodal_vol(:) = 0.0d0

        ! Reallocate working arrays explicitly
        if (allocated(elem_u)) deallocate (elem_u)
        if (allocated(psi)) deallocate (psi)
        if (allocated(dpsi_dx)) deallocate (dpsi_dx)

        allocate (elem_u(20))
        allocate (psi(20))
        allocate (dpsi_dx(dim, 20))

        do i = 1, num_elements
            call self%domain%get_fe(i, fe)
            call self%domain%get_fe_connectivity(i, p_conn)

            call fe%get_num_nodes(n_nodes_elem)
            call fe%get_num_gauss(n_gauss)

            call fe%get_weight(fe_weights)
            call fe%get_gauss(fe_gauss_pts)

            elem_u(1:n_nodes_elem) = values_vec(p_conn(1:n_nodes_elem))

            ! Retrieve coordinates (allocatable argument)
            call self%domain%get_fe_coordinate(i, node_coords)

            do p = 1, n_gauss
                r = fe_gauss_pts(p)

                call fe%calc_shape_function(r, node_coords, psi=psi(1:n_nodes_elem), &
                                            dpsi_dx=dpsi_dx(:, 1:n_nodes_elem), determinant_jacobian=det_j)
                w_vol = fe_weights(p) * det_j

                gauss_grad = 0.0d0
                do d = 1, dim
                    ! Explicit dot product to avoid non-contiguous array slice temporaries
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
                    if (dim >= 2) then
                        if (allocated(grad%y)) grad%y(global_nid) = grad%y(global_nid) + shape_weight * gauss_grad(2)
                    end if
                    if (dim >= 3) then
                        if (allocated(grad%z)) grad%z(global_nid) = grad%z(global_nid) + shape_weight * gauss_grad(3)
                    end if
                end do
            end do
        end do

        do k = 1, num_total_nodes
            if (nodal_vol(k) > epsilon(1.0d0)) then
                if (allocated(grad%x)) grad%x(k) = grad%x(k) / nodal_vol(k)
                if (allocated(grad%y)) grad%y(k) = grad%y(k) / nodal_vol(k)
                if (allocated(grad%z)) grad%z(k) = grad%z(k) / nodal_vol(k)
            else
                if (allocated(grad%x)) grad%x(k) = 0.0d0
                if (allocated(grad%y)) grad%y(k) = 0.0d0
                if (allocated(grad%z)) grad%z(k) = 0.0d0
            end if
        end do

        if (allocated(elem_u)) deallocate (elem_u)
        if (allocated(node_coords)) deallocate (node_coords)
        if (allocated(psi)) deallocate (psi)
        if (allocated(dpsi_dx)) deallocate (dpsi_dx)
        if (allocated(nodal_vol)) deallocate (nodal_vol)

    end subroutine calc_gradient_ftcms

    !> Calculate temperature gradient
    !>
    !> Mathematical definition:
    !> - Calls the generalized gradient calculation for temperature
    !>
    !> Assumptions:
    !> - Thermal physics is active
    !>
    !> Numerical guarantee:
    !> - No theoretical error bound available
    !>
    !> Computational complexity:
    !> - Dominates by the general gradient calculation
    !>
    !> Failure behavior:
    !> - Returns silently if thermal physics is inactive
    module subroutine calc_gradient_temperature_ftcms(self)
        implicit none
        !> Main application object
        class(type_ftcms), intent(inout) :: self

        real(real64), pointer, contiguous, dimension(:) :: temperature => null()
        type(type_coordinate_array_dp), pointer :: grad_T

        nullify (grad_T)

        if (.not. self%control%is_physics_active(PHYSICS_TYPES%THERMAL)) return

        call self%temperature%get_current(temperature)
        call self%temperature%get_current_gradient(grad_T)
        if (associated(grad_T)) then
            call self%calc_gradient(temperature, grad_T)
            nullify (grad_T)
        end if
        nullify (temperature)

    end subroutine calc_gradient_temperature_ftcms

    !> Calculate pressure gradient
    !>
    !> Mathematical definition:
    !> - Calls the generalized gradient calculation for pressure
    !>
    !> Assumptions:
    !> - Hydraulic physics is active
    !>
    !> Numerical guarantee:
    !> - No theoretical error bound available
    !>
    !> Computational complexity:
    !> - Dominates by the general gradient calculation
    !>
    !> Failure behavior:
    !> - Returns silently if hydraulic physics is inactive
    module subroutine calc_gradient_pressure_ftcms(self)
        implicit none
        !> Main application object
        class(type_ftcms), intent(inout) :: self

        real(real64), pointer, contiguous, dimension(:) :: pressure => null()
        type(type_coordinate_array_dp), pointer :: grad_P

        nullify (grad_P)

        if (.not. self%control%is_physics_active(PHYSICS_TYPES%HYDRAULIC)) return

        call self%pressure%get_current(pressure)
        call self%pressure%get_current_gradient(grad_P)
        if (associated(grad_P)) then
            call self%calc_gradient(pressure, grad_P)
            nullify (grad_P)
        end if

        nullify (pressure)

    end subroutine calc_gradient_pressure_ftcms

    !> Calculate liquid water flux vector
    !>
    !> Mathematical definition:
    !> - \( \mathbf{q}_w = -K_\mathrm{wT} \nabla T - K_\mathrm{wP} \nabla P - K_\mathrm{wP} \rho_w g \nabla z \)
    !>
    !> Assumptions:
    !> - Z-axis is aligned vertically with gravity
    !>
    !> Numerical guarantee:
    !> - No theoretical error bound available
    !>
    !> Computational complexity:
    !> - Memory: \(O(1)\)
    !> - Arithmetic: \(O(1)\)
    !>
    !> Failure behavior:
    !> - Returns without error
    module subroutine calc_water_flux_ftcms(self, material_id, state, grad_T, grad_P, water_flux)
        implicit none
        !> Main application object
        class(type_ftcms), intent(inout) :: self
        !> Material identifier
        !> Not modified
        integer(int32), intent(in) :: material_id
        !> Thermodynamic state
        !> Not modified
        type(type_state), intent(in) :: state
        !> Temperature gradient
        !> Not modified
        type(type_coordinate_dp), intent(in) :: grad_T
        !> Pressure gradient
        !> Not modified
        type(type_coordinate_dp), intent(in) :: grad_P
        !> Liquid water flux vector
        !> Overwritten on exit
        type(type_coordinate_dp), intent(inout) :: water_flux

        type(type_constant_id), pointer :: computation_type

        real(real64) :: K_wT, K_wP_raw, K_wP
        real(real64) :: rho_w, gravity_term

        nullify (computation_type)
        call self%domain%get_computation_type(computation_type)

        call self%hydraulic%calc_K_wT(material_id, state, K_wT)
        call self%hydraulic%calc_K_wP(material_id, state, K_wP_raw)

        ! Convert raw hydraulic conductivity to pressure-gradient coefficient.
        ! q = -K/(rho*g) * grad(P) - K * grad(z)
        call self%thermal%calc_density_water(state, rho_w)
        if (rho_w > tiny(1.0d0)) then
            K_wP = K_wP_raw / (rho_w * g)
        else
            K_wP = 0.0d0
        end if
        gravity_term = K_wP_raw

        ! --- Flux calculation (Darcy law: q = -K_wT*grad_T - K_wP*grad_P - K*grad_z) ---
        select case (computation_type%ID)
        case (COMP_TYPES%XY_2D%ID)
            water_flux%x = -K_wT * grad_T%x - K_wP * grad_P%x
            water_flux%y = -K_wT * grad_T%y - K_wP * grad_P%y
            water_flux%z = 0.0d0
        case (COMP_TYPES%XZ_2D%ID)
            water_flux%x = -K_wT * grad_T%x - K_wP * grad_P%x
            water_flux%y = 0.0d0
            water_flux%z = -K_wT * grad_T%z - K_wP * grad_P%z - gravity_term ! Assuming Z is vertical
        case (COMP_TYPES%XYZ_3D%ID)
            water_flux%x = -K_wT * grad_T%x - K_wP * grad_P%x
            water_flux%y = -K_wT * grad_T%y - K_wP * grad_P%y
            water_flux%z = -K_wT * grad_T%z - K_wP * grad_P%z - gravity_term ! Assuming Z is vertical
        end select

    end subroutine calc_water_flux_ftcms

    !> Calculate water vapor flux vector
    !>
    !> Mathematical definition:
    !> - \( \mathbf{q}_v = -K_\mathrm{vT} \nabla T - K_\mathrm{vP} \nabla P \)
    !>
    !> Assumptions:
    !> - Vapor transport neglects gravity terms
    !>
    !> Numerical guarantee:
    !> - No theoretical error bound available
    !>
    !> Computational complexity:
    !> - Memory: \(O(1)\)
    !> - Arithmetic: \(O(1)\)
    !>
    !> Failure behavior:
    !> - Returns without error
    module subroutine calc_vapor_flux_ftcms(self, material_id, state, grad_T, grad_P, water_flux)
        implicit none
        !> Main application object
        class(type_ftcms), intent(inout) :: self
        !> Material identifier
        !> Not modified
        integer(int32), intent(in) :: material_id
        !> Thermodynamic state
        !> Not modified
        type(type_state), intent(in) :: state
        !> Temperature gradient
        !> Not modified
        type(type_coordinate_dp), intent(in) :: grad_T
        !> Pressure gradient
        !> Not modified
        type(type_coordinate_dp), intent(in) :: grad_P
        !> Vapor flux vector
        !> Overwritten on exit
        type(type_coordinate_dp), intent(inout) :: water_flux

        type(type_constant_id), pointer :: computation_type

        real(real64) :: K_vT, K_vP

        nullify (computation_type)
        call self%domain%get_computation_type(computation_type)

        if (.not. self%hydraulic%is_vapor_transport_enabled()) then
            call water_flux%set(0.0d0, 0.0d0, 0.0d0)
            return
        end if

        call self%hydraulic%calc_K_vT(material_id, state, K_vT)
        call self%hydraulic%calc_K_vP(material_id, state, K_vP)

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

    !> Update segregated ice content after convergence via forward Euler.
    !>
    !> Mathematical definition:
    !> \[ \theta_{i,\mathrm{seg}}^{n+1} = \theta_{i,\mathrm{seg}}^n + S_\mathrm{seg} \Delta t \]
    !> where \( S_\mathrm{seg} = \mathrm{SP}_0 \, |\nabla T| \, f_\mathrm{fringe}(T) \).
    !>
    !> Assumptions:
    !> - Called once per converged time step (explicit integration)
    !> - Nodal gradients are up-to-date
    !>
    !> Numerical guarantee:
    !> - \(\theta_{i,\mathrm{seg}} \geq 0\)
    !> - Extraction limited by available liquid water
    !>
    !> Computational complexity:
    !> - \(O(N_\mathrm{nodes})\)
    !>
    !> Failure behavior:
    !> - No-op if segregation is inactive for all materials
    module subroutine update_segregation_ice_ftcms(self)
        implicit none
        class(type_ftcms), intent(inout) :: self

        integer(int32) :: i_node, num_nodes, material_id
        integer(int32), pointer, contiguous :: element_list(:)
        real(real64) :: qi_seg_old, qi_seg_new, S_seg, dt_s
        type(type_state) :: local_state
        real(real64), parameter :: rho_water = 999.8d0 !< [kg/m^3]
        real(real64), parameter :: rho_ice = 917.0d0 !< [kg/m^3]
        real(real64), parameter :: density_ratio = rho_water / rho_ice !< ~1.0903

        call self%control%get_dt(dt_s)
        if (dt_s <= 0.0d0) return

        call self%domain%get_num_nodes(num_nodes)

        do i_node = 1, num_nodes
            ! Get first adjacent element for material_id
            nullify (element_list)
            call self%domain%element_adjacency%get_list(i_node, element_list)
            if (.not. associated(element_list)) cycle
            if (size(element_list) < 1) cycle

            call self%domain%get_material_id(element_list(1), material_id)

            ! Build nodal state (includes grad_T, ice/water content)
            call self%set_state(i_node, element_list(1), local_state)

            ! Compute the effective (already clamped) segregation sink.
            ! The hydraulic wrapper applies the same forward-Euler pore/water
            ! clamps used below, so the PDE residual and Qi_seg integrator
            ! see identical rates -- total water mass is conserved.
            S_seg = 0.0d0
            call self%hydraulic%calc_segregation_sink(material_id, local_state, dt_s, S_seg)
            if (abs(S_seg) <= 0.0d0) cycle

            call self%Qi_seg%get_current(i_node, qi_seg_old)
            qi_seg_new = qi_seg_old + density_ratio * S_seg * dt_s
            qi_seg_new = max(qi_seg_new, 0.0d0)

            call self%Qi_seg%set_current(i_node, qi_seg_new)
        end do

    end subroutine update_segregation_ice_ftcms

end submodule ftcms_compute
