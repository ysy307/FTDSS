module module_ftdss
    use, intrinsic :: iso_fortran_env
    use :: mpi_f08
    use :: stdlib_logger
    use :: module_core
    use :: module_input, only:type_input
    use :: module_output, only:type_output

    use :: module_control, only:type_controls
    use :: module_domain, only:type_domain, abst_fe
    use :: module_boundary, only:abst_bc, type_bc_dirichlet
    use :: module_initial, only:type_ic_manager
    use :: module_field, only:type_jacobian_matrix, type_residual_vector

    use :: module_thermal, only:type_thermal
    use :: module_hydraulic, only:type_hydraulic
    implicit none

    type :: type_ftdss
        type(type_domain) :: domain

        type(type_variable) :: porosity
        type(type_variable) :: temperature
        type(type_variable) :: pressure

        type(type_coordinate_array_dp) :: water_flux
        type(type_coordinate_array_dp) :: vapor_flux

        type(type_variable) :: Qw
        type(type_variable) :: Qi
        type(type_variable) :: Qa
        type(type_variable) :: Qv

        type(type_jacobian_matrix) :: J
        type(type_residual_vector) :: R

        type(type_thermal) :: thermal
        type(type_hydraulic) :: hydraulic

        type(type_controls) :: controls
        type(type_output) :: output

    contains
        procedure, public, pass(self) :: initialize => initialize_type_ftdss
        procedure, public, pass(self) :: shift => shift_type_ftdss
        procedure, public, pass(self) :: calc_gradient => calc_gradient_ftdss

        ! --- Boundary Condition Procedures ---
        procedure, public, pass(self) :: apply_bc => apply_bc_ftdss
        procedure, private, pass(self) :: prescribe_essential_bc_generic
        procedure, private, pass(self) :: apply_natural_bc_generic
        procedure, private, pass(self) :: apply_essential_bc_generic
    end type type_ftdss

contains

    subroutine initialize_type_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self

        type(type_input) :: input
        type(type_ic_manager) :: ic

        integer(int32) :: max_bdf_order
        integer(int32), allocatable :: active_region_ids(:)
        integer(int32) :: num_nodes
        character(len=10), allocatable :: profiler_labels(:)
        real(real64) :: current_time

        profiler_labels = [character(len=10) :: "IO", "Setup", "Assemble", "Solve", "Total"]
        call self%controls%profiler%initialize(profiler_labels)
        call self%controls%profiler%record(TIME_RECORD_START)
        call self%controls%profiler%start("Total")
        call self%controls%profiler%start("IO")

        call setup_handler()

        call input%initialize()
        call self%controls%initialize(input)
        call ic%initialize(input)

        if (input%output_settings%standard_output%print_progress) then
            call global_logger%configure(level=information_level, &
                                         time_stamp=.true., &
                                         max_width=0)
        else
            call global_logger%configure(level=warning_level, &
                                         time_stamp=.true., &
                                         max_width=0)
        end if

        num_nodes = input%geometry%vtk%num_points
        call self%domain%initialize(input, self%controls)

        call self%J%initialize(self%domain)
        call self%R%initialize(self%domain)

        max_bdf_order = input%basic%solver_settings%bdf_order
        call self%porosity%initialize(num_nodes, max_bdf_order)
        call ic%apply(IC_TARGET_POROSITY, self%porosity)

        if (self%controls%is_physics_active(PHYSICS_TYPE_THERMAL)) then
            call self%temperature%initialize(num_nodes, max_bdf_order)
            call ic%apply(IC_TARGET_THERMAL, self%temperature)
        end if

        if (self%controls%is_physics_active(PHYSICS_TYPE_HYDRAULIC)) then
            call self%pressure%initialize(num_nodes, max_bdf_order)
            call ic%apply(IC_TARGET_HYDRAULIC, self%pressure)
        end if

        call self%Qw%initialize(num_nodes, max_bdf_order)
        call self%Qi%initialize(num_nodes, max_bdf_order)
        call self%Qa%initialize(num_nodes, max_bdf_order)
        call self%Qv%initialize(num_nodes, max_bdf_order)

        call input%geometry%vtk%get_active_region_info(active_region_ids, target_dim=self%domain%get_computation_dimension())

        call self%thermal%initialize(input, active_region_ids)
        call self%hydraulic%initialize(input, active_region_ids)

        ! 初期化時にBCを適用（Dirichlet値をフィールドに設定）
        call self%apply_bc()

        call self%output%initialize(input, self%controls, self%domain)

        call self%output%output_fields(0, self%domain, self%porosity%pre, &
                                       self%temperature%pre, self%Qw%pre, self%pressure%pre)
        call self%controls%time%get_time(current_time)
        call self%output%output_history(current_time, self%domain, self%porosity%pre, &
                                        self%temperature%pre, self%pressure%pre)

        call self%controls%profiler%stop("IO")
        call global_logger%log_information(message="FTDSS module initialized successfully.")
    end subroutine initialize_type_ftdss

    !>
    !> 節点上の物理量勾配を計算する（L2射影 / Lumped Mass法）
    !>
    subroutine calc_gradient_ftdss(self, values_vec, grad)
        implicit none
        class(type_ftdss), intent(inout) :: self
        real(real64), intent(in) :: values_vec(:)
        type(type_coordinate_array_dp), intent(inout) :: grad

        class(abst_fe), pointer :: fe
        integer(int32), dimension(:), pointer, contiguous :: p_conn

        ! 要素データ用配列
        real(real64), allocatable :: elem_u(:)
        real(real64), allocatable :: node_coords(:, :)
        real(real64), allocatable :: psi(:)
        real(real64), allocatable :: dpsi_dx(:, :)

        ! FE情報キャッシュ用
        real(real64), allocatable :: fe_weights(:)
        type(type_coordinate_dp), allocatable :: fe_gauss_pts(:)

        real(real64), allocatable :: nodal_vol(:)

        real(real64) :: det_j
        real(real64) :: gauss_grad(3)
        real(real64) :: w_vol, shape_weight
        type(type_coordinate_dp) :: r

        integer(int32) :: num_elements, num_total_nodes, dim
        integer(int32) :: n_nodes_elem, n_gauss
        integer(int32) :: i, p, k, d, global_nid

        num_elements = self%domain%get_num_elements()
        num_total_nodes = self%domain%get_num_nodes()
        dim = self%domain%get_computation_dimension()

        call grad%zero()

        if (allocated(nodal_vol)) deallocate (nodal_vol)
        allocate (nodal_vol(num_total_nodes))
        nodal_vol(:) = 0.0d0

        do i = 1, num_elements
            call self%domain%get_element(i, fe)
            call self%domain%get_element_connectivity(i, p_conn)

            call fe%get_num_nodes(n_nodes_elem)
            call fe%get_num_gauss(n_gauss)

            call fe%get_weight(fe_weights)
            call fe%get_gauss(fe_gauss_pts)

            ! 作業用配列の再確保 (allocatableは自動再割り当てされる場合もあるが明示的に管理)
            if (allocated(elem_u)) deallocate (elem_u)
            if (allocated(psi)) deallocate (psi)
            if (allocated(dpsi_dx)) deallocate (dpsi_dx)
            ! node_coordsは get_element_coordinate 内で handle されるためここでは deallocate しない方が安全だが、
            ! エラー回避のために明示的に ensure することも可能。
            ! ここでは元のコードの意図通り get_element_coordinate に任せる。

            allocate (elem_u(n_nodes_elem))
            allocate (psi(n_nodes_elem))
            allocate (dpsi_dx(n_nodes_elem, dim))

            elem_u(:) = values_vec(p_conn(:))

            ! 座標取得 (allocatable引数)
            call self%domain%get_element_coordinate(i, node_coords)

            do p = 1, n_gauss
                r = fe_gauss_pts(p)

                call fe%calc_shape_data(r, node_coords, p_conn, psi, dpsi_dx, det_j)
                w_vol = fe_weights(p) * det_j

                gauss_grad = 0.0d0
                do d = 1, dim
                    gauss_grad(d) = dot_product(elem_u, dpsi_dx(:, d))
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
        if (allocated(fe_weights)) deallocate (fe_weights)
        if (allocated(fe_gauss_pts)) deallocate (fe_gauss_pts)

    end subroutine calc_gradient_ftdss

    subroutine shift_type_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self
        ! 必要なShift処理があればここに記述
    end subroutine shift_type_ftdss

    !>
    !> Applies all boundary conditions for active physics.
    !> Order: Prescribe (Step 0) -> Natural (Step 1) -> Essential (Step 2)
    !>
    subroutine apply_bc_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self
        real(real64) :: current_time
        integer(int32) :: dof_offset

        call self%controls%time%get_time(current_time)

        ! ----------------------------------------------------------------------
        ! Step 0: Prescribe Dirichlet Values (Update Field Variables directly)
        ! ----------------------------------------------------------------------
        if (self%controls%is_physics_active(PHYSICS_TYPE_THERMAL)) then
            call self%prescribe_essential_bc_generic(PHYSICS_TYPE_THERMAL, current_time, self%temperature)
        end if

        if (self%controls%is_physics_active(PHYSICS_TYPE_HYDRAULIC)) then
            call self%prescribe_essential_bc_generic(PHYSICS_TYPE_HYDRAULIC, current_time, self%pressure)
        end if

        ! ----------------------------------------------------------------------
        ! Step 1: Apply Natural BCs (Neumann, Robin, etc.)
        ! ----------------------------------------------------------------------
        if (self%controls%is_physics_active(PHYSICS_TYPE_THERMAL)) then
            dof_offset = self%domain%dof_map%start_dof_index(PHYSICS_TYPE_THERMAL)
            call self%apply_natural_bc_generic(PHYSICS_TYPE_THERMAL, current_time, &
                                               self%temperature, dof_offset)
        end if

        if (self%controls%is_physics_active(PHYSICS_TYPE_HYDRAULIC)) then
            dof_offset = self%domain%dof_map%start_dof_index(PHYSICS_TYPE_HYDRAULIC)
            call self%apply_natural_bc_generic(PHYSICS_TYPE_HYDRAULIC, current_time, &
                                               self%pressure, dof_offset)
        end if

        ! ----------------------------------------------------------------------
        ! Step 2: Apply Essential BCs (Dirichlet Constraints)
        ! ----------------------------------------------------------------------
        if (self%controls%is_physics_active(PHYSICS_TYPE_THERMAL)) then
            dof_offset = self%domain%dof_map%start_dof_index(PHYSICS_TYPE_THERMAL)
            call self%apply_essential_bc_generic(PHYSICS_TYPE_THERMAL, current_time, &
                                                 self%temperature, dof_offset)
        end if

        if (self%controls%is_physics_active(PHYSICS_TYPE_HYDRAULIC)) then
            dof_offset = self%domain%dof_map%start_dof_index(PHYSICS_TYPE_HYDRAULIC)
            call self%apply_essential_bc_generic(PHYSICS_TYPE_HYDRAULIC, current_time, &
                                                 self%pressure, dof_offset)
        end if

    end subroutine apply_bc_ftdss

    !>
    !> Enforces Dirichlet values directly into the solution vector.
    !>
    subroutine prescribe_essential_bc_generic(self, physics_type, current_time, variable)
        implicit none
        class(type_ftdss), intent(inout), target :: self
        integer(int32), intent(in) :: physics_type
        real(real64), intent(in) :: current_time
        type(type_variable), intent(inout) :: variable

        integer(int32) :: i_patch, i, glob_node_id
        real(real64) :: val_fixed
        logical :: is_active
        class(abst_bc), pointer :: bc_obj

        associate (phys_mgr => self%domain%boundaries%physics(physics_type))
            do i_patch = 1, phys_mgr%num_bcs
                bc_obj => phys_mgr%bcs(i_patch)%condition

                call bc_obj%get_dirichlet_value(current_time, val_fixed, is_active)
                if (.not. is_active) cycle

                associate (patch => phys_mgr%bcs(i_patch))
                    do i = 1, size(patch%connectivity%val)
                        glob_node_id = patch%connectivity%val(i)
                        ! 現在の変数値をBC値に上書き
                        variable%pre(glob_node_id) = val_fixed
                        variable%new(glob_node_id) = val_fixed
                    end do
                end associate
            end do
        end associate
    end subroutine prescribe_essential_bc_generic

    !>
    !> Generic routine to integrate and assemble Natural BCs (Fluxes).
    !>
    subroutine apply_natural_bc_generic(self, physics_type, current_time, variable, dof_offset)
        implicit none
        class(type_ftdss), intent(inout), target :: self
        integer(int32), intent(in) :: physics_type
        real(real64), intent(in) :: current_time
        type(type_variable), intent(in) :: variable
        integer(int32), intent(in) :: dof_offset

        integer(int32) :: i_patch, i_elem, k_gp
        integer(int32) :: num_nodes_loc
        integer(int32) :: i, j
        integer(int32) :: num_gp

        real(real64) :: u_curr, q_flux, dq_du, w_vol, det_j

        real(real64), allocatable :: psi(:)
        real(real64), allocatable :: dpsi_dx(:, :)
        real(real64), allocatable :: node_coords(:, :)
        real(real64), allocatable :: fe_weights(:)
        type(type_coordinate_dp), allocatable :: fe_gauss_pts(:)
        type(type_coordinate_dp) :: r

        integer(int32), pointer, contiguous, dimension(:) :: connectivity
        class(abst_bc), pointer :: bc_obj
        class(abst_fe), pointer :: fe

        associate (phys_mgr => self%domain%boundaries%physics(physics_type))
            do i_patch = 1, phys_mgr%num_bcs
                bc_obj => phys_mgr%bcs(i_patch)%condition

                select type (bc_obj)
                type is (type_bc_dirichlet)
                    cycle
                end select

                associate (patch => phys_mgr%bcs(i_patch))
                    ! パッチ内の全ての境界要素についてFEタイプは同一と仮定して代表を取得 (index=1)
                    fe => patch%fe_manager%get_fe(1)

                    do i_elem = 1, patch%num_elements

                        ! コネクティビティ取得
                        call self%domain%get_element_connectivity( &
                            patch%connectivity%val(patch%connectivity%ind(i_elem)), connectivity)

                        if (.not. associated(connectivity)) cycle
                        num_nodes_loc = size(connectivity)

                        ! 座標取得 (境界要素に対応するノード群)
                        if (allocated(node_coords)) deallocate (node_coords)
                        allocate (node_coords(self%domain%computation_dimension, num_nodes_loc))
                        node_coords = self%domain%nodes%coordinates(:, connectivity)

                        ! ガウス積分情報
                        call fe%get_num_gauss(num_gp)
                        call fe%get_weight(fe_weights)
                        call fe%get_gauss(fe_gauss_pts)

                        if (allocated(psi)) deallocate (psi)
                        if (allocated(dpsi_dx)) deallocate (dpsi_dx)
                        allocate (psi(num_nodes_loc))
                        allocate (dpsi_dx(num_nodes_loc, self%domain%computation_dimension))

                        do k_gp = 1, num_gp
                            r = fe_gauss_pts(k_gp)

                            call fe%calc_shape_data(r, node_coords, connectivity, psi, dpsi_dx, det_j)
                            w_vol = fe_weights(k_gp) * det_j

                            u_curr = 0.0d0
                            do i = 1, num_nodes_loc
                                u_curr = u_curr + psi(i) * variable%pre(connectivity(i))
                            end do

                            call bc_obj%get_flux_and_derivative(current_time, u_curr, q_flux, dq_du)

                            do i = 1, num_nodes_loc
                                ! Residual: add(row_dof, global_node_index, value)
                                call self%R%add(dof_offset, connectivity(i), psi(i) * q_flux * w_vol)

                                do j = 1, num_nodes_loc
                                    ! Jacobian: add(row_dof, col_dof, row_node, col_node, value)
                                    call self%J%add(dof_offset, dof_offset, &
                                                    connectivity(i), connectivity(j), &
                                                    psi(i) * dq_du * psi(j) * w_vol)
                                end do
                            end do
                        end do
                    end do
                end associate
            end do
        end associate

    end subroutine apply_natural_bc_generic

    !>
    !> Generic routine to apply Essential BCs (Dirichlet Constraints).
    !>
    subroutine apply_essential_bc_generic(self, physics_type, current_time, variable, dof_offset)
        implicit none
        class(type_ftdss), intent(inout), target :: self
        integer(int32), intent(in) :: physics_type
        real(real64), intent(in) :: current_time
        type(type_variable), intent(in) :: variable
        integer(int32), intent(in) :: dof_offset

        integer(int32) :: i_patch, i, glob_node_id
        real(real64) :: val_fixed, val_curr
        logical :: is_active
        class(abst_bc), pointer :: bc_obj

        associate (phys_mgr => self%domain%boundaries%physics(physics_type))
            do i_patch = 1, phys_mgr%num_bcs
                bc_obj => phys_mgr%bcs(i_patch)%condition

                call bc_obj%get_dirichlet_value(current_time, val_fixed, is_active)
                if (.not. is_active) cycle

                associate (patch => phys_mgr%bcs(i_patch))
                    do i = 1, size(patch%connectivity%val)
                        glob_node_id = patch%connectivity%val(i)

                        val_curr = variable%pre(glob_node_id)

                        ! 1. Jacobianの行をゼロ化 (zero_row使用)
                        call self%J%zero(glob_node_id, dof_offset)

                        !    対角成分に1.0をセット
                        call self%J%set(dof_offset, dof_offset, glob_node_id, glob_node_id, 1.0d0)

                        ! 2. 残差ベクトルを上書き (set使用)
                        call self%R%set(dof_offset, glob_node_id, val_curr - val_fixed)
                    end do
                end associate
            end do
        end associate

    end subroutine apply_essential_bc_generic

end module module_ftdss
