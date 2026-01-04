module main_ftdss
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
    use :: module_physics, only:g => gravity_acceleration

    use :: module_thermal, only:type_thermal
    use :: module_hydraulic, only:type_hydraulic

    use :: module_solver
    implicit none

    type :: type_ftdss
        type(type_domain) :: domain

        type(type_variable) :: porosity
        type(type_variable) :: temperature
        type(type_variable) :: pressure

        ! type(type_coordinate_array_dp) :: water_flux
        ! type(type_coordinate_array_dp) :: vapor_flux

        type(type_variable) :: Qw
        type(type_variable) :: Qi
        type(type_variable) :: Qa
        type(type_variable) :: Qv

        type(type_jacobian_matrix) :: J
        type(type_residual_vector) :: R
        type(type_residual_vector) :: delta

        type(type_thermal) :: thermal
        type(type_hydraulic) :: hydraulic

        class(abst_solver), allocatable :: solver

        type(type_controls) :: controls
        type(type_output) :: output

    contains
        procedure, public, pass(self) :: initialize => initialize_type_ftdss
        procedure, public, pass(self) :: shift => shift_ftdss

        procedure, public, pass(self) :: calc_gradient => calc_gradient_ftdss
        procedure, public, pass(self) :: calc_gradient_temperature => calc_gradient_temperature_ftdss
        procedure, public, pass(self) :: calc_gradient_pressure => calc_gradient_pressure_ftdss

        procedure, public, pass(self) :: calc_water_flux => calc_water_flux_ftdss
        procedure, public, pass(self) :: calc_vapor_flux => calc_vapor_flux_ftdss

        ! --- Boundary Condition Procedures ---
        procedure, public, pass(self) :: apply_bc => apply_bc_ftdss
        procedure, private, pass(self) :: prescribe_essential_bc_generic
        procedure, private, pass(self) :: apply_natural_bc_generic
        procedure, private, pass(self) :: apply_essential_bc_generic

        !> ソルバー呼び出しルーチン
        procedure, public, pass(self) :: solve => solve_ftdss

        procedure, public, pass(self) :: set_state => set_state_ftdss
    end type type_ftdss

    interface
        module subroutine initialize_type_ftdss(self)
            implicit none
            class(type_ftdss), intent(inout) :: self

        end subroutine initialize_type_ftdss

        module subroutine prescribe_essential_bc_generic(self, physics_type, current_time, variable)
            implicit none
            class(type_ftdss), intent(inout), target :: self
            integer(int32), intent(in) :: physics_type
            real(real64), intent(in) :: current_time
            type(type_variable), intent(inout) :: variable

        end subroutine prescribe_essential_bc_generic

        module subroutine apply_natural_bc_generic(self, physics_type, current_time, variable, dof_offset)
            implicit none
            class(type_ftdss), intent(inout), target :: self
            integer(int32), intent(in) :: physics_type
            real(real64), intent(in) :: current_time
            type(type_variable), intent(in) :: variable
            integer(int32), intent(in) :: dof_offset

        end subroutine apply_natural_bc_generic

        module subroutine apply_essential_bc_generic(self, physics_type, current_time, variable, dof_offset)
            implicit none
            class(type_ftdss), intent(inout), target :: self
            integer(int32), intent(in) :: physics_type
            real(real64), intent(in) :: current_time
            type(type_variable), intent(in) :: variable
            integer(int32), intent(in) :: dof_offset

        end subroutine apply_essential_bc_generic

        module subroutine apply_bc_ftdss(self, prescribed)
            implicit none
            class(type_ftdss), intent(inout) :: self
            logical, intent(in), optional :: prescribed

        end subroutine apply_bc_ftdss
    end interface

contains

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

    subroutine calc_gradient_temperature_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self

        call self%calc_gradient(self%temperature%new, self%temperature%grad)

    end subroutine calc_gradient_temperature_ftdss

    subroutine calc_gradient_pressure_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self

        call self%calc_gradient(self%pressure%new, self%pressure%grad)

    end subroutine calc_gradient_pressure_ftdss

    subroutine calc_water_flux_ftdss(self, material_id, state, grad_T, grad_P, water_flux)
        implicit none
        class(type_ftdss), intent(inout) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        type(type_coordinate_dp), intent(in) :: grad_T, grad_P
        type(type_coordinate_dp), intent(inout) :: water_flux

        integer(int32) :: computation_type

        real(real64) :: K_wT, K_wP
        real(real64) :: rho_w, gravity_term

        computation_type = self%domain%get_computation_type()

        call self%hydraulic%calc_K_wT(material_id, state, K_wT)
        call self%hydraulic%calc_K_wP(material_id, state, K_wP)

        ! --- 重力項の計算 ---
        ! K_wP は K/(rho*g) なので，重力項(透水係数 K そのもの)を復元する
        ! gravity_term = K = K_wP * rho * g
        call self%thermal%calc_density_water(state, rho_w)
        gravity_term = K_wP * rho_w * g

        ! --- 流束の計算 (Darcy則: q = -K_wT*grad_T - K_wP*grad_P - K*grad_z) ---
        select case (computation_type)
        case (COMP_TYPE_2D_XY)
            water_flux%x = -K_wT * grad_T%x - K_wP * grad_P%x
            water_flux%y = -K_wT * grad_T%y - K_wP * grad_P%y
            water_flux%z = 0.0d0
        case (COMP_TYPE_2D_XZ)
            water_flux%x = -K_wT * grad_T%x - K_wP * grad_P%x
            water_flux%y = 0.0d0
            water_flux%z = -K_wT * grad_T%z - K_wP * grad_P%z - gravity_term ! Zを鉛直と仮定
        case (COMP_TYPE_3D)
            water_flux%x = -K_wT * grad_T%x - K_wP * grad_P%x
            water_flux%y = -K_wT * grad_T%y - K_wP * grad_P%y
            water_flux%z = -K_wT * grad_T%z - K_wP * grad_P%z - gravity_term ! Zを鉛直と仮定
        end select

    end subroutine calc_water_flux_ftdss

    subroutine calc_vapor_flux_ftdss(self, material_id, state, grad_T, grad_P, water_flux)
        implicit none
        class(type_ftdss), intent(inout) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        type(type_coordinate_dp), intent(in) :: grad_T, grad_P
        type(type_coordinate_dp), intent(inout) :: water_flux

        integer(int32) :: computation_type

        real(real64) :: K_vT, K_vP

        computation_type = self%domain%get_computation_type()

        call self%hydraulic%calc_K_vT(material_id, state, K_vT)
        call self%hydraulic%calc_K_vP(material_id, state, K_vP)

        select case (computation_type)
        case (COMP_TYPE_2D_XY)
            water_flux%x = -K_vT * grad_T%x - K_vP * grad_P%x
            water_flux%y = -K_vT * grad_T%y - K_vP * grad_P%y
            water_flux%z = 0.0d0
        case (COMP_TYPE_2D_XZ)
            water_flux%x = -K_vT * grad_T%x - K_vP * grad_P%x
            water_flux%y = 0.0d0
            water_flux%z = -K_vT * grad_T%z - K_vP * grad_P%z
        case (COMP_TYPE_3D)
            water_flux%x = -K_vT * grad_T%x - K_vP * grad_P%x
            water_flux%y = -K_vT * grad_T%y - K_vP * grad_P%y
            water_flux%z = -K_vT * grad_T%z - K_vP * grad_P%z
        end select

    end subroutine calc_vapor_flux_ftdss

    subroutine set_state_ftdss(self, node_id, element_id, state)
        implicit none
        class(type_ftdss), intent(inout) :: self
        integer(int32), intent(in) :: node_id
        integer(int32), intent(in) :: element_id
        type(type_state), intent(inout) :: state

        integer(int32) :: material_id
        type(type_coordinate_dp) :: grad_T, grad_P
        type(type_coordinate_dp) :: water_flux, vapor_flux
        real(real64) :: K_wT, K_wP, K_vT, K_vP

        call state%reset()

        grad_T%x = self%temperature%grad%x(node_id)
        grad_T%y = self%temperature%grad%y(node_id)
        grad_T%z = self%temperature%grad%z(node_id)
        grad_P%x = self%pressure%grad%x(node_id)
        grad_P%y = self%pressure%grad%y(node_id)
        grad_P%z = self%pressure%grad%z(node_id)

        call state%set(temperature=self%temperature%new(node_id), &
                       pressure=self%pressure%new(node_id), &
                       porosity=self%porosity%new(node_id), &
                       dot_T=self%temperature%dif(node_id), &
                       dot_P=self%pressure%dif(node_id), &
                       grad_T=grad_T, &
                       grad_P=grad_P)

        call self%domain%get_material_id(element_id, material_id)
        if (self%controls%is_target(PHYSICS_TYPE_HYDRAULIC, material_id)) then
            call self%calc_water_flux(material_id, state, grad_T, grad_P, water_flux)
            call self%calc_vapor_flux(material_id, state, grad_T, grad_P, vapor_flux)
            call state%set(water_flux=water_flux, vapor_flux=vapor_flux)
        end if

        call self%thermal%update_water_phases(material_id, state)

    end subroutine set_state_ftdss

    subroutine shift_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self
        ! 必要なShift処理があればここに記述
    end subroutine shift_ftdss

    subroutine solve_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self

        class(abst_matrix), pointer :: J_ptr
        type(type_vector_dp), pointer :: R_ptr
        type(type_vector_dp), pointer :: delta_prt

        call self%controls%profiler%start("Solve")

        J_ptr => self%J%get_matrix()
        R_ptr => self%R%get_vector()
        delta_prt => self%delta%get_vector()

        call self%solver%solve(J_ptr, R_ptr, delta_prt)
        call self%solver%check()

        J_ptr => null()
        R_ptr => null()
        delta_prt => null()
        call self%controls%profiler%stop("Solve")

    end subroutine solve_ftdss

end module main_ftdss
