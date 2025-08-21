module thermal_thermal_assemble
    use, intrinsic :: iso_fortran_env, only: int32, real64
!$  use omp_lib
    use :: module_core, only:type_state, type_variable, allocate_array, deallocate_array
    use :: module_domain, only:type_domain, abst_element
    use :: module_properties, only:type_properties_manager
    use :: module_matrix, only:type_crs
    use :: module_control, only:type_time

    implicit none
    private

    public :: abst_assemble_global

    ! public :: assemble_mass_heat_1, assemble_diffusion_heat_1
    ! public :: assemble_mass_heat_1_parallel, assemble_diffusion_heat_1_parallel
    public :: thermal_assemble_system_linear_1, thermal_assemble_system_linear_1_parallel

    abstract interface
        subroutine abst_assemble_global(J, R, domain, temperature, porosity, properties, time, actual_order)
            import :: type_crs, type_domain, type_properties_manager, type_variable, type_time, int32, real64
            implicit none
            type(type_crs), intent(inout) :: J
            real(real64), intent(inout) :: R(:)
            type(type_domain), intent(inout), target :: domain
            type(type_variable), intent(in) :: temperature
            type(type_variable), intent(in) :: porosity
            type(type_properties_manager), intent(in) :: properties
            type(type_time), intent(in) :: time
            integer(int32), intent(in) :: actual_order
        end subroutine abst_assemble_global
    end interface
contains

    subroutine process_element_thermal_linear_1(J, R, element, temperature, porosity, properties, time, actual_order)
        implicit none

        ! --- 引数 ---
        type(type_crs), intent(inout) :: J
        real(real64), intent(inout) :: R(:)
        class(abst_element), intent(in), pointer :: element
        type(type_variable), intent(in) :: temperature
        type(type_variable), intent(in) :: porosity
        type(type_properties_manager), intent(in) :: properties
        type(type_time), intent(in) :: time
        integer(int32), intent(in) :: actual_order

        ! --- ローカル変数 ---
        integer(int32) :: index, num_nodes, num_gauss, i_material, il, jl, iG, iO
        real(real64) :: xi, eta, weight, detJ
        real(real64) :: dNdx_i, dNdy_i, dNdx_j, dNdy_j
        real(real64) :: val
        real(real64) :: dt_n
        real(real64) :: coefficients(0:actual_order)

        ! --- スタック上のワークスペース (自動配列) ---
        real(real64) :: CT_e(element%get_num_nodes(), element%get_num_nodes())
        real(real64) :: KT_e(element%get_num_nodes(), element%get_num_nodes())
        real(real64) :: J_e(element%get_num_nodes(), element%get_num_nodes())
        real(real64) :: R_e(element%get_num_nodes())
        real(real64) :: T_hist_e(element%get_num_nodes())

        ! --- ガウスポイントでの物理量 (自動配列) ---
        type(type_state) :: state(element%get_num_gauss())
        real(real64) :: Ca(element%get_num_gauss()), lambda(element%get_num_gauss())

        ! ==========================================================================
        ! STEP 0: 初期化とサイズの取得
        ! ==========================================================================
        num_nodes = element%get_num_nodes()
        num_gauss = element%get_num_gauss()
        i_material = element%get_group()

        CT_e(:, :) = 0.0d0
        KT_e(:, :) = 0.0d0

        ! ==========================================================================
        ! STEP 1: 全ガウスポイントの物理量を一括計算
        ! ==========================================================================
        do iG = 1, num_gauss
            xi = element%gauss(1, iG)
            eta = element%gauss(2, iG)
            state(iG)%temperature = element%interpolate(xi, eta, temperature%pre)
            state(iG)%porosity = element%interpolate(xi, eta, porosity%pre)
        end do
        call properties%calc_thermal(state, i_material, lambda, Ca)

        ! ==========================================================================
        ! STEP 2: 要素行列 CT_e と KT_e を計算
        ! ==========================================================================
        do iG = 1, num_gauss
            xi = element%gauss(1, iG)
            eta = element%gauss(2, iG)
            weight = element%weight(iG)
            detJ = element%jacobian_det(xi, eta)
            do il = 1, num_nodes
                dNdx_i = (element%jacobian(2, 2, xi, eta) * element%dpsi_dxi(il, xi, eta) - &
                          element%jacobian(2, 1, xi, eta) * element%dpsi_deta(il, xi, eta)) / detJ
                dNdy_i = (-element%jacobian(1, 2, xi, eta) * element%dpsi_dxi(il, xi, eta) + &
                          element%jacobian(1, 1, xi, eta) * element%dpsi_deta(il, xi, eta)) / detJ
                do jl = 1, num_nodes
                    dNdx_j = (element%jacobian(2, 2, xi, eta) * element%dpsi_dxi(jl, xi, eta) - &
                              element%jacobian(2, 1, xi, eta) * element%dpsi_deta(jl, xi, eta)) / detJ
                    dNdy_j = (-element%jacobian(1, 2, xi, eta) * element%dpsi_dxi(jl, xi, eta) + &
                              element%jacobian(1, 1, xi, eta) * element%dpsi_deta(jl, xi, eta)) / detJ

                    CT_e(il, jl) = CT_e(il, jl) + element%psi(il, xi, eta) * element%psi(jl, xi, eta) * Ca(iG) * weight * detJ
                    KT_e(il, jl) = KT_e(il, jl) + (dNdx_i * dNdx_j + dNdy_i * dNdy_j) * lambda(iG) * weight * detJ
                end do
            end do
        end do

        dt_n = time%dt
        call time%get_time_coefficients(actual_order, coefficients)

        ! ==========================================================================
        ! STEP 3: 最終的な LHS(J_e) と RHS(R_e) を構築
        ! ==========================================================================
        ! --- 3a. LHS行列 J_e の構築 (物理的に正しい式) ---
        J_e(:, :) = 0.0d0
        do jl = 1, num_nodes
            do il = 1, num_nodes
                J_e(il, jl) = coefficients(0) * CT_e(il, jl) + dt_n * KT_e(il, jl)
            end do
        end do

        T_hist_e(:) = 0.0d0
        do il = 1, num_nodes
            do iO = 1, actual_order
                T_hist_e(il) = T_hist_e(il) + coefficients(iO) * temperature%old(element%get_connectivity(il), iO)
            end do
        end do

        do il = 1, num_nodes
            val = 0.0d0
            do jl = 1, num_nodes
                val = val + CT_e(il, jl) * T_hist_e(jl)
            end do
            R_e(il) = -val
        end do

        ! ==========================================================================
        ! STEP 4: 全体行列・ベクトルへのアセンブル (数学的に正しい標準手順)
        ! ==========================================================================
        do il = 1, num_nodes
            R(element%get_connectivity(il)) = R(element%get_connectivity(il)) + R_e(il)
            do jl = 1, num_nodes
                call J%find(element%get_connectivity(il), element%get_connectivity(jl), index)
                J%val(index) = J%val(index) + J_e(il, jl)
            end do
        end do

    end subroutine process_element_thermal_linear_1

    subroutine thermal_assemble_system_linear_1(J, R, domain, temperature, porosity, properties, time, actual_order)
        implicit none
        type(type_crs), intent(inout) :: J
        real(real64), intent(inout) :: R(:)
        type(type_domain), intent(inout), target :: domain
        type(type_variable), intent(in) :: temperature
        type(type_variable), intent(in) :: porosity
        type(type_properties_manager), intent(in) :: properties
        type(type_time), intent(in) :: time
        integer(int32), intent(in) :: actual_order

        class(abst_element), pointer :: element
        integer(int32) :: iE, num_elements

        num_elements = domain%get_num_elements()
        J%val(:) = 0.0d0
        R(:) = 0.0d0

        do iE = 1, num_elements
            element => domain%Elements(iE)%e
            call process_element_thermal_linear_1(J, R, element, temperature, porosity, properties, time, actual_order)
        end do
    end subroutine thermal_assemble_system_linear_1

    subroutine thermal_assemble_system_linear_1_parallel(J, R, domain, temperature, porosity, properties, time, actual_order)
        implicit none
        type(type_crs), intent(inout) :: J
        real(real64), intent(inout) :: R(:)
        type(type_domain), intent(inout), target :: domain
        type(type_variable), intent(in) :: temperature
        type(type_variable), intent(in) :: porosity
        type(type_properties_manager), intent(in) :: properties
        type(type_time), intent(in) :: time
        integer(int32), intent(in) :: actual_order

        integer(int32) :: c, ie_idx
        class(abst_element), pointer :: element

        J%val(:) = 0.0d0
        R(:) = 0.0d0

        !$omp parallel private(c, ie_idx, element)
        do c = 1, domain%colors%num_colors
            !$omp do
            do ie_idx = 1, domain%colors%colored(c)%num_elements
                element => domain%Elements(domain%colors%colored(c)%Elements(ie_idx))%e
                call process_element_thermal_linear_1(J, R, element, temperature, porosity, properties, time, actual_order)
            end do
            !$omp end do
        end do
        !$omp end parallel
    end subroutine thermal_assemble_system_linear_1_parallel

end module thermal_thermal_assemble
