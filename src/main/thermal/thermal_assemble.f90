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

    public :: assemble_mass_heat_1, assemble_diffusion_heat_1
    public :: assemble_mass_heat_1_parallel, assemble_diffusion_heat_1_parallel
    public :: thermal_assemble_system_linear_1, thermal_assemble_system_linear_1_parallel

    abstract interface
        subroutine abst_assemble_global(J, R, domain, temperature, porosity, propeties, time, actual_order)
            import :: type_crs, type_domain, type_properties_manager, type_variable, type_time, int32, real64
            implicit none
            type(type_crs), intent(inout) :: J
            real(real64), intent(inout) :: R(:)
            type(type_domain), intent(inout), target :: domain
            type(type_variable), intent(in) :: temperature
            type(type_variable), intent(in) :: porosity
            type(type_properties_manager), intent(inout) :: propeties
            type(type_time), intent(in) :: time
            integer(int32), intent(in) :: actual_order
        end subroutine abst_assemble_global
    end interface
contains

    subroutine process_single_element_mass(A, element, temperature, porosity, propeties)
        implicit none
        ! --- 引数 ---
        type(type_crs), intent(inout) :: A
        class(abst_element), pointer, intent(inout) :: element
        real(real64), intent(in) :: temperature(:)
        real(real64), intent(in) :: porosity(:)
        type(type_properties_manager), intent(inout) :: propeties

        ! --- ローカル変数 ---
        integer(int32) :: index, num_nodes, num_gauss, i_material, il, jl, iG
        real(real64) :: val, xi, eta, weight, detJ
        type(type_state) :: state

        ! 並列版のコードに合わせて、事前補間用の配列をローカルに用意
        integer(int32), parameter :: max_gauss = 10
        real(real64) :: Ca(max_gauss)

        state%pressure = 101325.0d0
        state%water_content = 0.0d0

        num_nodes = element%get_num_nodes()
        i_material = element%get_group()
        num_gauss = element%get_num_gauss()

        ! 積分点での物理量を事前に補間
        !$omp simd private(state)
        do iG = 1, num_gauss
            xi = element%gauss(1, iG)
            eta = element%gauss(2, iG)
            state%temperature = element%interpolate(xi, eta, temperature)
            state%porosity = element%interpolate(xi, eta, porosity)
            state%water_content = propeties%get_qw(state, i_material)
            Ca(iG) = propeties%get_vhc(state, i_material)
        end do

        ! 要素行列の計算とアセンブル
        do il = 1, num_nodes
            do jl = 1, num_nodes
                val = 0.0d0

                ! 積分ループ
                !$omp simd reduction(+:val)
                do iG = 1, num_gauss
                    xi = element%gauss(1, iG)
                    eta = element%gauss(2, iG)
                    weight = element%weight(iG)
                    detJ = element%jacobian_det(xi, eta)

                    val = val + (element%psi(il, xi, eta) * &
                                 element%psi(jl, xi, eta) * &
                                 detJ * weight * Ca(iG))
                end do

                ! 全体行列へのアセンブル
                call A%find(element%get_connectivity(il), element%get_connectivity(jl), index)
                A%val(index) = A%val(index) + val
            end do
        end do

    end subroutine process_single_element_mass

    subroutine process_single_element_diffusion(A, element, temperature, porosity, propeties)
        implicit none
        ! --- 引数 ---
        type(type_crs), intent(inout) :: A
        class(abst_element), pointer, intent(inout) :: element
        real(real64), intent(in) :: temperature(:)
        real(real64), intent(in) :: porosity(:)
        type(type_properties_manager), intent(inout) :: propeties

        ! --- ローカル変数 ---
        integer(int32) :: index, num_nodes, num_gauss, i_material, il, jl, iG, global_il, global_jl
        real(real64) :: val, xi, eta, weight, detJ
        real(real64) :: dNdx_i, dNdy_i, dNdx_j, dNdy_j
        real(real64) :: lambda_gp
        type(type_state) :: state

        ! 並列版のコードに合わせて、事前補間用の配列をローカルに用意
        integer(int32), parameter :: max_gauss = 10
        real(real64) :: lambda(max_gauss)
        real(real64) :: interp_temp(max_gauss), interp_poro(max_gauss)

        state%pressure = 101325.0d0
        state%water_content = 0.0d0

        num_nodes = element%get_num_nodes()
        i_material = element%get_group()
        num_gauss = element%get_num_gauss()

        ! 積分点での物理量を事前に補間
        !$omp simd
        do iG = 1, num_gauss
            xi = element%gauss(1, iG)
            eta = element%gauss(2, iG)
            state%temperature = element%interpolate(xi, eta, temperature)
            state%porosity = element%interpolate(xi, eta, porosity)
            state%water_content = propeties%get_qw(state, i_material)
            lambda(iG) = propeties%get_thc(state, i_material)
        end do

        ! 要素行列の計算とアセンブル
        do il = 1, num_nodes
            do jl = 1, num_nodes
                val = 0.0d0

                ! 積分ループ
                !$omp simd reduction(+:val)
                do iG = 1, num_gauss
                    xi = element%gauss(1, iG)
                    eta = element%gauss(2, iG)
                    weight = element%weight(iG)
                    detJ = element%jacobian_det(xi, eta)
                    ! 形状関数の勾配
                    dNdx_i = (element%jacobian(2, 2, xi, eta) * &
                              element%dpsi_dxi(il, xi, eta) - &
                              element%jacobian(2, 1, xi, eta) * &
                              element%dpsi_deta(il, xi, eta)) / detJ
                    dNdy_i = (-element%jacobian(1, 2, xi, eta) * &
                              element%dpsi_dxi(il, xi, eta) + &
                              element%jacobian(1, 1, xi, eta) * &
                              element%dpsi_deta(il, xi, eta)) / detJ
                    dNdx_j = (element%jacobian(2, 2, xi, eta) * &
                              element%dpsi_dxi(jl, xi, eta) - &
                              element%jacobian(2, 1, xi, eta) * &
                              element%dpsi_deta(jl, xi, eta)) / detJ
                    dNdy_j = (-element%jacobian(1, 2, xi, eta) * &
                              element%dpsi_dxi(jl, xi, eta) + &
                              element%jacobian(1, 1, xi, eta) * &
                              element%dpsi_deta(jl, xi, eta)) / detJ

                    ! 行列要素の計算
                    val = val + (dNdx_i * dNdx_j + dNdy_i * dNdy_j) * lambda(iG) * weight * detJ
                end do
                ! 全体行列へのアセンブル
                call A%find(element%get_connectivity(il), element%get_connectivity(jl), index)
                A%val(index) = A%val(index) + val
            end do
        end do

    end subroutine process_single_element_diffusion

    subroutine process_single_element_thermal(CT, KT, element, temperature, porosity, propeties)
        implicit none
        ! --- 引数 ---
        type(type_crs), intent(inout) :: CT, KT
        class(abst_element), pointer, intent(inout) :: element
        type(type_variable), intent(in) :: temperature
        type(type_variable), intent(in) :: porosity
        type(type_properties_manager), intent(inout) :: propeties

        ! --- ローカル変数 ---
        integer(int32) :: index, num_nodes, num_gauss, i_material, il, jl, iG
        real(real64) :: val_mass, val_diffusion, xi, eta, weight, detJ
        real(real64) :: dNdx_i, dNdy_i, dNdx_j, dNdy_j

        ! 熱伝導率と熱容量を事前に補間する配列
        integer(int32), parameter :: max_gauss = 10
        type(type_state) :: state(max_gauss)
        real(real64) :: Ca(max_gauss), lambda(max_gauss)

        num_nodes = element%get_num_nodes()
        i_material = element%get_group()
        num_gauss = element%get_num_gauss()

        ! 積分点での状態配列を事前に構築
        !$omp simd private(state)
        do iG = 1, num_gauss
            xi = element%gauss(1, iG)
            eta = element%gauss(2, iG)
            state(iG)%pressure = 101325.0d0
            state(iG)%water_content = 0.0d0
            state(iG)%temperature = element%interpolate(xi, eta, temperature%pre)
            state(iG)%porosity = element%interpolate(xi, eta, porosity%pre)
        end do

        ! 状態配列を渡して、熱容量と熱伝導率の配列を一括計算
        call propeties%calc_thermal(state(1:num_gauss), i_material, thc=lambda(1:num_gauss), vhc=Ca(1:num_gauss))

        ! 要素行列の計算とアセンブル
        do il = 1, num_nodes
            do jl = 1, num_nodes
                val_mass = 0.0d0
                val_diffusion = 0.0d0

                ! 積分ループ
                !$omp simd reduction(+:val_mass, val_diffusion)
                do iG = 1, num_gauss
                    xi = element%gauss(1, iG)
                    eta = element%gauss(2, iG)
                    weight = element%weight(iG)
                    detJ = element%jacobian_det(xi, eta)

                    ! 質量行列の計算
                    val_mass = val_mass + (element%psi(il, xi, eta) * &
                                           element%psi(jl, xi, eta) * &
                                           detJ * weight * Ca(iG))

                    ! 拡散行列の計算に必要な形状関数の勾配
                    dNdx_i = (element%jacobian(2, 2, xi, eta) * &
                              element%dpsi_dxi(il, xi, eta) - &
                              element%jacobian(2, 1, xi, eta) * &
                              element%dpsi_deta(il, xi, eta)) / detJ
                    dNdy_i = (-element%jacobian(1, 2, xi, eta) * &
                              element%dpsi_dxi(il, xi, eta) + &
                              element%jacobian(1, 1, xi, eta) * &
                              element%dpsi_deta(il, xi, eta)) / detJ
                    dNdx_j = (element%jacobian(2, 2, xi, eta) * &
                              element%dpsi_dxi(jl, xi, eta) - &
                              element%jacobian(2, 1, xi, eta) * &
                              element%dpsi_deta(jl, xi, eta)) / detJ
                    dNdy_j = (-element%jacobian(1, 2, xi, eta) * &
                              element%dpsi_dxi(jl, xi, eta) + &
                              element%jacobian(1, 1, xi, eta) * &
                              element%dpsi_deta(jl, xi, eta)) / detJ

                    val_diffusion = val_diffusion + (dNdx_i * dNdx_j + dNdy_i * dNdy_j) * lambda(iG) * weight * detJ
                end do

                ! 全体行列へのアセンブル
                call CT%find(element%get_connectivity(il), element%get_connectivity(jl), index)

                CT%val(index) = CT%val(index) + val_mass
                KT%val(index) = KT%val(index) + val_diffusion
            end do
        end do
    end subroutine process_single_element_thermal

    subroutine process_element_thermal_linear_1(J, R, element, temperature, porosity, propeties, time, actual_order)
        implicit none
        type(type_crs), intent(inout) :: J
        real(real64), intent(inout) :: R(:)
        class(abst_element), intent(in), pointer :: element
        type(type_variable), intent(in) :: temperature
        type(type_variable), intent(in) :: porosity
        type(type_properties_manager), intent(inout) :: propeties
        type(type_time), intent(in) :: time
        integer(int32), intent(in) :: actual_order

        ! --- ローカル変数 (変数名・宣言方法をあなたのコードに合わせる) ---
        integer(int32) :: index, num_nodes, num_gauss, i_material, il, jl, iG, iO
        real(real64) :: xi, eta, weight, detJ
        real(real64) :: dNdx_i, dNdy_i, dNdx_j, dNdy_j
        real(real64) :: val

        ! --- スタック上のワークスペース ---
        real(real64) :: CT_e(element%get_num_nodes(), element%get_num_nodes())
        real(real64) :: KT_e(element%get_num_nodes(), element%get_num_nodes())
        real(real64) :: J_e(element%get_num_nodes(), element%get_num_nodes())
        real(real64) :: R_e(element%get_num_nodes())
        real(real64) :: T_old_e(element%get_num_nodes(), actual_order)
        real(real64) :: T_hist_e(element%get_num_nodes())

        ! --- ガウスポイントでの物理量 ---
        type(type_state) :: state(element%get_num_gauss())
        real(real64) :: Ca(element%get_num_gauss()), lambda(element%get_num_gauss())

        ! --- 時間関連 ---
        real(real64) :: dt_n
        real(real64) :: coefficients(0:actual_order)

        ! --- 初期化 ---
        num_nodes = element%get_num_nodes()
        i_material = element%get_group()
        num_gauss = element%get_num_gauss()

        CT_e(:, :) = 0.0d0
        KT_e(:, :) = 0.0d0

        ! ==========================================================================
        ! STEP 1: 全ガウスポイントの物理量を一括計算
        ! ==========================================================================
        !$omp simd
        do iG = 1, num_gauss
            xi = element%gauss(1, iG)
            eta = element%gauss(2, iG)
            state(iG)%temperature = element%interpolate(xi, eta, temperature%pre(:))
            state(iG)%porosity = element%interpolate(xi, eta, porosity%pre(:))
        end do
        call propeties%calc_thermal(state(1:num_gauss), i_material, lambda(1:num_gauss), Ca(1:num_gauss))

        ! ==========================================================================
        ! STEP 2: 1回の積分ループで、CT_e と KT_e を計算
        ! ==========================================================================
        do iG = 1, num_gauss
            xi = element%gauss(1, iG)
            eta = element%gauss(2, iG)
            weight = element%weight(iG)
            detJ = element%jacobian_det(xi, eta)
            do il = 1, num_nodes
                dNdx_i = (element%jacobian(2, 2, xi, eta) * &
                          element%dpsi_dxi(il, xi, eta) - &
                          element%jacobian(2, 1, xi, eta) * &
                          element%dpsi_deta(il, xi, eta)) / detJ
                dNdy_i = (-element%jacobian(1, 2, xi, eta) * &
                          element%dpsi_dxi(il, xi, eta) + &
                          element%jacobian(1, 1, xi, eta) * &
                          element%dpsi_deta(il, xi, eta)) / detJ
                !$omp simd
                do jl = 1, num_nodes
                    dNdx_j = (element%jacobian(2, 2, xi, eta) * &
                              element%dpsi_dxi(jl, xi, eta) - &
                              element%jacobian(2, 1, xi, eta) * &
                              element%dpsi_deta(jl, xi, eta)) / detJ
                    dNdy_j = (-element%jacobian(1, 2, xi, eta) * &
                              element%dpsi_dxi(jl, xi, eta) + &
                              element%jacobian(1, 1, xi, eta) * &
                              element%dpsi_deta(jl, xi, eta)) / detJ
                    CT_e(il, jl) = CT_e(il, jl) + element%psi(il, xi, eta) * element%psi(jl, xi, eta) * Ca(iG) * weight * detJ
                    KT_e(il, jl) = KT_e(il, jl) + (dNdx_i * dNdx_j + dNdy_i * dNdy_j) * lambda(iG) * weight * detJ
                end do
            end do
        end do

        dt_n = time%dt
        call time%get_time_coefficients(actual_order, coefficients)

        ! ==========================================================================
        ! STEP 3: 計算済みのCT_e, KT_eを使い、最終的な LHS(J_e) と RHS(R_e) を構築
        ! ==========================================================================
        ! --- 3a. LHS行列 J_e (=A_e) の構築 ---
        J_e(:, :) = dt_n * KT_e(:, :) + coefficients(0) * CT_e(:, :)

        ! --- 3b. RHSベクトル R_e (=b_e) の構築 (PHIT相当部分, SIMD最適化) ---
        R_e(:) = 0.0d0
        !$omp simd collapse(2)
        do il = 1, num_nodes
            do iO = 1, actual_order
                T_old_e(il, iO) = temperature%old(element%get_connectivity(il), iO)
            end do
        end do

        T_hist_e(:) = 0.0d0
        !$omp simd
        do il = 1, num_nodes
            do iO = 1, actual_order
                T_hist_e(il) = T_hist_e(il) + coefficients(iO) * T_old_e(il, iO)
            end do
        end do

        !$omp simd
        do il = 1, num_nodes
            val = 0.0d0
            do jl = 1, num_nodes
                val = val + CT_e(il, jl) * T_hist_e(jl)
            end do
            R_e(il) = -val
        end do

        ! ==========================================================================
        ! STEP 4: 全体行列・ベクトルへの直接アセンブル
        ! ==========================================================================
        !$omp simd
        do il = 1, num_nodes
            R(element%get_connectivity(il)) = R(element%get_connectivity(il)) + R_e(il)
            do jl = 1, num_nodes
                call J%find(element%get_connectivity(il), element%get_connectivity(jl), index)
                J%val(index) = J%val(index) + J_e(il, jl)
            end do
        end do

    end subroutine process_element_thermal_linear_1

    subroutine assemble_mass_heat_1(A, domain, temperature, porosity, propeties)
        implicit none
        type(type_crs), intent(inout) :: A
        type(type_domain), intent(inout), target :: domain
        real(real64), intent(in) :: temperature(:)
        real(real64), intent(in) :: porosity(:)
        type(type_properties_manager), intent(inout) :: propeties

        class(abst_element), pointer :: element

        integer(int32) :: iE
        integer(int32) :: num_elements
        num_elements = domain%get_num_elements()

        do iE = 1, num_elements
            element => domain%Elements(iE)%e
            call process_single_element_mass(A, element, temperature, porosity, propeties)
        end do

    end subroutine assemble_mass_heat_1

    ! ==============================================================================
    ! Subroutine: Assemble_Mass_Heat_1_parallel
    ! Purpose:
    !   カラーリングの結果を用いて行列アセンブルを並列化する
    ! ==============================================================================
    subroutine assemble_mass_heat_1_parallel(A, domain, temperature, porosity, propeties)
        implicit none
        type(type_crs), intent(inout) :: A
        type(type_domain), intent(inout), target :: domain
        real(real64), intent(in) :: temperature(:)
        real(real64), intent(in) :: porosity(:)
        type(type_properties_manager), intent(inout) :: propeties

        integer(int32) :: c, ie_idx
        class(abst_element), pointer :: element

        !$omp parallel private(c, ie_idx, element) shared(domain, A, temperature, porosity, propeties)
        do c = 1, domain%colors%num_colors
            !$omp do
            do ie_idx = 1, domain%colors%colored(c)%num_elements
                element => domain%Elements(domain%colors%colored(c)%Elements(ie_idx))%e
                call process_single_element_mass(A, element, temperature, porosity, propeties)
            end do
            !$omp end do
        end do
        !$omp end parallel

    end subroutine assemble_mass_heat_1_parallel

    subroutine assemble_diffusion_heat_1(A, domain, temperature, porosity, propeties)
        implicit none
        ! --- 引数 ---
        type(type_crs), intent(inout) :: A
        type(type_domain), intent(inout), target :: domain
        real(real64), intent(in) :: temperature(:)
        real(real64), intent(in) :: porosity(:)
        type(type_properties_manager), intent(inout) :: propeties

        ! --- ローカル変数 ---
        class(abst_element), pointer :: element
        integer(int32) :: iE
        integer(int32) :: num_elements

        num_elements = domain%get_num_elements()
        do iE = 1, num_elements
            element => domain%Elements(iE)%e
            call process_single_element_diffusion(A, element, temperature, porosity, propeties)
        end do

    end subroutine assemble_diffusion_heat_1

    subroutine assemble_diffusion_heat_1_parallel(A, domain, temperature, porosity, propeties)
        implicit none
        type(type_crs), intent(inout) :: A
        type(type_domain), intent(inout), target :: domain
        real(real64), intent(in) :: temperature(:)
        real(real64), intent(in) :: porosity(:)
        type(type_properties_manager), intent(inout) :: propeties

        integer(int32) :: c, ie_idx
        class(abst_element), pointer :: element

        !$omp parallel private(c, ie_idx, element) shared(domain, A, temperature, porosity, propeties)
        do c = 1, domain%colors%num_colors
            !$omp do
            do ie_idx = 1, domain%colors%colored(c)%num_elements
                element => domain%Elements(domain%colors%colored(c)%Elements(ie_idx))%e
                call process_single_element_diffusion(A, element, temperature, porosity, propeties)
            end do
            !$omp end do
        end do
        !$omp end parallel
    end subroutine assemble_diffusion_heat_1_parallel

    subroutine thermal_assemble_system_linear_1(J, R, domain, temperature, porosity, propeties, time, actual_order)
        implicit none
        type(type_crs), intent(inout) :: J
        real(real64), intent(inout) :: R(:)
        type(type_domain), intent(inout), target :: domain
        type(type_variable), intent(in) :: temperature
        type(type_variable), intent(in) :: porosity
        type(type_properties_manager), intent(inout) :: propeties
        type(type_time), intent(in) :: time
        integer(int32), intent(in) :: actual_order

        class(abst_element), pointer :: element
        integer(int32) :: iE, num_elements

        num_elements = domain%get_num_elements()

        do iE = 1, num_elements
            element => domain%Elements(iE)%e
            call process_element_thermal_linear_1(J, R, element, temperature, porosity, propeties, time, actual_order)
        end do
    end subroutine thermal_assemble_system_linear_1

    subroutine thermal_assemble_system_linear_1_parallel(J, R, domain, temperature, porosity, propeties, time, actual_order)
        implicit none
        type(type_crs), intent(inout) :: J
        real(real64), intent(inout) :: R(:)
        type(type_domain), intent(inout), target :: domain
        type(type_variable), intent(in) :: temperature
        type(type_variable), intent(in) :: porosity
        type(type_properties_manager), intent(inout) :: propeties
        type(type_time), intent(in) :: time
        integer(int32), intent(in) :: actual_order

        integer(int32) :: c, ie_idx
        class(abst_element), pointer :: element

        !$omp parallel private(c, ie_idx, element, workspace) shared(domain, CT, KT, temperature, porosity, propeties)
        do c = 1, domain%colors%num_colors
            !$omp do
            do ie_idx = 1, domain%colors%colored(c)%num_elements
                element => domain%Elements(domain%colors%colored(c)%Elements(ie_idx))%e
                call process_element_thermal_linear_1(J, R, element, temperature, porosity, propeties, time, actual_order)
            end do
            !$omp end do
        end do
        !$omp end parallel
    end subroutine thermal_assemble_system_linear_1_parallel

end module thermal_thermal_assemble
