module thermal_thermal_assemble
    use, intrinsic :: iso_fortran_env, only: int32, real64
!$  use omp_lib
    use :: module_core, only:type_gauss_point_state
    use :: module_domain, only:type_domain, abst_element
    use :: module_properties, only:type_proereties_manager
    use :: module_matrix, only:type_crs

    implicit none
    private

    public :: Assemble_Mass_Heat_1, Assemble_Diffusion_Heat_1
    public :: Assemble_Mass_Heat_1_Parallel, Assemble_Diffusion_Heat_1_Parallel

contains

    subroutine process_single_element_mass(A, element, temperature, porosity, propeties)
        implicit none
        ! --- 引数 ---
        type(Type_CRS), intent(inout) :: A
        class(abst_element), pointer, intent(inout) :: element
        real(real64), intent(in) :: temperature(:)
        real(real64), intent(in) :: porosity(:)
        type(type_proereties_manager), intent(inout) :: propeties

        ! --- ローカル変数 ---
        integer(int32) :: index, num_nodes, num_gauss, i_material, il, jl, iG
        real(real64) :: val, xi, eta, weight, detJ, Ca
        type(type_gauss_point_state) :: state

        ! 並列版のコードに合わせて、事前補間用の配列をローカルに用意
        integer(int32), parameter :: MaxGauss = 10
        real(real64) :: interp_temp(MaxGauss), interp_poro(MaxGauss)

        state%pressure = 101325.0d0
        state%water_content = 0.0d0

        num_nodes = element%get_num_nodes()
        i_material = element%get_group()
        num_gauss = element%get_num_gauss()

        ! 積分点での物理量を事前に補間
        do iG = 1, num_gauss
            xi = element%gauss(1, iG)
            eta = element%gauss(2, iG)
            interp_temp(iG) = element%interpolate(xi, eta, temperature)
            interp_poro(iG) = element%interpolate(xi, eta, porosity)
        end do

        ! 要素行列の計算とアセンブル
        do il = 1, num_nodes
            do jl = 1, num_nodes
                val = 0.0d0

                ! 積分ループ
                do iG = 1, num_gauss
                    xi = element%gauss(1, iG)
                    eta = element%gauss(2, iG)
                    weight = element%weight(iG)
                    detJ = element%jacobian_det(xi, eta)

                    state%temperature = interp_temp(iG)
                    state%porosity = interp_poro(iG)
                    state%water_content = propeties%get_qw(state, i_material)
                    Ca = propeties%get_vhc(state, i_material)

                    val = val + (element%psi(il, xi, eta) * &
                                 element%psi(jl, xi, eta) * &
                                 detJ * weight * Ca)
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
        type(Type_CRS), intent(inout) :: A
        class(abst_element), pointer, intent(inout) :: element
        real(real64), intent(in) :: temperature(:)
        real(real64), intent(in) :: porosity(:)
        type(type_proereties_manager), intent(inout) :: propeties

        ! --- ローカル変数 ---
        integer(int32) :: index, num_nodes, num_gauss, i_material, il, jl, iG, global_il, global_jl
        real(real64) :: val, xi, eta, weight, detJ
        real(real64) :: dNdx_i, dNdy_i, dNdx_j, dNdy_j
        real(real64) :: lambda_gp
        type(type_gauss_point_state) :: state

        ! 並列版のコードに合わせて、事前補間用の配列をローカルに用意
        integer(int32), parameter :: MaxGauss = 10
        real(real64) :: interp_temp(MaxGauss), interp_poro(MaxGauss)

        state%pressure = 101325.0d0
        state%water_content = 0.0d0

        num_nodes = element%get_num_nodes()
        i_material = element%get_group()
        num_gauss = element%get_num_gauss()

        ! 積分点での物理量を事前に補間
        do iG = 1, num_gauss
            xi = element%gauss(1, iG)
            eta = element%gauss(2, iG)
            interp_temp(iG) = element%interpolate(xi, eta, temperature)
            interp_poro(iG) = element%interpolate(xi, eta, porosity)
        end do

        ! 要素行列の計算とアセンブル
        do il = 1, num_nodes
            do jl = 1, num_nodes
                val = 0.0d0

                ! 積分ループ
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
                    ! 状態の計算
                    state%temperature = interp_temp(iG)
                    state%porosity = interp_poro(iG)
                    state%water_content = propeties%get_qw(state, i_material)
                    ! 熱伝導率の取得
                    lambda_gp = propeties%get_thc(state, i_material)
                    ! 行列要素の計算
                    val = val + (dNdx_i * dNdx_j + dNdy_i * dNdy_j) * lambda_gp * weight * detJ
                end do
                ! 全体行列へのアセンブル
                call A%find(element%get_connectivity(il), element%get_connectivity(jl), index)
                A%val(index) = A%val(index) + val
            end do
        end do

    end subroutine process_single_element_diffusion

    subroutine Assemble_Mass_Heat_1(A, domain, temperature, porosity, propeties)
        implicit none
        type(Type_CRS), intent(inout) :: A
        type(type_domain), intent(inout), target :: domain
        real(real64), intent(in) :: temperature(:)
        real(real64), intent(in) :: porosity(:)
        type(type_proereties_manager), intent(inout) :: propeties

        class(abst_element), pointer :: element

        integer(int32) :: iE
        integer(int32) :: num_elements
        num_elements = domain%get_num_elements()

        do iE = 1, num_elements
            element => domain%Elements(iE)%e
            call process_single_element_mass(A, element, temperature, porosity, propeties)
        end do

    end subroutine Assemble_Mass_Heat_1

    ! ==============================================================================
    ! Subroutine: Assemble_Mass_Heat_1_Parallel
    ! Purpose:
    !   カラーリングの結果を用いて行列アセンブルを並列化する
    ! ==============================================================================
    subroutine Assemble_Mass_Heat_1_Parallel(A, domain, temperature, porosity, propeties)
        implicit none
        type(Type_CRS), intent(inout) :: A
        type(type_domain), intent(inout), target :: domain
        real(real64), intent(in) :: temperature(:)
        real(real64), intent(in) :: porosity(:)
        type(type_proereties_manager), intent(inout) :: propeties

        integer(int32) :: c, ie_idx
        class(abst_element), pointer :: element

        do c = 1, domain%colors%num_colors
            !$omp parallel do private(ie_idx, element) shared(domain, A, temperature, porosity, propeties)
            do ie_idx = 1, domain%colors%Colored(c)%num_elements
                element => domain%Elements(domain%colors%Colored(c)%Elements(ie_idx))%e
                call process_single_element_mass(A, element, temperature, porosity, propeties)
            end do
            !$omp end parallel do
        end do

    end subroutine Assemble_Mass_Heat_1_Parallel

    subroutine Assemble_Diffusion_Heat_1(A, domain, temperature, porosity, propeties)
        implicit none
        ! --- 引数 ---
        type(Type_CRS), intent(inout) :: A
        type(type_domain), intent(inout), target :: domain
        real(real64), intent(in) :: temperature(:)
        real(real64), intent(in) :: porosity(:)
        type(type_proereties_manager), intent(inout) :: propeties ! MaterialManagerに相当

        ! --- ローカル変数 ---
        class(abst_element), pointer :: element
        integer(int32) :: iE
        integer(int32) :: num_elements

        num_elements = domain%get_num_elements()
        do iE = 1, num_elements
            element => domain%Elements(iE)%e
            call process_single_element_diffusion(A, element, temperature, porosity, propeties)
        end do

    end subroutine Assemble_Diffusion_Heat_1

    subroutine Assemble_Diffusion_Heat_1_Parallel(A, domain, temperature, porosity, propeties)
        implicit none
        type(Type_CRS), intent(inout) :: A
        type(type_domain), intent(inout), target :: domain
        real(real64), intent(in) :: temperature(:)
        real(real64), intent(in) :: porosity(:)
        type(type_proereties_manager), intent(inout) :: propeties

        integer(int32) :: c, ie_idx
        class(abst_element), pointer :: element

        do c = 1, domain%colors%num_colors
            !$omp parallel do private(ie_idx, element) shared(domain, A, temperature, porosity, propeties)
            do ie_idx = 1, domain%colors%Colored(c)%num_elements
                element => domain%Elements(domain%colors%Colored(c)%Elements(ie_idx))%e
                call process_single_element_diffusion(A, element, temperature, porosity, propeties)
            end do
            !$omp end parallel do
        end do
    end subroutine Assemble_Diffusion_Heat_1_Parallel

end module thermal_thermal_assemble
