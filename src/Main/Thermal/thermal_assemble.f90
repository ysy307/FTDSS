module thermal_thermal_assemble
    use, intrinsic :: iso_fortran_env, only: int32, real64
!$  use omp_lib
    use :: module_core, only:type_gauss_point_state
    use :: module_domain, only:type_domain
    use :: Properties_Model_Base, only:Proereties_Model_t
    use :: Matrix_CRS

    implicit none
    private

    public :: Assemble_Mass_Heat_1, Assemble_Diffusion_Heat_1
    public :: Assemble_Mass_Heat_1_Parallel, Assemble_Diffusion_Heat_1_Parallel

contains
    subroutine Assemble_Mass_Heat_1(A, domain, temperature, porosity, Propeties)
        implicit none
        type(Type_CRS), intent(inout) :: A
        type(type_domain) :: domain
        real(real64), intent(in) :: temperature(:)
        real(real64), intent(in) :: porosity(:)
        type(Proereties_Model_t), intent(inout) :: Propeties

        type(type_gauss_point_state) :: state

        integer(int32) :: index, nNodes, nGauss
        integer(int32) :: iE, il, jl, iG, iRegion
        integer(int32) :: r_il, r_jl
        integer(int32) :: istat
        real(real64) :: val
        real(real64) :: xi, eta, weight, detJ
        real(real64) :: Ca

        real(real64), allocatable :: Original_temperature(:)
        real(real64), allocatable :: Original_porosity(:)

        integer(int32) :: nElements

        allocate (Original_temperature, mold=temperature)
        allocate (Original_porosity, mold=porosity)

        call domain%rcm%reorder_to_original(temperature, Original_temperature)
        call domain%rcm%reorder_to_original(porosity, Original_porosity)
        ! call Reorder_to_Original(temperature, Original_temperature, domain%RCM_perm, istat)
        ! call Reorder_to_Original(porosity, Original_porosity, domain%RCM_perm, istat)

        state%porosity = 0.0d0
        state%temperature = 0.0d0
        state%pressure = 101325.0d0
        state%water_content = 0.0d0

        nElements = domain%get_num_elements()
        do iE = 1, nElements
            nNodes = domain%Elements(iE)%e%get_num_nodes()
            iRegion = domain%Elements(iE)%e%get_group()
            do il = 1, nNodes
                do jl = 1, nNodes
                    val = 0.0d0
                    nGauss = domain%Elements(iE)%e%get_num_gauss()
                    call domain%rcm%reorder_to_rcm(domain%Elements(iE)%e%connectivity(il), r_il)
                    call domain%rcm%reorder_to_rcm(domain%Elements(iE)%e%connectivity(jl), r_jl)
                    do iG = 1, nGauss
                        xi = domain%Elements(iE)%e%gauss(1, iG)
                        eta = domain%Elements(iE)%e%gauss(2, iG)
                        weight = domain%Elements(iE)%e%weight(iG)
                        detJ = domain%Elements(iE)%e%jacobian_det(xi, eta)

                        state%temperature = domain%Elements(iE)%e%Interpolate(xi, eta, Original_temperature)
                        state%porosity = domain%Elements(iE)%e%Interpolate(xi, eta, Original_porosity)
                        state%water_content = Propeties%get_Qw(state, iRegion)

                        Ca = Propeties%get_Ca(state, iRegion)

                        val = val + (domain%Elements(iE)%e%psi(il, xi, eta) * &
                                     domain%Elements(iE)%e%psi(jl, xi, eta) * &
                                     detJ * weight * Ca)
                    end do
                    call A%Find(r_il, r_jl, index)
                    A%Val(index) = A%Val(index) + val
                end do
            end do
        end do

        ! print *, A%val(:)

        ! stop
        deallocate (Original_temperature)
        deallocate (Original_porosity)

        ! stop
    end subroutine Assemble_Mass_Heat_1

    ! ==============================================================================
    ! Subroutine: Assemble_Mass_Heat_1_Parallel
    ! Purpose:
    !   カラーリングの結果を用いて行列アセンブルを並列化する
    ! ==============================================================================
    subroutine Assemble_Mass_Heat_1_Parallel(A, domain, temperature, porosity, Propeties)
        implicit none
        type(Type_CRS), intent(inout) :: A
        type(type_domain), intent(inout) :: domain
        real(real64), intent(in) :: temperature(:)
        real(real64), intent(in) :: porosity(:)
        type(Proereties_Model_t), intent(inout) :: Propeties

        ! ループ・局所変数
        integer(int32) :: c, ie_idx
        integer(int32) :: index, nNodes, nGauss
        integer(int32) :: iE, il, jl, iG, iRegion
        integer(int32) :: r_il, r_jl
        integer(int32) :: istat
        real(real64) :: val, xi, eta, weight, detJ, Ca

        ! 各スレッド専用
        type(type_gauss_point_state) :: state
        integer, parameter :: MaxGauss = 10
        real(real64) :: interp_temp(MaxGauss)
        real(real64) :: interp_poro(MaxGauss)

        ! 共有変数
        real(real64), allocatable :: Original_temperature(:)
        real(real64), allocatable :: Original_porosity(:)

        allocate (Original_temperature, mold=temperature)
        allocate (Original_porosity, mold=porosity)

        call domain%rcm%reorder_to_original(temperature, Original_temperature)
        call domain%rcm%reorder_to_original(porosity, Original_porosity)

        ! 色ごとにループ（逐次）
        do c = 1, domain%Colors%num_colors

            !$OMP PARALLEL DO PRIVATE(ie_idx, iE, nNodes, iRegion, il, jl, r_il, r_jl, val, nGauss, iG, xi, eta, weight, detJ, state, Ca, index, istat, interp_temp, interp_poro) &
            !$OMP SHARED(domain, A, Original_temperature, Original_porosity, Propeties)
            do ie_idx = 1, domain%Colors%Colored(c)%num_elements

                iE = domain%Colors%Colored(c)%Elements(ie_idx)

                state%pressure = 101325.0d0
                state%water_content = 0.0d0

                nNodes = domain%Elements(iE)%e%get_num_nodes()
                iRegion = domain%Elements(iE)%e%get_group()
                nGauss = domain%Elements(iE)%e%get_num_gauss()

                ! 補間前計算（共通）
                do iG = 1, nGauss
                    xi = domain%Elements(iE)%e%gauss(1, iG)
                    eta = domain%Elements(iE)%e%gauss(2, iG)
                    interp_temp(iG) = domain%Elements(iE)%e%Interpolate(xi, eta, Original_temperature)
                    interp_poro(iG) = domain%Elements(iE)%e%Interpolate(xi, eta, Original_porosity)
                end do

                ! 要素行列計算
                do il = 1, nNodes
                    do jl = 1, nNodes
                        val = 0.0d0
                        call domain%rcm%reorder_to_rcm(domain%Elements(iE)%e%connectivity(il), r_il)
                        call domain%rcm%reorder_to_rcm(domain%Elements(iE)%e%connectivity(jl), r_jl)
                        do iG = 1, nGauss
                            xi = domain%Elements(iE)%e%gauss(1, iG)
                            eta = domain%Elements(iE)%e%gauss(2, iG)
                            weight = domain%Elements(iE)%e%weight(iG)
                            detJ = domain%Elements(iE)%e%jacobian_det(xi, eta)

                            state%temperature = interp_temp(iG)
                            state%porosity = interp_poro(iG)
                            state%water_content = Propeties%get_Qw(state, iRegion)

                            Ca = Propeties%get_Ca(state, iRegion)

                            val = val + (domain%Elements(iE)%e%psi(il, xi, eta) * &
                                         domain%Elements(iE)%e%psi(jl, xi, eta) * &
                                         detJ * weight * Ca)
                        end do

                        call A%Find(r_il, r_jl, index)
                        A%Val(index) = A%Val(index) + val
                    end do
                end do
            end do
            !$OMP END PARALLEL DO
        end do

        deallocate (Original_temperature)
        deallocate (Original_porosity)

    end subroutine Assemble_Mass_Heat_1_Parallel

    subroutine Assemble_Diffusion_Heat_1(A, domain, temperature, porosity, Propeties)
        implicit none
        ! --- 引数 ---
        type(Type_CRS), intent(inout) :: A
        type(type_domain), intent(inout) :: domain
        real(real64), intent(in) :: temperature(:)
        real(real64), intent(in) :: porosity(:)
        type(Proereties_Model_t), intent(inout) :: Propeties ! MaterialManagerに相当

        ! --- ローカル変数 ---
        type(type_gauss_point_state) :: state ! 状態の運び屋
        integer(int32) :: index, nNodes, nGauss, nElements
        integer(int32) :: iE, il, jl, iG, iRegion
        integer(int32) :: r_il, r_jl
        integer(int32) :: istat
        real(real64) :: val
        real(real64) :: xi, eta, weight, detJ
        real(real64) :: dNdx_i, dNdy_i, dNdx_j, dNdy_j
        real(real64) :: lambda_gp ! Gauss Pointでの熱伝導率

        real(real64), allocatable :: Original_temperature(:)
        real(real64), allocatable :: Original_porosity(:)

        nElements = domain%get_num_elements()

        allocate (Original_temperature, mold=temperature)
        allocate (Original_porosity, mold=porosity)

        call domain%rcm%reorder_to_original(temperature, Original_temperature)
        call domain%rcm%reorder_to_original(porosity, Original_porosity)
        ! call Reorder_to_Original(temperature, Original_temperature, domain%RCM_perm, istat)
        ! call Reorder_to_Original(porosity, Original_porosity, domain%RCM_perm, istat)

        state%porosity = 0.0d0
        state%temperature = 0.0d0
        state%pressure = 101325.0d0
        state%water_content = 0.0d0

        do iE = 1, nElements

            ! 節点数取得
            nNodes = domain%Elements(iE)%e%get_num_nodes()
            iRegion = domain%Elements(iE)%e%get_group()
            do il = 1, nNodes
                do jl = 1, nNodes
                    val = 0.0d0

                    call domain%rcm%reorder_to_rcm(domain%Elements(iE)%e%connectivity(il), r_il)
                    call domain%rcm%reorder_to_rcm(domain%Elements(iE)%e%connectivity(jl), r_jl)
                    do iG = 1, domain%Elements(iE)%e%get_num_gauss()
                        xi = domain%Elements(iE)%e%gauss(1, iG)
                        eta = domain%Elements(iE)%e%gauss(2, iG)
                        weight = domain%Elements(iE)%e%weight(iG)

                        ! ヤコビアン行列式
                        detJ = domain%Elements(iE)%e%jacobian_det(xi, eta)

                        ! 形状関数勾配（x,y方向）
                        dNdx_i = (domain%Elements(iE)%e%jacobian(2, 2, xi, eta) * &
                                  domain%Elements(iE)%e%dpsi_dxi(il, xi, eta) - &
                                  domain%Elements(iE)%e%jacobian(2, 1, xi, eta) * &
                                  domain%Elements(iE)%e%dpsi_deta(il, xi, eta) &
                                  ) / detJ
                        dNdy_i = (-domain%Elements(iE)%e%jacobian(1, 2, xi, eta) * &
                                  domain%Elements(iE)%e%dpsi_dxi(il, xi, eta) + &
                                  domain%Elements(iE)%e%jacobian(1, 1, xi, eta) * &
                                  domain%Elements(iE)%e%dpsi_deta(il, xi, eta) &
                                  ) / detJ
                        dNdx_j = (domain%Elements(iE)%e%jacobian(2, 2, xi, eta) * &
                                  domain%Elements(iE)%e%dpsi_dxi(jl, xi, eta) - &
                                  domain%Elements(iE)%e%jacobian(2, 1, xi, eta) * &
                                  domain%Elements(iE)%e%dpsi_deta(jl, xi, eta) &
                                  ) / detJ
                        dNdy_j = (-domain%Elements(iE)%e%jacobian(1, 2, xi, eta) * &
                                  domain%Elements(iE)%e%dpsi_dxi(jl, xi, eta) + &
                                  domain%Elements(iE)%e%jacobian(1, 1, xi, eta) * &
                                  domain%Elements(iE)%e%dpsi_deta(jl, xi, eta) &
                                  ) / detJ

                        ! (1) このガウス点での「状態」を計算する (質量行列と同じ)
                        state%temperature = domain%Elements(iE)%e%Interpolate(xi, eta, Original_temperature)
                        state%porosity = domain%Elements(iE)%e%Interpolate(xi, eta, Original_porosity)
                        state%water_content = Propeties%get_Qw(state, iRegion) ! 必要なら

                        ! (2) その「状態」を使って、このガウス点での熱伝導率を取得する
                        lambda_gp = Propeties%get_lambda(state, iRegion)

                        val = val + (dNdx_i * dNdx_j + dNdy_i * dNdy_j) * lambda_gp * weight * detJ
                    end do
                    call A%Find(r_il, r_jl, index)
                    A%Val(index) = A%Val(index) + val
                end do
            end do
        end do

        deallocate (Original_temperature)
        deallocate (Original_porosity)

    end subroutine Assemble_Diffusion_Heat_1

    !==============================================================================
    ! Subroutine: Assemble_Diffusion_Heat_1_Parallel
    ! Purpose:
    !   カラーリングの結果を用いて行列アセンブルを並列化する
    !==============================================================================
    subroutine Assemble_Diffusion_Heat_1_Parallel(A, domain, temperature, porosity, Propeties)
        use omp_lib
        implicit none

        type(Type_CRS), intent(inout) :: A
        type(type_domain), intent(inout) :: domain
        real(real64), intent(in) :: temperature(:)
        real(real64), intent(in) :: porosity(:)
        type(Proereties_Model_t), intent(inout) :: Propeties

        ! ループ・局所変数
        integer(int32) :: c, ie_idx
        integer(int32) :: index, nNodes, nGauss
        integer(int32) :: iE, il, jl, iG, iRegion
        integer(int32) :: r_il, r_jl
        integer(int32) :: istat
        real(real64) :: val
        real(real64) :: xi, eta, weight, detJ
        real(real64) :: dNdx_i, dNdy_i, dNdx_j, dNdy_j
        real(real64) :: lambda_gp

        ! スレッドローカル
        type(type_gauss_point_state) :: state
        integer, parameter :: MaxGauss = 10
        real(real64) :: interp_temp(MaxGauss)
        real(real64) :: interp_poro(MaxGauss)

        ! 共有読み込み用
        real(real64), allocatable :: Original_temperature(:)
        real(real64), allocatable :: Original_porosity(:)

        allocate (Original_temperature, mold=temperature)
        allocate (Original_porosity, mold=porosity)

        ! 元の配列をRCM順に並べ替え
        call domain%rcm%reorder_to_original(temperature, Original_temperature)
        call domain%rcm%reorder_to_original(porosity, Original_porosity)

        ! 色のループ（逐次）
        do c = 1, domain%Colors%num_colors
            !$OMP PARALLEL DO PRIVATE(ie_idx, iE, nNodes, nGauss, iRegion, il, jl, r_il, r_jl, val, iG, xi, eta, weight, detJ, dNdx_i, dNdy_i, dNdx_j, dNdy_j, state, lambda_gp, index, interp_temp, interp_poro)
            do ie_idx = 1, domain%Colors%Colored(c)%num_elements
                iE = domain%Colors%Colored(c)%Elements(ie_idx)
                nNodes = domain%Elements(iE)%e%get_num_nodes()
                nGauss = domain%Elements(iE)%e%get_num_gauss()
                iRegion = domain%Elements(iE)%e%get_group()

                if (nGauss > MaxGauss) then
                    print *, "Error: nGauss > MaxGauss"
                    stop
                end if

                ! 状態の初期値
                state%pressure = 101325.0d0
                state%water_content = 0.0d0

                ! 補間量を前計算
                do iG = 1, nGauss
                    xi = domain%Elements(iE)%e%gauss(1, iG)
                    eta = domain%Elements(iE)%e%gauss(2, iG)
                    interp_temp(iG) = domain%Elements(iE)%e%Interpolate(xi, eta, Original_temperature)
                    interp_poro(iG) = domain%Elements(iE)%e%Interpolate(xi, eta, Original_porosity)
                end do

                do il = 1, nNodes
                    do jl = 1, nNodes
                        val = 0.0d0
                        call domain%rcm%reorder_to_rcm(domain%Elements(iE)%e%connectivity(il), r_il)
                        call domain%rcm%reorder_to_rcm(domain%Elements(iE)%e%connectivity(jl), r_jl)

                        do iG = 1, nGauss
                            xi = domain%Elements(iE)%e%gauss(1, iG)
                            eta = domain%Elements(iE)%e%gauss(2, iG)
                            weight = domain%Elements(iE)%e%weight(iG)
                            detJ = domain%Elements(iE)%e%jacobian_det(xi, eta)

                            ! 形状関数の微分（逆ヤコビアン込み）
                            dNdx_i = (domain%Elements(iE)%e%jacobian(2, 2, xi, eta) * domain%Elements(iE)%e%dpsi_dxi(il, xi, eta) - &
                                      domain%Elements(iE)%e%jacobian(2, 1, xi, eta) * domain%Elements(iE)%e%dpsi_deta(il, xi, eta)) / detJ
                            dNdy_i = (-domain%Elements(iE)%e%jacobian(1, 2, xi, eta) * domain%Elements(iE)%e%dpsi_dxi(il, xi, eta) + &
                                      domain%Elements(iE)%e%jacobian(1, 1, xi, eta) * domain%Elements(iE)%e%dpsi_deta(il, xi, eta)) / detJ

                            dNdx_j = (domain%Elements(iE)%e%jacobian(2, 2, xi, eta) * domain%Elements(iE)%e%dpsi_dxi(jl, xi, eta) - &
                                      domain%Elements(iE)%e%jacobian(2, 1, xi, eta) * domain%Elements(iE)%e%dpsi_deta(jl, xi, eta)) / detJ
                            dNdy_j = (-domain%Elements(iE)%e%jacobian(1, 2, xi, eta) * domain%Elements(iE)%e%dpsi_dxi(jl, xi, eta) + &
                                      domain%Elements(iE)%e%jacobian(1, 1, xi, eta) * domain%Elements(iE)%e%dpsi_deta(jl, xi, eta)) / detJ

                            ! 状態変数の更新
                            state%temperature = interp_temp(iG)
                            state%porosity = interp_poro(iG)
                            state%water_content = Propeties%get_Qw(state, iRegion)

                            ! 熱伝導率取得
                            lambda_gp = Propeties%get_lambda(state, iRegion)

                            ! 積分
                            val = val + (dNdx_i * dNdx_j + dNdy_i * dNdy_j) * lambda_gp * weight * detJ
                        end do

                        call A%Find(r_il, r_jl, index)
                        A%Val(index) = A%Val(index) + val
                    end do
                end do
            end do
            !$OMP END PARALLEL DO
        end do

        deallocate (Original_temperature)
        deallocate (Original_porosity)

    end subroutine Assemble_Diffusion_Heat_1_Parallel

end module thermal_thermal_assemble
