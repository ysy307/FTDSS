module thermal_thermal_assemble
    use, intrinsic :: iso_fortran_env, only: int32, real64
!$  use omp_lib
    use :: module_core, only:type_gauss_point_state
    use :: module_domain, only:type_domain
    use :: module_properties, only:type_proereties_manager
    use :: Matrix_CRS

    implicit none
    private

!$  real(real64) :: time_total = 0.0d0
!$  real(real64) :: time_interp = 0.0d0
!$  real(real64) :: time_get_prop = 0.0d0
!$  real(real64) :: time_integration = 0.0d0
!$  real(real64) :: time_find_index = 0.0d0
!$  real(real64) :: time_add_val = 0.0d0

!$  public :: time_total, time_interp, time_get_prop, time_integration, time_find_index, time_add_val

    public :: Assemble_Mass_Heat_1, Assemble_Diffusion_Heat_1
    public :: Assemble_Mass_Heat_1_Parallel, Assemble_Diffusion_Heat_1_Parallel

contains
    subroutine Assemble_Mass_Heat_1(A, domain, temperature, porosity, Propeties)
        implicit none
        type(Type_CRS), intent(inout) :: A
        type(type_domain) :: domain
        real(real64), intent(in) :: temperature(:)
        real(real64), intent(in) :: porosity(:)
        type(type_proereties_manager), intent(inout) :: Propeties

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
                        state%water_content = Propeties%get_qw(state, iRegion)

                        Ca = Propeties%get_vhc(state, iRegion)

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
        type(type_proereties_manager), intent(inout) :: Propeties

        ! ループ・局所変数
        integer(int32) :: c, ie_idx
        integer(int32) :: index, nNodes, nGauss
        integer(int32) :: iE, il, jl, iG, iRegion
        integer(int32) :: r_il, r_jl
        integer(int32) :: istat
        real(real64) :: val, xi, eta, weight, detJ, Ca

        ! 時間測定用
!$      real(real64) :: t_start, t_end
!$      real(real64) :: t_total, t_interp, t_get_prop, t_integration, t_find_index, t_add_val

        ! 各スレッド専用
        type(type_gauss_point_state) :: state
        integer, parameter :: MaxGauss = 10
        real(real64) :: interp_temp(MaxGauss)
        real(real64) :: interp_poro(MaxGauss)

        ! 共有変数
        real(real64), allocatable :: Original_temperature(:)
        real(real64), allocatable :: Original_porosity(:)

        ! 初期化
!$      t_total = 0.0d0
!$      t_interp = 0.0d0
!$      t_get_prop = 0.0d0
!$      t_integration = 0.0d0
!$      t_find_index = 0.0d0
!$      t_add_val = 0.0d0

        allocate (Original_temperature, mold=temperature)
        allocate (Original_porosity, mold=porosity)

        call domain%rcm%reorder_to_original(temperature, Original_temperature)
        call domain%rcm%reorder_to_original(porosity, Original_porosity)

        ! 色ごとにループ（逐次）
        do c = 1, domain%Colors%num_colors

            !$OMP PARALLEL DO PRIVATE(ie_idx, iE, nNodes, iRegion, il, jl, r_il, r_jl, val, nGauss, iG, xi, eta, weight, detJ, state, Ca, index, istat, interp_temp, interp_poro, t_start, t_end) &
            !$OMP SHARED(domain, A, Original_temperature, Original_porosity, Propeties) &
            !$OMP REDUCTION(+:t_total, t_interp, t_get_prop, t_integration, t_find_index, t_add_val)
            do ie_idx = 1, domain%Colors%Colored(c)%num_elements
!$              t_start = omp_get_wtime()

                iE = domain%Colors%Colored(c)%Elements(ie_idx)

                state%pressure = 101325.0d0
                state%water_content = 0.0d0

                nNodes = domain%Elements(iE)%e%get_num_nodes()
                iRegion = domain%Elements(iE)%e%get_group()
                nGauss = domain%Elements(iE)%e%get_num_gauss()

!$              t_end = omp_get_wtime()
!$              t_total = t_total + (t_end - t_start)

                ! --- 補間 ---
!$              t_start = omp_get_wtime()
                do iG = 1, nGauss
                    xi = domain%Elements(iE)%e%gauss(1, iG)
                    eta = domain%Elements(iE)%e%gauss(2, iG)
                    interp_temp(iG) = domain%Elements(iE)%e%Interpolate(xi, eta, Original_temperature)
                    interp_poro(iG) = domain%Elements(iE)%e%Interpolate(xi, eta, Original_porosity)
                end do
!$              t_end = omp_get_wtime()
!$              t_interp = t_interp + (t_end - t_start)

                ! --- 要素行列計算 ---
                do il = 1, nNodes
                    do jl = 1, nNodes
                        val = 0.0d0
                        call domain%rcm%reorder_to_rcm(domain%Elements(iE)%e%connectivity(il), r_il)
                        call domain%rcm%reorder_to_rcm(domain%Elements(iE)%e%connectivity(jl), r_jl)

                        ! 積分ループ
!$                      t_start = omp_get_wtime()
                        do iG = 1, nGauss
                            xi = domain%Elements(iE)%e%gauss(1, iG)
                            eta = domain%Elements(iE)%e%gauss(2, iG)
                            weight = domain%Elements(iE)%e%weight(iG)
                            detJ = domain%Elements(iE)%e%jacobian_det(xi, eta)

!$                          t_end = omp_get_wtime()
!$                          t_integration = t_integration + (t_end - t_start)

                            ! --- プロパティ取得 ---
!$                          t_start = omp_get_wtime()
                            state%temperature = interp_temp(iG)
                            state%porosity = interp_poro(iG)
                            state%water_content = Propeties%get_qw(state, iRegion)
                            Ca = Propeties%get_vhc(state, iRegion)
!$                          t_end = omp_get_wtime()
!$                          t_get_prop = t_get_prop + (t_end - t_start)

                            val = val + (domain%Elements(iE)%e%psi(il, xi, eta) * &
                                         domain%Elements(iE)%e%psi(jl, xi, eta) * &
                                         detJ * weight * Ca)
!$                          t_start = omp_get_wtime()
                        end do

                        ! --- インデックス探索と値加算 ---
!$                      t_start = omp_get_wtime()
                        call A%Find(r_il, r_jl, index)
!$                      t_end = omp_get_wtime()
!$                      t_find_index = t_find_index + (t_end - t_start)

!$                      t_start = omp_get_wtime()
                        A%Val(index) = A%Val(index) + val
!$                      t_end = omp_get_wtime()
!$                      t_add_val = t_add_val + (t_end - t_start)
                    end do
                end do
            end do
            !$OMP END PARALLEL DO
        end do

        ! モジュールのグローバル時間変数に反映
!$      time_total = time_total + t_total
!$      time_interp = time_interp + t_interp
!$      time_get_prop = time_get_prop + t_get_prop
!$      time_integration = time_integration + t_integration
!$      time_find_index = time_find_index + t_find_index
!$      time_add_val = time_add_val + t_add_val

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
        type(type_proereties_manager), intent(inout) :: Propeties ! MaterialManagerに相当

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
                        state%water_content = Propeties%get_qw(state, iRegion) ! 必要なら

                        ! (2) その「状態」を使って、このガウス点での熱伝導率を取得する
                        lambda_gp = Propeties%get_thc(state, iRegion)

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

    subroutine Assemble_Diffusion_Heat_1_Parallel(A, domain, temperature, porosity, Propeties)
        implicit none

        type(Type_CRS), intent(inout) :: A
        type(type_domain), intent(inout) :: domain
        real(real64), intent(in) :: temperature(:)
        real(real64), intent(in) :: porosity(:)
        type(type_proereties_manager), intent(inout) :: Propeties

        integer(int32) :: c, ie_idx, index, nNodes, nGauss
        integer(int32) :: iE, il, jl, iG, iRegion
        integer(int32) :: r_il, r_jl
        real(real64) :: val
        real(real64) :: xi, eta, weight, detJ
        real(real64) :: dNdx_i, dNdy_i, dNdx_j, dNdy_j
        real(real64) :: lambda_gp
        type(type_gauss_point_state) :: state
        integer, parameter :: MaxGauss = 10
        real(real64) :: interp_temp(MaxGauss)
        real(real64) :: interp_poro(MaxGauss)

        real(real64), allocatable :: Original_temperature(:)
        real(real64), allocatable :: Original_porosity(:)

        ! --- タイマー関連 ---
!$      real(real64) :: t_start, t_end
!$      real(real64) :: t_total, t_interp, t_get_prop, t_integration, t_find_index, t_add_val
!$      t_total = 0.0d0
!$      t_interp = 0.0d0
!$      t_get_prop = 0.0d0
!$      t_integration = 0.0d0
!$      t_find_index = 0.0d0
!$      t_add_val = 0.0d0

        allocate (Original_temperature, mold=temperature)
        allocate (Original_porosity, mold=porosity)
        call domain%rcm%reorder_to_original(temperature, Original_temperature)
        call domain%rcm%reorder_to_original(porosity, Original_porosity)

        do c = 1, domain%Colors%num_colors
            !$OMP PARALLEL DO PRIVATE(ie_idx, iE, nNodes, nGauss, iRegion, il, jl, r_il, r_jl, val, iG, xi, eta, weight, detJ, dNdx_i, dNdy_i, dNdx_j, dNdy_j, state, lambda_gp, index, interp_temp, interp_poro, t_start, t_end) &
            !$OMP SHARED(domain, A, Original_temperature, Original_porosity, Propeties) &
            !$OMP REDUCTION(+:t_total, t_interp, t_get_prop, t_integration, t_find_index, t_add_val)
            do ie_idx = 1, domain%Colors%Colored(c)%num_elements
!$              t_start = omp_get_wtime()

                iE = domain%Colors%Colored(c)%Elements(ie_idx)
                nNodes = domain%Elements(iE)%e%get_num_nodes()
                nGauss = domain%Elements(iE)%e%get_num_gauss()
                iRegion = domain%Elements(iE)%e%get_group()

                state%pressure = 101325.0d0
                state%water_content = 0.0d0

!$              t_end = omp_get_wtime()
!$              t_total = t_total + (t_end - t_start)

                ! --- 補間 ---
!$              t_start = omp_get_wtime()
                do iG = 1, nGauss
                    xi = domain%Elements(iE)%e%gauss(1, iG)
                    eta = domain%Elements(iE)%e%gauss(2, iG)
                    interp_temp(iG) = domain%Elements(iE)%e%Interpolate(xi, eta, Original_temperature)
                    interp_poro(iG) = domain%Elements(iE)%e%Interpolate(xi, eta, Original_porosity)
                end do
!$              t_end = omp_get_wtime()
!$              t_interp = t_interp + (t_end - t_start)

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

                            ! --- 形状関数微分 ---
                            dNdx_i = (domain%Elements(iE)%e%jacobian(2, 2, xi, eta) * domain%Elements(iE)%e%dpsi_dxi(il, xi, eta) - &
                                      domain%Elements(iE)%e%jacobian(2, 1, xi, eta) * domain%Elements(iE)%e%dpsi_deta(il, xi, eta)) / detJ
                            dNdy_i = (-domain%Elements(iE)%e%jacobian(1, 2, xi, eta) * domain%Elements(iE)%e%dpsi_dxi(il, xi, eta) + &
                                      domain%Elements(iE)%e%jacobian(1, 1, xi, eta) * domain%Elements(iE)%e%dpsi_deta(il, xi, eta)) / detJ

                            dNdx_j = (domain%Elements(iE)%e%jacobian(2, 2, xi, eta) * domain%Elements(iE)%e%dpsi_dxi(jl, xi, eta) - &
                                      domain%Elements(iE)%e%jacobian(2, 1, xi, eta) * domain%Elements(iE)%e%dpsi_deta(jl, xi, eta)) / detJ
                            dNdy_j = (-domain%Elements(iE)%e%jacobian(1, 2, xi, eta) * domain%Elements(iE)%e%dpsi_dxi(jl, xi, eta) + &
                                      domain%Elements(iE)%e%jacobian(1, 1, xi, eta) * domain%Elements(iE)%e%dpsi_deta(jl, xi, eta)) / detJ

                            ! --- プロパティ取得 ---
!$                          t_start = omp_get_wtime()
                            state%temperature = interp_temp(iG)
                            state%porosity = interp_poro(iG)
                            state%water_content = Propeties%get_qw(state, iRegion)
                            lambda_gp = Propeties%get_thc(state, iRegion)
!$                          t_end = omp_get_wtime()
!$                          t_get_prop = t_get_prop + (t_end - t_start)

                            ! --- 積分 ---
!$                          t_start = omp_get_wtime()
                            val = val + (dNdx_i * dNdx_j + dNdy_i * dNdy_j) * lambda_gp * weight * detJ
!$                          t_end = omp_get_wtime()
!$                          t_integration = t_integration + (t_end - t_start)
                        end do

                        ! --- 探索 ---
!$                      t_start = omp_get_wtime()
                        call A%Find(r_il, r_jl, index)
!$                      t_end = omp_get_wtime()
!$                      t_find_index = t_find_index + (t_end - t_start)

                        ! --- 値加算 ---
!$                      t_start = omp_get_wtime()
                        A%Val(index) = A%Val(index) + val
!$                      t_end = omp_get_wtime()
!$                      t_add_val = t_add_val + (t_end - t_start)
                    end do
                end do
            end do
            !$OMP END PARALLEL DO
        end do

        ! モジュール変数へ反映
!$      time_total = time_total + t_total
!$      time_interp = time_interp + t_interp
!$      time_get_prop = time_get_prop + t_get_prop
!$      time_integration = time_integration + t_integration
!$      time_find_index = time_find_index + t_find_index
!$      time_add_val = time_add_val + t_add_val

        deallocate (Original_temperature)
        deallocate (Original_porosity)
    end subroutine Assemble_Diffusion_Heat_1_Parallel

end module thermal_thermal_assemble
