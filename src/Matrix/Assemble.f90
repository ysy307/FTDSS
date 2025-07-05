module Matrix_Assemble
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Core_BaseTypes, only:GaussPointState_t
    use :: Properties_Model_Base, only:Proereties_Model_t
    use :: Matrix_CRS
    use :: Domain_Module, only:Domain_t
    use :: Matrix_RCM, only:Reorder_to_Original
#ifdef _OPENMP
    use omp_lib
#endif
    implicit none
    private

    public :: Assemble_Mass_Heat_1, Assemble_Diffusion_Heat_1
    public :: Assemble_Mass_Heat_1_Parallel, Assemble_Diffusion_Heat_1_Parallel

contains
    subroutine Assemble_Mass_Heat_1(A, Domain, Temperature, Porosity, Propeties)
        implicit none
        type(Type_CRS), intent(inout) :: A
        type(Domain_t) :: Domain
        real(real64), intent(in) :: Temperature(:)
        real(real64), intent(in) :: Porosity(:)
        type(Proereties_Model_t), intent(inout) :: Propeties

        type(GaussPointState_t) :: State

        integer(int32) :: index, nNodes, nGauss
        integer(int32) :: iE, il, jl, iG, iRegion
        integer(int32) :: r_il, r_jl
        integer(int32) :: istat
        real(real64) :: val
        real(real64) :: xi, eta, weight, detJ
        real(real64) :: Ca

        real(real64), allocatable :: Original_Temperature(:)
        real(real64), allocatable :: Original_Porosity(:)

        integer(int32) :: nElements

        allocate (Original_Temperature, mold=Temperature)
        allocate (Original_Porosity, mold=Porosity)

        call Reorder_to_Original(Temperature, Original_Temperature, Domain%RCM_perm, istat)
        call Reorder_to_Original(Porosity, Original_Porosity, Domain%RCM_perm, istat)

        State%porosity = 0.0d0
        State%temperature = 0.0d0
        State%pressure = 101325.0d0
        State%water_content = 0.0d0

        nElements = Domain%get_numElement()
        do iE = 1, nElements
            nNodes = Domain%Elements(iE)%e%get_size()
            iRegion = Domain%Elements(iE)%e%get_group()
            do il = 1, nNodes
                do jl = 1, nNodes
                    val = 0.0d0
                    nGauss = Domain%Elements(iE)%e%nGauss
                    r_il = Domain%RCM_inv_perm(Domain%Elements(iE)%e%conn(il))
                    r_jl = Domain%RCM_inv_perm(Domain%Elements(iE)%e%conn(jl))
                    do iG = 1, nGauss
                        xi = Domain%Elements(iE)%e%gauss(1, iG)
                        eta = Domain%Elements(iE)%e%gauss(2, iG)
                        weight = Domain%Elements(iE)%e%weight(iG)
                        detJ = Domain%Elements(iE)%e%Jac_Det(xi, eta)

                        State%temperature = Domain%Elements(iE)%e%Interpolate(xi, eta, Original_Temperature)
                        State%porosity = Domain%Elements(iE)%e%Interpolate(xi, eta, Original_Porosity)
                        State%water_content = Propeties%get_Qw(State, iRegion)

                        Ca = Propeties%get_Ca(State, iRegion)

                        val = val + (Domain%Elements(iE)%e%psi(il, xi, eta) * &
                                     Domain%Elements(iE)%e%psi(jl, xi, eta) * &
                                     detJ * weight * Ca)
                    end do
                    call A%Find(r_il, r_jl, index)
                    A%Val(index) = A%Val(index) + val
                end do
            end do
        end do

        deallocate (Original_Temperature)
        deallocate (Original_Porosity)

        ! stop
    end subroutine Assemble_Mass_Heat_1

    ! ==============================================================================
! Subroutine: Assemble_Mass_Heat_1_Parallel
! Purpose:
!   カラーリングの結果を用いて行列アセンブルを並列化する
! ==============================================================================
    subroutine Assemble_Mass_Heat_1_Parallel(A, Domain, Temperature, Porosity, Propeties)
        implicit none
        type(Type_CRS), intent(inout) :: A
        type(Domain_t), intent(inout) :: Domain ! Domain%Colors を使うため inout
        real(real64), intent(in) :: Temperature(:)
        real(real64), intent(in) :: Porosity(:)
        type(Proereties_Model_t), intent(inout) :: Propeties

        ! ループ変数など
        integer(int32) :: c, ie_idx

        ! 元の変数宣言 (プライベート変数として各スレッドで持つ)
        type(GaussPointState_t) :: State
        integer(int32) :: index, nNodes, nGauss
        integer(int32) :: iE, il, jl, iG, iRegion
        integer(int32) :: r_il, r_jl
        integer(int32) :: istat
        real(real64) :: val
        real(real64) :: xi, eta, weight, detJ
        real(real64) :: Ca

        ! 共有変数 (全スレッドで共有して読み込む)
        real(real64), allocatable :: Original_Temperature(:)
        real(real64), allocatable :: Original_Porosity(:)

        allocate (Original_Temperature, mold=Temperature)
        allocate (Original_Porosity, mold=Porosity)

        call Reorder_to_Original(Temperature, Original_Temperature, Domain%RCM_perm, istat)
        call Reorder_to_Original(Porosity, Original_Porosity, Domain%RCM_perm, istat)

        !================================================!
        ! 並列アセンブル処理
        !================================================!
        ! 色のループ (ここは直列)
        do c = 1, Domain%Colors%nColor

            ! 同じ色に属する要素のループを並列化する
            !$OMP PARALLEL DO &
            !$OMP PRIVATE(ie_idx, iE, nNodes, iRegion, il, jl, r_il, r_jl, val, nGauss, iG, xi, eta, weight, detJ, State, Ca, index, istat) &
            !$OMP SHARED(Domain, A, Original_Temperature, Original_Porosity, Propeties)
            do ie_idx = 1, Domain%Colors%Colored(c)%numElements

                ! グローバルな要素番号を取得
                iE = Domain%Colors%Colored(c)%Elements(ie_idx)

                !
                ! --- ↓↓↓ 以下は元のコードのループ内部とほぼ同じ ---
                !

                State%pressure = 101325.0d0
                State%water_content = 0.0d0

                nNodes = Domain%Elements(iE)%e%get_size()
                iRegion = Domain%Elements(iE)%e%get_group()
                do il = 1, nNodes
                    do jl = 1, nNodes
                        val = 0.0d0
                        nGauss = Domain%Elements(iE)%e%nGauss
                        r_il = Domain%RCM_inv_perm(Domain%Elements(iE)%e%conn(il))
                        r_jl = Domain%RCM_inv_perm(Domain%Elements(iE)%e%conn(jl))
                        do iG = 1, nGauss
                            xi = Domain%Elements(iE)%e%gauss(1, iG)
                            eta = Domain%Elements(iE)%e%gauss(2, iG)
                            weight = Domain%Elements(iE)%e%weight(iG)
                            detJ = Domain%Elements(iE)%e%Jac_Det(xi, eta)

                            State%temperature = Domain%Elements(iE)%e%Interpolate(xi, eta, Original_Temperature)
                            State%porosity = Domain%Elements(iE)%e%Interpolate(xi, eta, Original_Porosity)
                            State%water_content = Propeties%get_Qw(State, iRegion)

                            Ca = Propeties%get_Ca(State, iRegion)

                            val = val + (Domain%Elements(iE)%e%psi(il, xi, eta) * &
                                         Domain%Elements(iE)%e%psi(jl, xi, eta) * &
                                         detJ * weight * Ca)
                        end do
                        call A%Find(r_il, r_jl, index)
                        ! 同じ色の要素同士は競合しないので、atomic や critical は不要
                        A%Val(index) = A%Val(index) + val
                    end do
                end do
            end do
            !$OMP END PARALLEL DO
        end do
        !================================================!

        deallocate (Original_Temperature)
        deallocate (Original_Porosity)

    end subroutine Assemble_Mass_Heat_1_Parallel

    subroutine Assemble_Diffusion_Heat_1(A, Domain, Temperature, Porosity, Propeties)
        implicit none
        ! --- 引数 ---
        type(Type_CRS), intent(inout) :: A
        type(Domain_t), intent(in) :: Domain
        real(real64), intent(in) :: Temperature(:)
        real(real64), intent(in) :: Porosity(:)
        type(Proereties_Model_t), intent(inout) :: Propeties ! MaterialManagerに相当

        ! --- ローカル変数 ---
        type(GaussPointState_t) :: State ! 状態の運び屋
        integer(int32) :: index, nNodes, nGauss, nElements
        integer(int32) :: iE, il, jl, iG, iRegion
        integer(int32) :: r_il, r_jl
        integer(int32) :: istat
        real(real64) :: val
        real(real64) :: xi, eta, weight, detJ
        real(real64) :: dNdx_i, dNdy_i, dNdx_j, dNdy_j
        real(real64) :: lambda_gp ! Gauss Pointでの熱伝導率

        real(real64), allocatable :: Original_Temperature(:)
        real(real64), allocatable :: Original_Porosity(:)

        nElements = Domain%get_numElement()

        allocate (Original_Temperature, mold=Temperature)
        allocate (Original_Porosity, mold=Porosity)

        call Reorder_to_Original(Temperature, Original_Temperature, Domain%RCM_perm, istat)
        call Reorder_to_Original(Porosity, Original_Porosity, Domain%RCM_perm, istat)

        State%porosity = 0.0d0
        State%temperature = 0.0d0
        State%pressure = 101325.0d0
        State%water_content = 0.0d0

        do iE = 1, nElements

            ! 節点数取得
            nNodes = Domain%Elements(iE)%e%get_size()
            iRegion = Domain%Elements(iE)%e%get_group()
            do il = 1, nNodes
                do jl = 1, nNodes
                    val = 0.0d0

                    r_il = Domain%RCM_inv_perm(Domain%Elements(iE)%e%conn(il))
                    r_jl = Domain%RCM_inv_perm(Domain%Elements(iE)%e%conn(jl))
                    do iG = 1, Domain%Elements(iE)%e%nGauss
                        xi = Domain%Elements(iE)%e%gauss(1, iG)
                        eta = Domain%Elements(iE)%e%gauss(2, iG)
                        weight = Domain%Elements(iE)%e%weight(iG)

                        ! ヤコビアン行列式
                        detJ = Domain%Elements(iE)%e%Jac_Det(xi, eta)

                        ! 形状関数勾配（x,y方向）
                        dNdx_i = (Domain%Elements(iE)%e%Jac(2, 2, xi, eta) * &
                                  Domain%Elements(iE)%e%dpsi_dxi(il, xi, eta) - &
                                  Domain%Elements(iE)%e%Jac(2, 1, xi, eta) * &
                                  Domain%Elements(iE)%e%dpsi_deta(il, xi, eta) &
                                  ) / detJ
                        dNdy_i = (-Domain%Elements(iE)%e%Jac(1, 2, xi, eta) * &
                                  Domain%Elements(iE)%e%dpsi_dxi(il, xi, eta) + &
                                  Domain%Elements(iE)%e%Jac(1, 1, xi, eta) * &
                                  Domain%Elements(iE)%e%dpsi_deta(il, xi, eta) &
                                  ) / detJ
                        dNdx_j = (Domain%Elements(iE)%e%Jac(2, 2, xi, eta) * &
                                  Domain%Elements(iE)%e%dpsi_dxi(jl, xi, eta) - &
                                  Domain%Elements(iE)%e%Jac(2, 1, xi, eta) * &
                                  Domain%Elements(iE)%e%dpsi_deta(jl, xi, eta) &
                                  ) / detJ
                        dNdy_j = (-Domain%Elements(iE)%e%Jac(1, 2, xi, eta) * &
                                  Domain%Elements(iE)%e%dpsi_dxi(jl, xi, eta) + &
                                  Domain%Elements(iE)%e%Jac(1, 1, xi, eta) * &
                                  Domain%Elements(iE)%e%dpsi_deta(jl, xi, eta) &
                                  ) / detJ

                        ! (1) このガウス点での「状態」を計算する (質量行列と同じ)
                        State%temperature = Domain%Elements(iE)%e%Interpolate(xi, eta, Original_Temperature)
                        State%porosity = Domain%Elements(iE)%e%Interpolate(xi, eta, Original_Porosity)
                        State%water_content = Propeties%get_Qw(State, iRegion) ! 必要なら

                        ! (2) その「状態」を使って、このガウス点での熱伝導率を取得する
                        lambda_gp = Propeties%get_lambda(State, iRegion)

                        val = val + (dNdx_i * dNdx_j + dNdy_i * dNdy_j) * lambda_gp * weight * detJ
                    end do
                    call A%Find(r_il, r_jl, index)
                    A%Val(index) = A%Val(index) + val
                end do
            end do
        end do

        deallocate (Original_Temperature)
        deallocate (Original_Porosity)

    end subroutine Assemble_Diffusion_Heat_1

    !==============================================================================
! Subroutine: Assemble_Diffusion_Heat_1_Parallel
! Purpose:
!   カラーリングの結果を用いて行列アセンブルを並列化する
!==============================================================================
    subroutine Assemble_Diffusion_Heat_1_Parallel(A, Domain, Temperature, Porosity, Propeties)
        implicit none
        type(Type_CRS), intent(inout) :: A
        type(Domain_t), intent(inout) :: Domain ! Domain%Colors を使うため inout
        real(real64), intent(in) :: Temperature(:)
        real(real64), intent(in) :: Porosity(:)
        type(Proereties_Model_t), intent(inout) :: Propeties ! MaterialManagerに相当

        ! ループ変数など
        integer(int32) :: c, ie_idx
        ! 元の変数宣言 (プライベート変数として各スレッドで持つ)
        type(GaussPointState_t) :: State ! 状態の運び屋
        integer(int32) :: index, nNodes, nGauss
        integer(int32) :: iE, il, jl, iG, iRegion
        integer(int32) :: r_il, r_jl
        integer(int32) :: istat
        real(real64) :: val
        real(real64) :: xi, eta, weight, detJ
        real(real64) :: dNdx_i, dNdy_i, dNdx_j, dNdy_j
        real(real64) :: lambda_gp ! Gauss Pointでの熱伝導率

        ! 共有変数 (全スレッドで共有して読み込む)
        real(real64), allocatable :: Original_Temperature(:)
        real(real64), allocatable :: Original_Porosity(:)

        allocate (Original_Temperature, mold=Temperature)
        allocate (Original_Porosity, mold=Porosity)
        call Reorder_to_Original(Temperature, Original_Temperature, Domain%RCM_perm, istat)
        call Reorder_to_Original(Porosity, Original_Porosity, Domain%RCM_perm, istat)
        !================================================!
        ! 並列アセンブル処理
        !================================================!
        ! 色のループ (ここは直列)
        do c = 1, Domain%Colors%nColor
            ! 同じ色に属する要素のループを並列化する
            !$OMP PARALLEL DO &
            !$OMP PRIVATE(ie_idx, iE, nNodes, iRegion, il, jl, r_il, r_jl, val, nGauss, iG, xi, eta, weight, detJ, dNdx_i, dNdy_i, dNdx_j, dNdy_j, State, lambda_gp, index) &
            !$OMP SHARED(Domain, A, Original_Temperature, Original_Porosity, Propeties)
            do ie_idx = 1, Domain%Colors%Colored(c)%numElements

                ! グローバルな要素番号を取得
                iE = Domain%Colors%Colored(c)%Elements(ie_idx)

                State%pressure = 101325.0d0
                State%water_content = 0.0d0

                nNodes = Domain%Elements(iE)%e%get_size()
                iRegion = Domain%Elements(iE)%e%get_group()
                do il = 1, nNodes
                    do jl = 1, nNodes
                        val = 0.0d0

                        r_il = Domain%RCM_inv_perm(Domain%Elements(iE)%e%conn(il))
                        r_jl = Domain%RCM_inv_perm(Domain%Elements(iE)%e%conn(jl))
                        do iG = 1, Domain%Elements(iE)%e%nGauss
                            xi = Domain%Elements(iE)%e%gauss(1, iG)
                            eta = Domain%Elements(iE)%e%gauss(2, iG)
                            weight = Domain%Elements(iE)%e%weight(iG)

                            ! ヤコビアン行列式
                            detJ = Domain%Elements(iE)%e%Jac_Det(xi, eta)

                            ! 形状関数勾配（x,y方向）
                            dNdx_i = (Domain%Elements(iE)%e%Jac(2, 2, xi, eta) * &
                                      Domain%Elements(iE)%e%dpsi_dxi(il, xi, eta) - &
                                      Domain%Elements(iE)%e%Jac(2, 1, xi, eta) * &
                                      Domain%Elements(iE)%e%dpsi_deta(il, xi, eta) &
                                      ) / detJ
                            dNdy_i = (-Domain%Elements(iE)%e%Jac(1, 2, xi, eta) * &
                                      Domain%Elements(iE)%e%dpsi_dxi(il, xi, eta) + &
                                      Domain%Elements(iE)%e%Jac(1, 1, xi, eta) * &
                                      Domain%Elements(iE)%e%dpsi_deta(il, xi, eta) &
                                      ) / detJ
                            dNdx_j = (Domain%Elements(iE)%e%Jac(2, 2, xi, eta) * &
                                      Domain%Elements(iE)%e%dpsi_dxi(jl, xi, eta) - &
                                      Domain%Elements(iE)%e%Jac(2, 1, xi, eta) * &
                                      Domain%Elements(iE)%e%dpsi_deta(jl, xi, eta) &
                                      ) / detJ
                            dNdy_j = (-Domain%Elements(iE)%e%Jac(1, 2, xi, eta) * &
                                      Domain%Elements(iE)%e%dpsi_dxi(jl, xi, eta) + &
                                      Domain%Elements(iE)%e%Jac(1, 1, xi, eta) * &
                                      Domain%Elements(iE)%e%dpsi_deta(jl, xi, eta) &
                                      ) / detJ
                            ! (1) このガウス点での「状態」を計算する (質量行列と同じ)
                            State%temperature = Domain%Elements(iE)%e%Interpolate(xi, eta, Original_Temperature)
                            State%porosity = Domain%Elements(iE)%e%Interpolate(xi, eta, Original_Porosity)
                            State%water_content = Propeties%get_Qw(State, iRegion) ! 必要なら
                            ! (2) その「状態」を使って、このガウス点での熱伝導率を取得する
                            lambda_gp = Propeties%get_lambda(State, iRegion)
                            val = val + (dNdx_i * dNdx_j + dNdy_i * dNdy_j) * lambda_gp * weight * detJ
                        end do
                        call A%Find(r_il, r_jl, index)
                        ! 同じ色の要素同士は競合しないので、atomic や critical は不要
                        A%Val(index) = A%Val(index) + val
                    end do
                end do
            end do
            !$OMP END PARALLEL DO
        end do
        !================================================!
        deallocate (Original_Temperature)
        deallocate (Original_Porosity)
    end subroutine Assemble_Diffusion_Heat_1_Parallel
end module Matrix_Assemble
