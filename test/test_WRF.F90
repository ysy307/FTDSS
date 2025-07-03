program test_WRF
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Calculate_WRF
    implicit none
    real(real64) :: head(330)
    integer(int32) :: i

    real(real64) :: thetaS_BC, thetaR_BC, alpha1_BC, n1_BC
    real(real64) :: thetaS_vG, thetaR_vG, alpha1_vG, n1_vG
    real(real64) :: thetaS_KO, thetaR_KO, alpha1_KO, n1_KO
    real(real64) :: thetaS_MVG, thetaR_MVG, alpha1_MVG, n1_MVG, theatM_MVG
    real(real64) :: thetaS_Durner, thetaR_Durner, alpha1_Durner, n1_Durner, w1_Durner, alpha2_Durner, n2_Durner
    real(real64) :: thetaS_DVGCH, thetaR_DVGCH, alpha1_DVGCH, n1_DVGCH, w1_DVGCH, alpha2_DVGCH, n2_DVGCH

    integer(int32) :: case_num

    class(Abstract_WRF), allocatable :: WRF

    ! BC
    thetaS_BC = 0.3d0
    thetaR_BC = 0.0d0
    alpha1_BC = -2.558d0
    n1_BC = 0.57087d0

    ! vG
    thetaS_vG = 0.3d0
    thetaR_vG = 0.0d0
    alpha1_vG = 0.2d0
    n1_vG = 1.8d0

    ! KO
    thetaS_KO = 0.3d0
    thetaR_KO = 0.0d0
    alpha1_KO = -11.473d0
    n1_KO = 1.3685d0

    ! MVG
    thetaS_MVG = 0.3d0
    thetaR_MVG = 0.0d0
    alpha1_MVG = 0.2d0
    n1_MVG = 1.8d0
    theatM_MVG = 0.0d0

    ! Durner
    thetaS_Durner = 0.39971d0
    thetaR_Durner = 0.00671d0
    alpha1_Durner = 0.04034d0
    n1_Durner = 8.46152d0
    w1_Durner = 0.72352d0
    alpha2_Durner = 0.04034d0
    n2_Durner = 1.30984d0

    ! DVGCH
    thetaS_DVGCH = 0.39971d0
    thetaR_DVGCH = 0.00671d0
    alpha1_DVGCH = 0.04034d0
    n1_DVGCH = 8.46152d0
    w1_DVGCH = 0.72352d0
    n2_DVGCH = 1.30984d0

    case_num = 6

    do i = 1, 330
        head(i) = -10.0d0**((i - 1) / 40.0d0 - 2.0d0)
    end do
    select case (case_num)
    case (1)
        allocate (Type_WRF_BC :: WRF)
    case (2)
        allocate (Type_WRF_VG :: WRF)
    case (3)
        allocate (Type_WRF_KO :: WRF)
    case (4)
        allocate (Type_WRF_MVG :: WRF)
    case (5)
        allocate (Type_WRF_Durner :: WRF)
    case (6)
        allocate (Type_WRF_DVGCH :: WRF)
    end select

    select type (w => WRF)
    type is (Type_WRF_BC)
        w%thetaS = thetaS_BC
        w%thetaR = thetaR_BC
        w%alpha1 = alpha1_BC
        w%n1 = n1_BC
    type is (Type_WRF_VG)
        w%thetaS = thetaS_vG
        w%thetaR = thetaR_vG
        w%alpha1 = alpha1_vG
        w%n1 = n1_vG
        w%m1 = 1.0d0 - 1.0d0 / n1_vG
    type is (Type_WRF_KO)
        w%thetaS = thetaS_KO
        w%thetaR = thetaR_KO
        w%alpha1 = alpha1_KO
        w%n1 = n1_KO
    type is (Type_WRF_MVG)
        w%thetaS = thetaS_MVG
        w%thetaR = thetaR_MVG
        w%alpha1 = alpha1_MVG
        w%n1 = n1_MVG
        w%m1 = 1.0d0 - 1.0d0 / n1_MVG
        w%hcrit = theatM_MVG
    type is (Type_WRF_Durner)
        w%thetaS = thetaS_Durner
        w%thetaR = thetaR_Durner
        w%alpha1 = alpha1_Durner
        w%n1 = n1_Durner
        w%m1 = 1.0d0 - 1.0d0 / n1_Durner
        w%w1 = w1_Durner
        w%alpha2 = alpha2_Durner
        w%n2 = n2_Durner
        w%m2 = 1.0d0 - 1.0d0 / n2_Durner
        w%w2 = 1.0d0 - w1_Durner
    type is (Type_WRF_DVGCH)
        w%thetaS = thetaS_DVGCH
        w%thetaR = thetaR_DVGCH
        w%alpha1 = alpha1_DVGCH
        w%n1 = n1_DVGCH
        w%m1 = 1.0d0 - 1.0d0 / n1_DVGCH
        w%w1 = w1_DVGCH
        w%n2 = n2_DVGCH
        w%m2 = 1.0d0 - 1.0d0 / n2_DVGCH
        w%w2 = 1.0d0 - w1_DVGCH
    end select

    print *, "h Q Cw"
    do i = 1, 330
        print *, head(i), WRF%Calculate_WRF(head(i)), WRF%Calculate_WRF_Derivative(head(i))
    end do

end program test_WRF
