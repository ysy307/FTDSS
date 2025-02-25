program test_HCF
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Calculate_HCF
    implicit none
    real(real64) :: head(330)
    integer(int32) :: i

    real(real64) :: thetaS_BC, thetaR_BC, alpha1_BC, n1_BC, Ks_BC, l_BC
    real(real64) :: thetaS_vG, thetaR_vG, alpha1_vG, n1_vG, Ks_vG, l_vG
    real(real64) :: thetaS_KO, thetaR_KO, alpha1_KO, n1_KO, Ks_KO, l_KO
    real(real64) :: thetaS_MVG, thetaR_MVG, alpha1_MVG, n1_MVG, theatM_MVG, Ks_MVG, l_MVG
    real(real64) :: thetaS_Durner, thetaR_Durner, alpha1_Durner, n1_Durner, w1_Durner, alpha2_Durner, n2_Durner, Ks_Durner, l_Durner
    real(real64) :: thetaS_DVGCH, thetaR_DVGCH, alpha1_DVGCH, n1_DVGCH, w1_DVGCH, alpha2_DVGCH, n2_DVGCH, Ks_DVGCH, l_DVGCH

    integer(int32) :: case_num

    class(Abstract_HCF), allocatable :: HCF

    ! Loamy sand(BC)
    thetaS_BC = 0.437d0
    thetaR_BC = 0.035d0
    alpha1_BC = -0.115d0
    n1_BC = 0.474d0
    Ks_BC = 350.2d0
    l_BC = 0.5d0

    ! Loamy sand(vG)
    thetaS_vG = 0.41d0
    thetaR_vG = 0.057d0
    alpha1_vG = 0.124d0
    n1_vG = 2.28d0
    Ks_vG = 350.2d0
    l_vG = 0.5d0

    ! Loamy sand(KO)
    thetaS_KO = 0.41d0
    thetaR_KO = 0.057d0
    alpha1_KO = -12.4656d0
    n1_KO = 0.9497d0
    Ks_KO = 350.2d0
    l_KO = 0.5d0

    ! Loamy sand(MVG)
    thetaS_MVG = 0.41d0
    thetaR_MVG = 0.057d0
    alpha1_MVG = 0.03d0
    n1_MVG = 1.5d0
    theatM_MVG = 0.0d0
    Ks_MVG = 350.2d0
    l_MVG = 0.5d0

    ! Durner
    thetaS_Durner = 0.39971d0
    thetaR_Durner = 0.00671d0
    alpha1_Durner = 0.04034d0
    n1_Durner = 8.46152d0
    w1_Durner = 0.72352d0
    alpha2_Durner = 0.04034d0
    n2_Durner = 1.30984d0
    Ks_Durner = 3.754d-2 * 86400d0
    l_Durner = 0.5d0

    ! DVGCH
    thetaS_DVGCH = 0.39971d0
    thetaR_DVGCH = 0.00671d0
    alpha1_DVGCH = 0.04034d0
    n1_DVGCH = 8.46152d0
    w1_DVGCH = 0.72352d0
    n2_DVGCH = 1.30984d0
    Ks_DVGCH = 3.754d-2 * 86400d0
    l_DVGCH = 0.5d0

    case_num = 6

    do i = 1, 330
        head(i) = -10.0d0**((i - 1) / 40.0d0 - 2.0d0)
    end do

    case_num = 8

    select case (case_num)
    case (1)
        allocate (Type_HCF_Base_BC :: HCF)
    case (2)
        allocate (Type_HCF_Base_VG :: HCF)
    case (3)
        allocate (Type_HCF_Base_KO :: HCF)
    case (4)
        allocate (Type_HCF_Base_MVG :: HCF)
    case (5)
        allocate (Type_HCF_Base_Durner :: HCF)
    case (6)
        allocate (Type_HCF_Base_DVGCH :: HCF)
    case (7)
        allocate (Type_HCF_Impedance :: HCF)
    case (8)
        allocate (Type_HCF_Viscosity :: HCF)
    end select

    select type (h => HCF)
    type is (Type_HCF_Base_BC)
        h%thetaS = thetaS_BC
        h%thetaR = thetaR_BC
        h%alpha1 = alpha1_BC
        h%n1 = n1_BC
        h%Ks = Ks_BC
        h%l = l_BC

        print *, "h K"
        do i = 1, 330
            print *, head(i), h%Calculate_Kflh(head(i))
        end do
    type is (Type_HCF_Base_VG)
        h%thetaS = thetaS_vG
        h%thetaR = thetaR_vG
        h%alpha1 = alpha1_vG
        h%n1 = n1_vG
        h%m1 = 1.0d0 - 1.0d0 / n1_vG
        h%Ks = Ks_vG
        h%l = l_vG

        print *, "h K"
        do i = 1, 330
            print *, head(i), h%Calculate_Kflh(head(i))
        end do
    type is (Type_HCF_Base_KO)
        h%thetaS = thetaS_KO
        h%thetaR = thetaR_KO
        h%alpha1 = alpha1_KO
        h%n1 = n1_KO
        h%Ks = Ks_KO
        h%l = l_KO

        print *, "h K"
        do i = 1, 330
            print *, head(i), h%Calculate_Kflh(head(i))
        end do
    type is (Type_HCF_Base_MVG)
        h%thetaS = thetaS_MVG
        h%thetaR = thetaR_MVG
        h%alpha1 = alpha1_MVG
        h%n1 = n1_MVG
        h%m1 = 1.0d0 - 1.0d0 / n1_MVG
        h%Ks = Ks_MVG
        h%hcrit = theatM_MVG
        h%l = l_MVG

        print *, "h K"
        do i = 1, 330
            print *, head(i), h%Calculate_Kflh(head(i))
        end do
    type is (Type_HCF_Base_Durner)
        h%thetaS = thetaS_Durner
        h%thetaR = thetaR_Durner
        h%alpha1 = alpha1_Durner
        h%n1 = n1_Durner
        h%m1 = 1.0d0 - 1.0d0 / n1_Durner
        h%w1 = w1_Durner
        h%alpha2 = alpha2_Durner
        h%n2 = n2_Durner
        h%m2 = 1.0d0 - 1.0d0 / n2_Durner
        h%w2 = 1.0d0 - w1_Durner
        h%Ks = Ks_Durner
        h%l = l_Durner

        print *, "h K"
        do i = 1, 330
            print *, head(i), h%Calculate_Kflh(head(i))
        end do
    type is (Type_HCF_Base_DVGCH)

        h%thetaS = thetaS_DVGCH
        h%thetaR = thetaR_DVGCH
        h%alpha1 = alpha1_DVGCH
        h%n1 = n1_DVGCH
        h%m1 = 1.0d0 - 1.0d0 / n1_DVGCH
        h%w1 = w1_DVGCH
        h%n2 = n2_DVGCH
        h%m2 = 1.0d0 - 1.0d0 / n2_DVGCH
        h%w2 = 1.0d0 - w1_DVGCH
        h%Ks = Ks_DVGCH
        h%l = l_DVGCH

        print *, "h K"
        do i = 1, 330
            print *, head(i), h%Calculate_Kflh(head(i))
        end do

    type is (Type_HCF_Impedance)
        h%Ks = 245.d0
        h%Omega = 3.0d0

        print *, "Qice K"
        do i = 1, 101
            print *, (dble(i) - 1.d0) / 100.d0, h%Calculate_Kflh((dble(i) - 1.d0) / 100.d0)
        end do
    type is (Type_HCF_Viscosity)
        h%Ks = 245.d0
        h%Calculate_Viscosity => h%Set_Calculate_Viscosity(1)
        h%kzero = h%Ks * h%Calculate_Viscosity(15.d0)

        print *, "T K mu"
        do i = -300, 200
            print *, dble(i) / 10.d0, h%Calculate_Kflh(dble(i) / 10.d0), h%Calculate_Viscosity(dble(i) / 10.d0)
        end do

    end select
end program test_HCF
