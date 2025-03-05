program test_HCF
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Calculate_HCF
    implicit none
    real(real64), allocatable :: head(:)
    integer(int32) :: i

    real(real64) :: thetaS_BC, thetaR_BC, alpha1_BC, n1_BC, Ks_BC, l_BC
    real(real64) :: thetaS_vG, thetaR_vG, alpha1_vG, n1_vG, Ks_vG, l_vG
    real(real64) :: thetaS_KO, thetaR_KO, alpha1_KO, n1_KO, Ks_KO, l_KO
    real(real64) :: thetaS_MVG, thetaR_MVG, alpha1_MVG, n1_MVG, theatM_MVG, Ks_MVG, l_MVG, hcrit_MVG
    real(real64) :: thetaS_Durner, thetaR_Durner, alpha1_Durner, n1_Durner, w1_Durner, alpha2_Durner, n2_Durner, Ks_Durner, l_Durner
    real(real64) :: thetaS_DVGCH, thetaR_DVGCH, alpha1_DVGCH, n1_DVGCH, w1_DVGCH, alpha2_DVGCH, n2_DVGCH, Ks_DVGCH, l_DVGCH
    real(real64) :: Omega
    integer(int32) :: nsize, useViscosityType

    integer(int32) :: case_num

    class(Abstract_HCF), allocatable :: HCF

    ! BC
    thetaS_BC = 0.3d0
    thetaR_BC = 0.0d0
    alpha1_BC = -2.558d0
    n1_BC = 0.57087d0
    Ks_BC = 1.96d-7
    l_BC = 0.5d0

    ! vG
    thetaS_vG = 0.3d0
    thetaR_vG = 0.0d0
    alpha1_vG = 0.2d0
    n1_vG = 1.8d0
    Ks_vG = 1.96d-7
    l_vG = 0.5d0

    ! KO
    thetaS_KO = 0.3d0
    thetaR_KO = 0.0d0
    alpha1_KO = -11.473d0
    n1_KO = 1.3685d0
    Ks_KO = 1.96d-7
    l_KO = 0.5d0

    ! MVG
    thetaS_MVG = 0.3d0
    thetaR_MVG = 0.0d0
    alpha1_MVG = 0.2d0
    n1_MVG = 1.8d0
    theatM_MVG = 0.0d0
    Ks_MVG = 1.96d-7
    l_MVG = 0.5d0
    hcrit_MVG = -1.0d0

    ! Durner
    thetaS_Durner = 0.39971d0
    thetaR_Durner = 0.00671d0
    alpha1_Durner = 0.04034d2
    n1_Durner = 8.46152d0
    w1_Durner = 0.72352d0
    alpha2_Durner = 0.04034d2
    n2_Durner = 1.30984d0
    Ks_Durner = 3.754d-2
    l_Durner = 0.5d0

    ! DVGCH
    thetaS_DVGCH = 0.39971d0
    thetaR_DVGCH = 0.00671d0
    alpha1_DVGCH = 0.04034d2
    n1_DVGCH = 8.46152d0
    w1_DVGCH = 0.72352d0
    n2_DVGCH = 1.30984d0
    Ks_DVGCH = 3.754d-2
    l_DVGCH = 0.5d0

    useViscosityType = 2
    nsize = 330
    Omega = 10.0d0
    allocate (head(nsize))

    do i = 1, nsize
        head(i) = -10.0d0**((i - 1) / 40.0d0 - 2.0d0)
    end do

    case_num = 7

    select case (case_num)
    case (1)
        HCF = Type_HCF(useHCFType=11, ks=Ks_BC, alpha1=alpha1_BC, n1=n1_BC, l=l_BC, nsize=nsize)
    case (2)
        HCF = Type_HCF(usehcftype=12, ks=Ks_vG, alpha1=alpha1_vG, n1=n1_vG, l=l_vG, nsize=nsize)
    case (3)
        HCF = Type_HCF(usehcftype=13, ks=Ks_KO, alpha1=alpha1_KO, n1=n1_KO, l=l_KO, nsize=nsize)
    case (4)
        HCF = Type_HCF(usehcftype=14, ks=Ks_MVG, thetaS=thetaS_MVG, thetaR=thetaR_MVG, alpha1=alpha1_MVG, n1=n1_MVG, l=l_MVG, hcrit=hcrit_MVG, nsize=nsize)
    case (5)
        HCF = Type_HCF(usehcftype=15, ks=Ks_Durner, alpha1=alpha1_Durner, n1=n1_Durner, w1=w1_Durner, alpha2=alpha2_Durner, n2=n2_Durner, l=l_Durner, nsize=nsize)
    case (6)
        HCF = Type_HCF(usehcftype=16, ks=Ks_DVGCH, alpha1=alpha1_DVGCH, n1=n1_DVGCH, w1=w1_DVGCH, n2=n2_DVGCH, l=l_DVGCH, nsize=nsize)
    case (7)
        HCF = Type_HCF(usehcftype=21, Ks=Ks_vG, Omega=Omega, nsize=nsize)
    case (8)
        HCF = Type_HCF(usehcftype=31, Ks=Ks_vG, useViscosity=useViscosityType, nsize=nsize)
    end select

    select type (h => HCF)
    type is (Type_HCF_Base_BC)
        print *, "h K"
        do i = 1, nsize
            print *, head(i), h%Calculate_Kflh(head(i))
        end do
    type is (Type_HCF_Base_VG)
        print *, "h K"
        do i = 1, nsize
            print *, head(i), h%Calculate_Kflh(head(i))
        end do
    type is (Type_HCF_Base_KO)
        print *, "h K"
        do i = 1, nsize
            print *, head(i), h%Calculate_Kflh(head(i))
        end do
    type is (Type_HCF_Base_MVG)
        print *, "h K"
        do i = 1, nsize
            print *, head(i), h%Calculate_Kflh(head(i))
        end do
    type is (Type_HCF_Base_Durner)
        print *, "h K"
        do i = 1, nsize
            print *, head(i), h%Calculate_Kflh(head(i))
        end do
    type is (Type_HCF_Base_DVGCH)
        print *, "h K"
        do i = 1, nsize
            print *, head(i), h%Calculate_Kflh(head(i))
        end do
    type is (Type_HCF_Impedance)
        print *, "Qice K"
        do i = 1, 301
            print *, (dble(i) - 1.d0) / 1000.d0, h%Calculate_Kflh((dble(i) - 1.d0) / 1000.d0)
        end do
    type is (Type_HCF_Viscosity)
        print *, "T K mu"
        do i = -300, 200
            print *, dble(i) / 10.d0, h%Calculate_Kflh(dble(i) / 10.d0), h%Calculate_Viscosity(dble(i) / 10.d0)
        end do

    end select
end program test_HCF
