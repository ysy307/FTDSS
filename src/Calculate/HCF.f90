module Calculate_HCF
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Types
    implicit none

    type, extends(HCF_Parameters) :: HCF
        private
        procedure(Calculation_HCF), pointer, nopass :: Calculate_HCF => null()
        procedure(Calculation_HCF_mu), pointer, nopass :: Calculate_HCF_mu => null()
    contains
        procedure :: Calculate_Kflh => Calculate_Ks
    end type HCF

    interface HCF
        module procedure HCF_Constructor
    end interface

    abstract interface
        function Calculation_HCF(Instance_HCF, h) result(kr)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: HCF
            implicit none
            type(HCF), intent(in) :: Instance_HCF
            real(real64), intent(in) :: h
            real(real64) :: kr
        end function Calculation_HCF
    end interface

    abstract interface
        function Calculation_HCF_mu(T) result(mu)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: HCF
            implicit none
            real(real64), intent(in) :: T
            real(real64) :: mu
        end function Calculation_HCF_mu
    end interface

contains
    type(HCF) function HCF_Constructor(in_HCF_Parameters, HCF_Model_id, Kflh_Model_id)
        implicit none
        real(real64), intent(in) :: in_HCF_Parameters(:)
        integer(int32), intent(in) :: HCF_Model_id, Kflh_Model_id
        real(real64), parameter :: Tcrit = 15.0d0

        select case (HCF_Model_id)
        case (1)
            HCF_Constructor%thetaS = in_HCF_Parameters(1)
            HCF_Constructor%thetaR = in_HCF_Parameters(2)
            HCF_Constructor%alpha1 = in_HCF_Parameters(3)
            HCF_Constructor%n1 = in_HCF_Parameters(4)
            HCF_Constructor%Ks = in_HCF_Parameters(5)
            HCF_Constructor%l = in_HCF_Parameters(6)

            HCF_Constructor%Calculate_HCF => Calculate_HCF_BC
        case (2)
            HCF_Constructor%thetaS = in_HCF_Parameters(1)
            HCF_Constructor%thetaR = in_HCF_Parameters(2)
            HCF_Constructor%alpha1 = in_HCF_Parameters(3)
            HCF_Constructor%n1 = in_HCF_Parameters(4)
            HCF_Constructor%Ks = in_HCF_Parameters(5)
            HCF_Constructor%l = in_HCF_Parameters(6)
            HCF_Constructor%m1 = 1.0d0 - 1.0d0 / HCF_Constructor%n1

            HCF_Constructor%Calculate_HCF => Calculate_HCF_VG
        case (3)
            HCF_Constructor%thetaS = in_HCF_Parameters(1)
            HCF_Constructor%thetaR = in_HCF_Parameters(2)
            HCF_Constructor%alpha1 = in_HCF_Parameters(3)
            HCF_Constructor%n1 = in_HCF_Parameters(4)
            HCF_Constructor%Ks = in_HCF_Parameters(5)
            HCF_Constructor%l = in_HCF_Parameters(6)

            HCF_Constructor%Calculate_HCF => Calculate_HCF_KO
        case (4)
            HCF_Constructor%thetaS = in_HCF_Parameters(1)
            HCF_Constructor%thetaR = in_HCF_Parameters(2)
            HCF_Constructor%alpha1 = in_HCF_Parameters(3)
            HCF_Constructor%n1 = in_HCF_Parameters(4)
            HCF_Constructor%hcrit = in_HCF_Parameters(5)
            HCF_Constructor%Ks = in_HCF_Parameters(6)
            HCF_Constructor%l = in_HCF_Parameters(7)
            HCF_Constructor%m1 = 1.0d0 - 1.0d0 / HCF_Constructor%n1

            HCF_Constructor%Calculate_HCF => Calculate_HCF_MVG
        case (5)
            HCF_Constructor%thetaS = in_HCF_Parameters(1)
            HCF_Constructor%thetaR = in_HCF_Parameters(2)
            HCF_Constructor%alpha1 = in_HCF_Parameters(3)
            HCF_Constructor%n1 = in_HCF_Parameters(4)
            HCF_Constructor%alpha2 = in_HCF_Parameters(5)
            HCF_Constructor%n2 = in_HCF_Parameters(6)
            HCF_Constructor%w1 = in_HCF_Parameters(7)
            HCF_Constructor%Ks = in_HCF_Parameters(8)
            HCF_Constructor%l = in_HCF_Parameters(9)

            HCF_Constructor%m1 = 1.0d0 - 1.0d0 / HCF_Constructor%n1
            HCF_Constructor%m2 = 1.0d0 - 1.0d0 / HCF_Constructor%n2
            HCF_Constructor%w2 = 1.0d0 - HCF_Constructor%w1

            HCF_Constructor%Calculate_HCF => Calculate_HCF_Durner
        case (6)
            HCF_Constructor%thetaS = in_HCF_Parameters(1)
            HCF_Constructor%thetaR = in_HCF_Parameters(2)
            HCF_Constructor%alpha1 = in_HCF_Parameters(3)
            HCF_Constructor%n1 = in_HCF_Parameters(4)
            HCF_Constructor%n2 = in_HCF_Parameters(5)
            HCF_Constructor%w1 = in_HCF_Parameters(6)
            HCF_Constructor%Ks = in_HCF_Parameters(7)
            HCF_Constructor%l = in_HCF_Parameters(8)

            HCF_Constructor%m1 = 1.0d0 - 1.0d0 / HCF_Constructor%n1
            HCF_Constructor%m2 = 1.0d0 - 1.0d0 / HCF_Constructor%n2
            HCF_Constructor%w2 = 1.0d0 - HCF_Constructor%w1

            HCF_Constructor%Calculate_HCF => Calculate_HCF_DVGCH
        end select

        select case (Kflh_Model_id)
        case (3, 5)
            HCF_Constructor%Calculate_HCF_mu => Calc_HCF_mu_Exponential
            HCF_Constructor%kzero = HCF_Constructor%Ks / HCF_Constructor%Calculate_HCF_mu(Tcrit)
        case (6, 7)
            HCF_Constructor%Calculate_HCF_mu => Calc_HCF_mu_Exponential_Supercooled
            HCF_Constructor%kzero = HCF_Constructor%Ks / HCF_Constructor%Calculate_HCF_mu(Tcrit)
        end select

    end function HCF_Constructor

    function Calculate_Ks(Instance_HCF, h) result(kr)
        implicit none
        class(HCF), intent(in) :: Instance_HCF
        real(real64), intent(in) :: h
        real(real64) :: kr

        ! select case (Instance_HCF%Ks)
        ! case (1)
        !     kr = Calculate_HCF_BC(Instance_HCF, h)
        ! case (2)
        !     kr = Calculate_HCF_VG(Instance_HCF, h)
        ! case (3)
        !     kr = Calculate_HCF_KO(Instance_HCF, h)
        ! case (4)
        !     kr = Calculate_HCF_MVG(Instance_HCF, h)
        ! case (5)
        !     kr = Calculate_HCF_Durner(Instance_HCF, h)
        ! case (6)
        !     kr = Calculate_HCF_DVGCH(Instance_HCF, h)
        ! end select

    end function Calculate_Ks

    function Calculate_HCF_BC(Instance_HCF, h) result(kr)
        implicit none
        type(HCF), intent(in) :: Instance_HCF
        real(real64), intent(in) :: h
        real(real64) :: kr
        real(real64) :: Sw

        if (h < -1.0d0 / Instance_HCF%alpha1) then
            Sw = abs(Instance_HCF%alpha1 * h)**(-Instance_HCF%n1)
        else
            Sw = 1.0d0
        end if

        kr = Sw**(2.0d0 / (Instance_HCF%n1 + Instance_HCF%l + 2.0d0))

    end function Calculate_HCF_BC

    function Calculate_HCF_VG(Instance_HCF, h) result(kr)
        implicit none
        type(HCF), intent(in) :: Instance_HCF
        real(real64), intent(in) :: h
        real(real64) :: kr
        real(real64) :: Sw

        if (h < 0) then
            Sw = (1.0d0 + abs(Instance_HCF%alpha1 * h)**Instance_HCF%n1)**(-Instance_HCF%m1)
        else
            Sw = 1.0d0
        end if

        kr = Sw**Instance_HCF%l * (1.0d0 - (1.0d0 - Sw**(1.0d0 / Instance_HCF%m1))**Instance_HCF%m1)**2.0d0

    end function Calculate_HCF_VG

    function Calculate_HCF_KO(Instance_HCF, h) result(kr)
        implicit none
        type(HCF), intent(in) :: Instance_HCF
        real(real64), intent(in) :: h
        real(real64) :: kr
        real(real64) :: Sw

        if (h < 0) then
            Sw = 0.5d0 * erfc(log(h / Instance_HCF%alpha1) / (Instance_HCF%n1 * sqrt(2.0d0)))
            kr = Sw**0.5d0 * (0.5d0 * erfc(log(h / Instance_HCF%alpha1) / (Instance_HCF%n1 * sqrt(2.0d0)) + Instance_HCF%n1 / sqrt(2.0d0)))**2.0d0
        else
            kr = 1.0d0
        end if

    end function Calculate_HCF_KO

    function Calculate_HCF_MVG(Instance_HCF, h) result(kr)
        implicit none
        type(HCF), intent(in) :: Instance_HCF
        real(real64), intent(in) :: h
        real(real64) :: kr
        real(real64) :: Sw, thetaM

        thetaM = Instance_HCF%thetaR + (Instance_HCF%thetaS - Instance_HCF%thetaR) * (1.0d0 + abs(Instance_HCF%alpha1 * Instance_HCF%hcrit)**Instance_HCF%n1)**(-Instance_HCF%m1)

        if (h < Instance_HCF%hcrit) then
            Sw = (Instance_HCF%thetaS - Instance_HCF%thetaR) / (thetaM - Instance_HCF%thetaR) * (1.0d0 + abs(Instance_HCF%alpha1 * h)**Instance_HCF%n1)**(-Instance_HCF%m1)
            kr = Sw**Instance_HCF%l * ((1.0d0 - (1.0d0 - Sw**(1.0d0 / Instance_HCF%m1))**Instance_HCF%m1) / (1.0d0 - (1.0d0 - 1.0d0**(1.0d0 / Instance_HCF%m1))**Instance_HCF%m1))**2.0d0
        else
            kr = 1.0d0
        end if

    end function Calculate_HCF_MVG

    function Calculate_HCF_Durner(Instance_HCF, h) result(kr)
        implicit none
        type(HCF), intent(in) :: Instance_HCF
        real(real64), intent(in) :: h
        real(real64) :: kr
        real(real64) :: Sw1, Sw2

        if (h < 0) then
            Sw1 = Instance_HCF%w1 * (1.0d0 + abs(Instance_HCF%alpha1 * h)**Instance_HCF%n1)**(-Instance_HCF%m1)
            Sw2 = Instance_HCF%w2 * (1.0d0 + abs(Instance_HCF%alpha2 * h)**Instance_HCF%n2)**(-Instance_HCF%m2)
            kr = (Instance_HCF%w1 * Sw1 + Instance_HCF%w2 * Sw2)**Instance_HCF%l &
                 * (Instance_HCF%w1 * Instance_HCF%alpha1 * (1.0d0 - (1.0d0 - Sw1**(1.0d0 / Instance_HCF%m1))**Instance_HCF%m1) &
                    + Instance_HCF%w2 * Instance_HCF%alpha2 * (1.0d0 - (1.0d0 - Sw2**(1.0d0 / Instance_HCF%m2))**Instance_HCF%m2))**2.0d0 &
                 / (Instance_HCF%w1 * Instance_HCF%alpha1 + Instance_HCF%w2 * Instance_HCF%alpha2)**2.0d0
        else
            kr = 1.0d0
        end if

    end function Calculate_HCF_Durner

    function Calculate_HCF_DVGCH(Instance_HCF, h) result(kr)
        implicit none
        type(HCF), intent(in) :: Instance_HCF
        real(real64), intent(in) :: h
        real(real64) :: kr
        real(real64) :: Sw1, Sw2

        if (h < 0) then
            Sw1 = Instance_HCF%w1 * (1.0d0 + abs(Instance_HCF%alpha1 * h)**Instance_HCF%n1)**(-Instance_HCF%m1)
            Sw2 = Instance_HCF%w2 * (1.0d0 + abs(Instance_HCF%alpha1 * h)**Instance_HCF%n2)**(-Instance_HCF%m2)
            kr = (Instance_HCF%w1 * Sw1 + Instance_HCF%w2 * Sw2)**Instance_HCF%l &
                 * (Instance_HCF%w1 * Instance_HCF%alpha1 * (1.0d0 - (1.0d0 - Sw1**(1.0d0 / Instance_HCF%m1))**Instance_HCF%m1) &
                    + Instance_HCF%w2 * Instance_HCF%alpha1 * (1.0d0 - (1.0d0 - Sw2**(1.0d0 / Instance_HCF%m2))**Instance_HCF%m2))**2.0d0 &
                 / (Instance_HCF%w1 * Instance_HCF%alpha1 + Instance_HCF%w2 * Instance_HCF%alpha2)**2.0d0
        else
            kr = 1.0d0
        end if

    end function Calculate_HCF_DVGCH

    function Calc_HCF_mu_Exponential(T) result(mu)
        implicit none
        real(real64), intent(in) :: T
        real(real64) :: mu

        mu = 2.1d-6 * exp(1808.5d0 / (T + 273.15d0))

    end function Calc_HCF_mu_Exponential

    function Calc_HCF_mu_Exponential_Supercooled(T) result(mu)
        implicit none
        real(real64), intent(in) :: T
        real(real64) :: mu

        mu = 1.3788d-4 * ((273.15d0 + T) / 225.66d0 - 1.0d0)**(-1.6438)

    end function Calc_HCF_mu_Exponential_Supercooled

end module Calculate_HCF
