module Calculate_HCF
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Types
    implicit none

    type, abstract :: Abstract_Base_HCF
        real(real64) :: Ks !! saturated hydraulic conductivity
    end type Abstract_Base_HCF

    type, abstract, extends(Abstract_Base_HCF) :: Abstract_HCF
        real(real64) :: thetaS !! saturated water content
        real(real64) :: thetaR !! residual water content
        real(real64) :: alpha1
        real(real64) :: n1
        real(real64) :: l
    contains
        procedure(Abstract_Calculation_kr), deferred :: Calculate_kr
    end type Abstract_HCF

    type, abstract, extends(Abstract_Base_HCF) :: Abstract_HCF_Impedance
        real(real64) :: Omega
    contains
        procedure(Abstract_Calculation_Impedance), deferred :: Calculate_Impedance
    end type Abstract_HCF_Impedance

    type, extends(Abstract_HCF) :: Type_HCF_BC
    contains
        procedure :: Calculate_kr => Calculate_BC_kr
    end type Type_HCF_BC

    type, extends(Abstract_Base_HCF) :: Type_HCF_BC_Impedance
        real(real64) :: Omega
    contains
        procedure :: Calculate_Impedance => Calculate_HCF_BC_Impedance
    end type Type_HCF_BC_Impedance

    type, extends(Abstract_Base_HCF) :: Type_HCF_BC_Viscosity
        real(real64) :: kzero
        procedure(Abstract_Calculate_Viscosiy), pointer, nopass :: Calculate_Viscosiy => null()
    end type Type_HCF_BC_Viscosity

    type, extends(Type_HCF_BC) :: Type_HCF_BC_BC_Impedance
        real(real64) :: Omega
    contains
        procedure :: Calculate_Impedance => Calculate_HCF_BC_BC_Impedance
    end type Type_HCF_BC_BC_Impedance

    type, extends(Type_HCF_BC) :: Type_HCF_BC_BC_Viscosity
        real(real64) :: kzero
        procedure(Abstract_Calculate_Viscosiy), pointer, nopass :: Calculate_Viscosiy => null()
    end type Type_HCF_BC_BC_Viscosity

    type, extends(Type_HCF_BC_BC_Impedance) :: Type_HCF_BC_BC_Impedance_Viscosity
        real(real64) :: kzero
        procedure(Abstract_Calculate_Viscosiy), pointer, nopass :: Calculate_Viscosiy => null()
    end type Type_HCF_BC_BC_Impedance_Viscosity

    ! type, extends(HCF_Parameters) :: HCF
    !     private
    !     procedure(Calculation_HCF), pointer, nopass :: Calculate_HCF => null()
    !     procedure(Calculation_HCF_mu), pointer, nopass :: Calculate_HCF_mu => null()
    ! contains
    !     procedure :: Calculate_Kflh => Calculate_Ks
    ! end type HCF

    ! interface HCF
    !     module procedure HCF_Constructor
    ! end interface

    interface
        function Abstract_Calculation_kr(self, h) result(kr)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Abstract_HCF
            implicit none
            class(Abstract_HCF), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr
        end function Abstract_Calculation_kr

        function Abstract_Calculation_Impedance(self, theta_ice) result(Impedance)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Abstract_HCF_Impedance
            implicit none
            class(Abstract_HCF_Impedance), intent(in) :: self
            real(real64), intent(in) :: theta_ice
            real(real64) :: Impedance
        end function Abstract_Calculation_Impedance

    end interface

    ! abstract interface
    !     function Calculation_HCF(self, h) result(kr)
    !         use, intrinsic :: iso_fortran_env, only: real64
    !         import :: HCF
    !         implicit none
    !         type(HCF), intent(in) :: self
    !         real(real64), intent(in) :: h
    !         real(real64) :: kr
    !     end function Calculation_HCF
    ! end interface

    abstract interface
        function Abstract_Calculate_Viscosiy(Temp) result(Viscosity)
            use, intrinsic :: iso_fortran_env, only: real64
            implicit none
            real(real64), intent(in) :: Temp
            real(real64) :: Viscosity
        end function Abstract_Calculate_Viscosiy
    end interface

contains
    ! type(HCF) function HCF_Constructor(in_HCF_Parameters, HCF_Model_id, Kflh_Model_id)
    !     implicit none
    !     real(real64), intent(in) :: in_HCF_Parameters(:)
    !     integer(int32), intent(in) :: HCF_Model_id, Kflh_Model_id
    !     real(real64), parameter :: Tcrit = 15.0d0

    !     select case (HCF_Model_id)
    !     case (1)
    !         HCF_Constructor%thetaS = in_HCF_Parameters(1)
    !         HCF_Constructor%thetaR = in_HCF_Parameters(2)
    !         HCF_Constructor%alpha1 = in_HCF_Parameters(3)
    !         HCF_Constructor%n1 = in_HCF_Parameters(4)
    !         HCF_Constructor%Ks = in_HCF_Parameters(5)
    !         HCF_Constructor%l = in_HCF_Parameters(6)

    !         HCF_Constructor%Calculate_HCF => Calculate_HCF_BC
    !     case (2)
    !         HCF_Constructor%thetaS = in_HCF_Parameters(1)
    !         HCF_Constructor%thetaR = in_HCF_Parameters(2)
    !         HCF_Constructor%alpha1 = in_HCF_Parameters(3)
    !         HCF_Constructor%n1 = in_HCF_Parameters(4)
    !         HCF_Constructor%Ks = in_HCF_Parameters(5)
    !         HCF_Constructor%l = in_HCF_Parameters(6)
    !         HCF_Constructor%m1 = 1.0d0 - 1.0d0 / HCF_Constructor%n1

    !         HCF_Constructor%Calculate_HCF => Calculate_HCF_VG
    !     case (3)
    !         HCF_Constructor%thetaS = in_HCF_Parameters(1)
    !         HCF_Constructor%thetaR = in_HCF_Parameters(2)
    !         HCF_Constructor%alpha1 = in_HCF_Parameters(3)
    !         HCF_Constructor%n1 = in_HCF_Parameters(4)
    !         HCF_Constructor%Ks = in_HCF_Parameters(5)
    !         HCF_Constructor%l = in_HCF_Parameters(6)

    !         HCF_Constructor%Calculate_HCF => Calculate_HCF_KO
    !     case (4)
    !         HCF_Constructor%thetaS = in_HCF_Parameters(1)
    !         HCF_Constructor%thetaR = in_HCF_Parameters(2)
    !         HCF_Constructor%alpha1 = in_HCF_Parameters(3)
    !         HCF_Constructor%n1 = in_HCF_Parameters(4)
    !         HCF_Constructor%hcrit = in_HCF_Parameters(5)
    !         HCF_Constructor%Ks = in_HCF_Parameters(6)
    !         HCF_Constructor%l = in_HCF_Parameters(7)
    !         HCF_Constructor%m1 = 1.0d0 - 1.0d0 / HCF_Constructor%n1

    !         HCF_Constructor%Calculate_HCF => Calculate_HCF_MVG
    !     case (5)
    !         HCF_Constructor%thetaS = in_HCF_Parameters(1)
    !         HCF_Constructor%thetaR = in_HCF_Parameters(2)
    !         HCF_Constructor%alpha1 = in_HCF_Parameters(3)
    !         HCF_Constructor%n1 = in_HCF_Parameters(4)
    !         HCF_Constructor%alpha2 = in_HCF_Parameters(5)
    !         HCF_Constructor%n2 = in_HCF_Parameters(6)
    !         HCF_Constructor%w1 = in_HCF_Parameters(7)
    !         HCF_Constructor%Ks = in_HCF_Parameters(8)
    !         HCF_Constructor%l = in_HCF_Parameters(9)

    !         HCF_Constructor%m1 = 1.0d0 - 1.0d0 / HCF_Constructor%n1
    !         HCF_Constructor%m2 = 1.0d0 - 1.0d0 / HCF_Constructor%n2
    !         HCF_Constructor%w2 = 1.0d0 - HCF_Constructor%w1

    !         HCF_Constructor%Calculate_HCF => Calculate_HCF_Durner
    !     case (6)
    !         HCF_Constructor%thetaS = in_HCF_Parameters(1)
    !         HCF_Constructor%thetaR = in_HCF_Parameters(2)
    !         HCF_Constructor%alpha1 = in_HCF_Parameters(3)
    !         HCF_Constructor%n1 = in_HCF_Parameters(4)
    !         HCF_Constructor%n2 = in_HCF_Parameters(5)
    !         HCF_Constructor%w1 = in_HCF_Parameters(6)
    !         HCF_Constructor%Ks = in_HCF_Parameters(7)
    !         HCF_Constructor%l = in_HCF_Parameters(8)

    !         HCF_Constructor%m1 = 1.0d0 - 1.0d0 / HCF_Constructor%n1
    !         HCF_Constructor%m2 = 1.0d0 - 1.0d0 / HCF_Constructor%n2
    !         HCF_Constructor%w2 = 1.0d0 - HCF_Constructor%w1

    !         HCF_Constructor%Calculate_HCF => Calculate_HCF_DVGCH
    !     end select

    !     select case (Kflh_Model_id)
    !     case (3, 5)
    !         HCF_Constructor%Calculate_HCF_mu => Calc_HCF_mu_Exponential
    !         HCF_Constructor%kzero = HCF_Constructor%Ks / HCF_Constructor%Calculate_HCF_mu(Tcrit)
    !     case (6, 7)
    !         HCF_Constructor%Calculate_HCF_mu => Calc_HCF_mu_Exponential_Supercooled
    !         HCF_Constructor%kzero = HCF_Constructor%Ks / HCF_Constructor%Calculate_HCF_mu(Tcrit)
    !     end select

    ! end function HCF_Constructor

    ! function Calculate_Ks(self, h) result(kr)
    !     implicit none
    !     class(HCF), intent(in) :: self
    !     real(real64), intent(in) :: h
    !     real(real64) :: kr

    !     ! select case (self%Ks)
    !     ! case (1)
    !     !     kr = Calculate_HCF_BC(self, h)
    !     ! case (2)
    !     !     kr = Calculate_HCF_VG(self, h)
    !     ! case (3)
    !     !     kr = Calculate_HCF_KO(self, h)
    !     ! case (4)
    !     !     kr = Calculate_HCF_MVG(self, h)
    !     ! case (5)
    !     !     kr = Calculate_HCF_Durner(self, h)
    !     ! case (6)
    !     !     kr = Calculate_HCF_DVGCH(self, h)
    !     ! end select
    !     kr = 1.0d0

    ! end function Calculate_Ks

    ! subroutine Set_HCF(self, HCF_Model_id)
    !     implicit none
    !     class(Type_HCF_BC), intent(inout) :: self
    !     integer(int32), intent(in) :: HCF_Model_id

    !     select case (HCF_Model_id)

    !     case (1)
    !         self%Calculate_kr => Calculate_BC_kr
    !     end select
    ! end subroutine Set_HCF

    function Calculate_BC_kr(self, h) result(kr)
        implicit none
        class(Type_HCF_BC), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr
        real(real64) :: Sw

        if (h < -1.0d0 / self%alpha1) then
            Sw = (self%alpha1 * h)**(-self%n1)
        else
            Sw = 1.0d0
        end if

        kr = Sw**(2.0d0 / (self%n1 + self%l + 2.0d0))

    end function Calculate_BC_kr

    ! function Calculate_HCF_VG(self, h) result(kr)
    !     implicit none
    !     type(HCF), intent(in) :: self
    !     real(real64), intent(in) :: h
    !     real(real64) :: kr
    !     real(real64) :: Sw

    !     if (h < 0) then
    !         Sw = (1.0d0 + abs(self%alpha1 * h)**self%n1)**(-self%m1)
    !     else
    !         Sw = 1.0d0
    !     end if

    !     kr = Sw**self%l * (1.0d0 - (1.0d0 - Sw**(1.0d0 / self%m1))**self%m1)**2.0d0

    ! end function Calculate_HCF_VG

    ! function Calculate_HCF_KO(self, h) result(kr)
    !     implicit none
    !     type(HCF), intent(in) :: self
    !     real(real64), intent(in) :: h
    !     real(real64) :: kr
    !     real(real64) :: Sw

    !     if (h < 0) then
    !         Sw = 0.5d0 * erfc(log(h / self%alpha1) / (self%n1 * sqrt(2.0d0)))
    !         kr = Sw**0.5d0 * (0.5d0 * erfc(log(h / self%alpha1) / (self%n1 * sqrt(2.0d0)) + self%n1 / sqrt(2.0d0)))**2.0d0
    !     else
    !         kr = 1.0d0
    !     end if

    ! end function Calculate_HCF_KO

    ! function Calculate_HCF_MVG(self, h) result(kr)
    !     implicit none
    !     type(HCF), intent(in) :: self
    !     real(real64), intent(in) :: h
    !     real(real64) :: kr
    !     real(real64) :: Sw, thetaM

    !     thetaM = self%thetaR + (self%thetaS - self%thetaR) * (1.0d0 + abs(self%alpha1 * self%hcrit)**self%n1)**(-self%m1)

    !     if (h < self%hcrit) then
    !         Sw = (self%thetaS - self%thetaR) / (thetaM - self%thetaR) * (1.0d0 + abs(self%alpha1 * h)**self%n1)**(-self%m1)
    !         kr = Sw**self%l * ((1.0d0 - (1.0d0 - Sw**(1.0d0 / self%m1))**self%m1) / (1.0d0 - (1.0d0 - 1.0d0**(1.0d0 / self%m1))**self%m1))**2.0d0
    !     else
    !         kr = 1.0d0
    !     end if

    ! end function Calculate_HCF_MVG

    ! function Calculate_HCF_Durner(self, h) result(kr)
    !     implicit none
    !     type(HCF), intent(in) :: self
    !     real(real64), intent(in) :: h
    !     real(real64) :: kr
    !     real(real64) :: Sw1, Sw2

    !     if (h < 0) then
    !         Sw1 = self%w1 * (1.0d0 + abs(self%alpha1 * h)**self%n1)**(-self%m1)
    !         Sw2 = self%w2 * (1.0d0 + abs(self%alpha2 * h)**self%n2)**(-self%m2)
    !         kr = (self%w1 * Sw1 + self%w2 * Sw2)**self%l &
    !              * (self%w1 * self%alpha1 * (1.0d0 - (1.0d0 - Sw1**(1.0d0 / self%m1))**self%m1) &
    !                 + self%w2 * self%alpha2 * (1.0d0 - (1.0d0 - Sw2**(1.0d0 / self%m2))**self%m2))**2.0d0 &
    !              / (self%w1 * self%alpha1 + self%w2 * self%alpha2)**2.0d0
    !     else
    !         kr = 1.0d0
    !     end if

    ! end function Calculate_HCF_Durner

    ! function Calculate_HCF_DVGCH(self, h) result(kr)
    !     implicit none
    !     type(HCF), intent(in) :: self
    !     real(real64), intent(in) :: h
    !     real(real64) :: kr
    !     real(real64) :: Sw1, Sw2

    !     if (h < 0) then
    !         Sw1 = self%w1 * (1.0d0 + abs(self%alpha1 * h)**self%n1)**(-self%m1)
    !         Sw2 = self%w2 * (1.0d0 + abs(self%alpha1 * h)**self%n2)**(-self%m2)
    !         kr = (self%w1 * Sw1 + self%w2 * Sw2)**self%l &
    !              * (self%w1 * self%alpha1 * (1.0d0 - (1.0d0 - Sw1**(1.0d0 / self%m1))**self%m1) &
    !                 + self%w2 * self%alpha1 * (1.0d0 - (1.0d0 - Sw2**(1.0d0 / self%m2))**self%m2))**2.0d0 &
    !              / (self%w1 * self%alpha1 + self%w2 * self%alpha2)**2.0d0
    !     else
    !         kr = 1.0d0
    !     end if

    ! end function Calculate_HCF_DVGCH

    function Calculate_HCF_mu_Exponential(Temp) result(Viscosity)
        implicit none
        real(real64), intent(in) :: Temp
        real(real64) :: Viscosity

        Viscosity = 2.1d-6 * exp(1808.5d0 / (Temp + 273.15d0))

    end function Calculate_HCF_mu_Exponential

    function Calculate_HCF_mu_Exponential_Supercooled(Temp) result(Viscosity)
        implicit none
        real(real64), intent(in) :: Temp
        real(real64) :: Viscosity

        Viscosity = 1.3788d-4 * ((273.15d0 + Temp) / 225.66d0 - 1.0d0)**(-1.6438)

    end function Calculate_HCF_mu_Exponential_Supercooled

    function Calculate_HCF_BC_Impedance(self, theta_ice) result(Impedance)
        implicit none
        class(Type_HCF_BC_Impedance), intent(in) :: self
        real(real64), intent(in) :: theta_ice
        real(real64) :: Impedance

        Impedance = 10.0d0**(-self%Omega * theta_ice)

    end function Calculate_HCF_BC_Impedance

    function Calculate_HCF_BC_BC_Impedance(self, theta_ice) result(Impedance)
        implicit none
        class(Type_HCF_BC_BC_Impedance), intent(in) :: self
        real(real64), intent(in) :: theta_ice
        real(real64) :: Impedance

        Impedance = 10.0d0**(-self%Omega * theta_ice)

    end function Calculate_HCF_BC_BC_Impedance

end module Calculate_HCF
