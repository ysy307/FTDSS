module Calculate_HCF
    use, intrinsic :: iso_fortran_env, only: int32, real64
#ifdef _OPENMP
    use omp_lib
#endif
    implicit none

    type, abstract :: Abstract_HCF
        real(real64) :: Ks !! saturated hydraulic conductivity
        integer(int32) :: nsize
        real(real64), allocatable :: Kflh(:)
    contains
        !! get/set
    end type Abstract_HCF

    type, abstract, extends(Abstract_HCF) :: Abstract_HCF_Base
        real(real64) :: thetaS !! saturated water content
        real(real64) :: thetaR !! residual water content
        real(real64) :: alpha1
        real(real64) :: n1
        real(real64) :: l
    contains
        procedure(Abstract_Calculate_kr_HCF_Base), pass(self), deferred :: Calculate_kr
        procedure(Abstract_Calculate_Kflh_Base), pass(self), deferred :: Calculate_Kflh
        procedure(Abstract_Update_Kflh_Base), pass(self), deferred :: Update_Kflh
    end type Abstract_HCF_Base

    type, abstract, extends(Abstract_HCF) :: Abstract_HCF_Impedance
        real(real64) :: Omega !! Impedance factor
    contains
        procedure(Abstract_Calculate_Impedance), nopass, deferred :: Calculate_Impedance
        procedure(Abstract_Calculate_Kflh_Impedance), pass(self), deferred :: Calculate_Kflh
        procedure(Abstract_Update_Kflh_Impedance), pass(self), deferred :: Update_Kflh
    end type Abstract_HCF_Impedance

    type, abstract, extends(Abstract_HCF) :: Abstract_HCF_Viscosity
        real(real64) :: kzero
        procedure(Abstract_Calculate_Viscosity), nopass, pointer :: Calculate_Viscosity => null()
    contains
        procedure(Abstract_Set_Calculate_HCF_Viscosity), nopass, deferred :: Set_Calculate_Viscosity
        procedure(Abstract_Calculate_Kflh_Viscosity), pass(self), deferred :: Calculate_Kflh
        procedure(Abstract_Update_Kflh_Viscosity), pass(self), deferred :: Update_Kflh
    end type Abstract_HCF_Viscosity

    type, abstract, extends(Abstract_HCF) :: Abstract_HCF_Base_Impedance
        real(real64) :: thetaS !! saturated water content
        real(real64) :: thetaR !! residual water content
        real(real64) :: alpha1
        real(real64) :: n1
        real(real64) :: l
        real(real64) :: Omega
    contains
        procedure(Abstract_Calculate_kr_HCF_Base_Impedance), pass(self), deferred :: Calculate_kr
        procedure(Abstract_Calculate_Impedance), nopass, deferred :: Calculate_Impedance
        procedure(Abstract_Calculate_Kflh_Base_Impedance), pass(self), deferred :: Calculate_Kflh
        procedure(Abstract_Update_Kflh_Base_Impedance), pass(self), deferred :: Update_Kflh
    end type Abstract_HCF_Base_Impedance

    type, abstract, extends(Abstract_HCF) :: Abstract_HCF_Base_Viscosity
        real(real64) :: thetaS !! saturated water content
        real(real64) :: thetaR !! residual water content
        real(real64) :: alpha1
        real(real64) :: n1
        real(real64) :: l
        real(real64) :: kzero
        procedure(Abstract_Calculate_Viscosity), nopass, pointer :: Calculate_Viscosity => null()
    contains
        procedure(Abstract_Calculate_kr_HCF_Base_Viscosity), pass(self), deferred :: Calculate_kr
        procedure(Abstract_Set_Calculate_HCF_Viscosity), nopass, deferred :: Set_Calculate_Viscosity
        procedure(Abstract_Calculate_Kflh_Base_Viscosity), pass(self), deferred :: Calculate_Kflh
        procedure(Abstract_Update_Kflh_Base_Viscosity), pass(self), deferred :: Update_Kflh
    end type Abstract_HCF_Base_Viscosity

    type, abstract, extends(Abstract_HCF) :: Abstract_HCF_Impedance_Viscosity
        real(real64) :: thetaS !! saturated water content
        real(real64) :: thetaR !! residual water content
        real(real64) :: alpha1
        real(real64) :: n1
        real(real64) :: l
        real(real64) :: Omega
        real(real64) :: kzero
        procedure(Abstract_Calculate_Viscosity), nopass, pointer :: Calculate_Viscosity => null()
    contains
        procedure(Abstract_Set_Calculate_HCF_Viscosity), nopass, deferred :: Set_Calculate_Viscosity
        procedure(Abstract_Calculate_Impedance), nopass, deferred :: Calculate_Impedance
        procedure(Abstract_Calculate_Kflh_Impedance_Viscosity), pass(self), deferred :: Calculate_Kflh
        procedure(Abstract_Update_Kflh_Impedance_Viscosity), pass(self), deferred :: Update_Kflh
    end type Abstract_HCF_Impedance_Viscosity

    type, abstract, extends(Abstract_HCF) :: Abstract_HCF_Base_Impedance_Viscosity
        real(real64) :: thetaS !! saturated water content
        real(real64) :: thetaR !! residual water content
        real(real64) :: alpha1
        real(real64) :: n1
        real(real64) :: l
        real(real64) :: Omega
        real(real64) :: kzero
        procedure(Abstract_Calculate_Viscosity), nopass, pointer :: Calculate_Viscosity => null()
    contains
        procedure(Abstract_Calculate_kr_HCF_Base_Impedance_Viscosity), pass(self), deferred :: Calculate_kr
        procedure(Abstract_Calculate_Impedance), nopass, deferred :: Calculate_Impedance
        procedure(Abstract_Set_Calculate_HCF_Viscosity), nopass, deferred :: Set_Calculate_Viscosity
        procedure(Abstract_Calculate_Kflh_Base_Impedance_Viscosity), pass(self), deferred :: Calculate_Kflh
        procedure(Abstract_Update_Kflh_Base_Impedance_Viscosity), pass(self), deferred :: Update_Kflh
    end type Abstract_HCF_Base_Impedance_Viscosity

    type, extends(Abstract_HCF_Base) :: Type_HCF_Base_BC
    contains
        procedure :: Calculate_kr => Calculate_kr_Base_BC
        procedure :: Calculate_Kflh => Calculate_Kflh_Base_BC
        procedure :: Update_Kflh => Update_Kflh_Base_BC
    end type Type_HCF_Base_BC

    type, extends(Abstract_HCF_Base) :: Type_HCF_Base_VG
        real(real64) :: m1
    contains
        procedure :: Calculate_kr => Calculate_kr_Base_VG
        procedure :: Calculate_Kflh => Calculate_Kflh_Base_VG
        procedure :: Update_Kflh => Update_Kflh_Base_VG
    end type Type_HCF_Base_VG

    type, extends(Abstract_HCF_Base) :: Type_HCF_Base_KO
    contains
        procedure :: Calculate_kr => Calculate_kr_Base_KO
        procedure :: Calculate_Kflh => Calculate_Kflh_Base_KO
        procedure :: Update_Kflh => Update_Kflh_Base_KO
    end type Type_HCF_Base_KO

    type, extends(Abstract_HCF_Base) :: Type_HCF_Base_MVG
        real(real64) :: hcrit
        real(real64) :: m1
    contains
        procedure :: Calculate_kr => Calculate_kr_Base_MVG
        procedure :: Calculate_Kflh => Calculate_Kflh_Base_MVG
        procedure :: Update_Kflh => Update_Kflh_Base_MVG
    end type Type_HCF_Base_MVG

    type, extends(Abstract_HCF_Base) :: Type_HCF_Base_Durner
        real(real64) :: m1
        real(real64) :: alpha2
        real(real64) :: n2
        real(real64) :: m2
        real(real64) :: w1
        real(real64) :: w2
    contains
        procedure :: Calculate_kr => Calculate_kr_Base_Durner
        procedure :: Calculate_Kflh => Calculate_Kflh_Base_Durner
        procedure :: Update_Kflh => Update_Kflh_Base_Durner
    end type Type_HCF_Base_Durner

    type, extends(Abstract_HCF_Base) :: Type_HCF_Base_DVGCH
        real(real64) :: m1
        real(real64) :: n2
        real(real64) :: m2
        real(real64) :: w1
        real(real64) :: w2
    contains
        procedure :: Calculate_kr => Calculate_kr_Base_DVGCH
        procedure :: Calculate_Kflh => Calculate_Kflh_Base_DVGCH
        procedure :: Update_Kflh => Update_Kflh_Base_DVGCH
    end type Type_HCF_Base_DVGCH

    type, extends(Abstract_HCF_Impedance) :: Type_HCF_Impedance
    contains
        procedure, nopass :: Calculate_Impedance => Calculate_Impedance_Base
        procedure :: Calculate_Kflh => Calculate_Kflh_Impedance
        procedure :: Update_Kflh => Update_Kflh_Impedance
    end type Type_HCF_Impedance

    type, extends(Abstract_HCF_Viscosity) :: Type_HCF_Viscosity
    contains
        procedure, nopass :: Set_Calculate_Viscosity => Set_Calculate_Viscosity_Base
        procedure :: Calculate_Kflh => Calculate_Kflh_Viscosity
        procedure :: Update_Kflh => Update_Kflh_Viscosity
    end type Type_HCF_Viscosity

    type, extends(Abstract_HCF_Impedance_Viscosity) :: Type_HCF_Impedance_Viscosity
    contains
        procedure, nopass :: Calculate_Impedance => Calculate_Impedance_Base
        procedure, nopass :: Set_Calculate_Viscosity => Set_Calculate_Viscosity_Base
        procedure, pass :: Calculate_Kflh => Calculate_Kflh_Impedance_Viscosity
        procedure, pass :: Update_Kflh => Update_Kflh_Impedance_Viscosity
    end type Type_HCF_Impedance_Viscosity

    type, extends(Abstract_HCF_Base_Impedance) :: Type_HCF_Base_Impedance_BC
    contains
        procedure :: Calculate_kr => Calculate_kr_Base_Impedance_BC
        procedure, nopass :: Calculate_Impedance => Calculate_Impedance_Base
        procedure :: Calculate_Kflh => Calculate_Kflh_Base_Impedance_BC
        procedure :: Update_Kflh => Update_Kflh_Base_Impedance_BC
    end type Type_HCF_Base_Impedance_BC

    type, extends(Abstract_HCF_Base_Impedance) :: Type_HCF_Base_Impedance_VG
        real(real64) :: m1
    contains
        procedure :: Calculate_kr => Calculate_kr_Base_Impedance_VG
        procedure, nopass :: Calculate_Impedance => Calculate_Impedance_Base
        procedure :: Calculate_Kflh => Calculate_Kflh_Base_Impedance_VG
        procedure :: Update_Kflh => Update_Kflh_Base_Impedance_VG
    end type Type_HCF_Base_Impedance_VG

    type, extends(Abstract_HCF_Base_Impedance) :: Type_HCF_Base_Impedance_KO
    contains
        procedure :: Calculate_kr => Calculate_kr_Base_Impedance_KO
        procedure, nopass :: Calculate_Impedance => Calculate_Impedance_Base
        procedure :: Calculate_Kflh => Calculate_Kflh_Base_Impedance_KO
        procedure :: Update_Kflh => Update_Kflh_Base_Impedance_KO
    end type Type_HCF_Base_Impedance_KO

    type, extends(Abstract_HCF_Base_Impedance) :: Type_HCF_Base_Impedance_MVG
        real(real64) :: m1
        real(real64) :: hcrit
    contains
        procedure :: Calculate_kr => Calculate_kr_Base_Impedance_MVG
        procedure, nopass :: Calculate_Impedance => Calculate_Impedance_Base
        procedure :: Calculate_Kflh => Calculate_Kflh_Base_Impedance_MVG
        procedure :: Update_Kflh => Update_Kflh_Base_Impedance_MVG
    end type Type_HCF_Base_Impedance_MVG

    type, extends(Abstract_HCF_Base_Impedance) :: Type_HCF_Base_Impedance_Durner
        real(real64) :: m1
        real(real64) :: alpha2
        real(real64) :: n2
        real(real64) :: m2
        real(real64) :: w1
        real(real64) :: w2
    contains
        procedure :: Calculate_kr => Calculate_kr_Base_Impedance_Durner
        procedure, nopass :: Calculate_Impedance => Calculate_Impedance_Base
        procedure :: Calculate_Kflh => Calculate_Kflh_Base_Impedance_Durner
        procedure :: Update_Kflh => Update_Kflh_Base_Impedance_Durner
    end type Type_HCF_Base_Impedance_Durner

    type, extends(Abstract_HCF_Base_Impedance) :: Type_HCF_Base_Impedance_DVGCH
        real(real64) :: m1
        real(real64) :: n2
        real(real64) :: m2
        real(real64) :: w1
        real(real64) :: w2
    contains
        procedure :: Calculate_kr => Calculate_kr_Base_Impedance_DVGCH
        procedure, nopass :: Calculate_Impedance => Calculate_Impedance_Base
        procedure :: Calculate_Kflh => Calculate_Kflh_Base_Impedance_DVGCH
        procedure :: Update_Kflh => Update_Kflh_Base_Impedance_DVGCH
    end type Type_HCF_Base_Impedance_DVGCH

    type, extends(Abstract_HCF_Base_Viscosity) :: Type_HCF_Base_Viscosity_BC
    contains
        procedure :: Calculate_kr => Calculate_kr_Base_Viscosity_BC
        procedure, nopass :: Set_Calculate_Viscosity => Set_Calculate_Viscosity_Base
        procedure :: Calculate_Kflh => Calculate_Kflh_Base_Viscosity_BC
        procedure :: Update_Kflh => Update_Kflh_Base_Viscosity_BC
    end type Type_HCF_Base_Viscosity_BC

    type, extends(Abstract_HCF_Base_Viscosity) :: Type_HCF_Base_Viscosity_VG
        real(real64) :: m1
    contains
        procedure :: Calculate_kr => Calculate_kr_Base_Viscosity_VG
        procedure, nopass :: Set_Calculate_Viscosity => Set_Calculate_Viscosity_Base
        procedure :: Calculate_Kflh => Calculate_Kflh_Base_Viscosity_VG
        procedure :: Update_Kflh => Update_Kflh_Base_Viscosity_VG
    end type Type_HCF_Base_Viscosity_VG

    type, extends(Abstract_HCF_Base_Viscosity) :: Type_HCF_Base_Viscosity_KO
    contains
        procedure, nopass :: Set_Calculate_Viscosity => Set_Calculate_Viscosity_Base
        procedure :: Calculate_kr => Calculate_kr_Base_Viscosity_KO
        procedure :: Calculate_Kflh => Calculate_Kflh_Base_Viscosity_KO
        procedure :: Update_Kflh => Update_Kflh_Base_Viscosity_KO
    end type Type_HCF_Base_Viscosity_KO

    type, extends(Abstract_HCF_Base_Viscosity) :: Type_HCF_Base_Viscosity_MVG
        real(real64) :: m1
        real(real64) :: hcrit
    contains
        procedure :: Calculate_kr => Calculate_kr_Base_Viscosity_MVG
        procedure, nopass :: Set_Calculate_Viscosity => Set_Calculate_Viscosity_Base
        procedure :: Calculate_Kflh => Calculate_Kflh_Base_Viscosity_MVG
        procedure :: Update_Kflh => Update_Kflh_Base_Viscosity_MVG
    end type Type_HCF_Base_Viscosity_MVG

    type, extends(Abstract_HCF_Base_Viscosity) :: Type_HCF_Base_Viscosity_Durner
        real(real64) :: m1
        real(real64) :: alpha2
        real(real64) :: n2
        real(real64) :: m2
        real(real64) :: w1
        real(real64) :: w2
    contains
        procedure :: Calculate_kr => Calculate_kr_Base_Viscosity_Durner
        procedure, nopass :: Set_Calculate_Viscosity => Set_Calculate_Viscosity_Base
        procedure :: Calculate_Kflh => Calculate_Kflh_Base_Viscosity_Durner
        procedure :: Update_Kflh => Update_Kflh_Base_Viscosity_Durner
    end type Type_HCF_Base_Viscosity_Durner

    type, extends(Abstract_HCF_Base_Viscosity) :: Type_HCF_Base_Viscosity_DVGCH
        real(real64) :: m1
        real(real64) :: n2
        real(real64) :: m2
        real(real64) :: w1
        real(real64) :: w2
    contains
        procedure :: Calculate_kr => Calculate_kr_Base_Viscosity_DVGCH
        procedure, nopass :: Set_Calculate_Viscosity => Set_Calculate_Viscosity_Base
        procedure :: Calculate_Kflh => Calculate_Kflh_Base_Viscosity_DVGCH
        procedure :: Update_Kflh => Update_Kflh_Base_Viscosity_DVGCH
    end type Type_HCF_Base_Viscosity_DVGCH

    type, extends(Abstract_HCF_Base_Impedance_Viscosity) :: Type_HCF_Base_Impedance_Viscosity_BC
    contains
        procedure :: Calculate_kr => Calculate_kr_Base_Impedance_Viscosity_BC
        procedure, nopass :: Calculate_Impedance => Calculate_Impedance_Base
        procedure, nopass :: Set_Calculate_Viscosity => Set_Calculate_Viscosity_Base
        procedure :: Calculate_Kflh => Calculate_Kflh_Base_Impedance_Viscosity_BC
        procedure :: Update_Kflh => Update_Kflh_Base_Impedance_Viscosity_BC
    end type Type_HCF_Base_Impedance_Viscosity_BC

    type, extends(Abstract_HCF_Base_Impedance_Viscosity) :: Type_HCF_Base_Impedance_Viscosity_VG
        real(real64) :: m1
    contains
        procedure :: Calculate_kr => Calculate_kr_Base_Impedance_Viscosity_VG
        procedure, nopass :: Calculate_Impedance => Calculate_Impedance_Base
        procedure, nopass :: Set_Calculate_Viscosity => Set_Calculate_Viscosity_Base
        procedure :: Calculate_Kflh => Calculate_Kflh_Base_Impedance_Viscosity_VG
        procedure :: Update_Kflh => Update_Kflh_Base_Impedance_Viscosity_VG
    end type Type_HCF_Base_Impedance_Viscosity_VG

    type, extends(Abstract_HCF_Base_Impedance_Viscosity) :: Type_HCF_Base_Impedance_Viscosity_KO
    contains
        procedure :: Calculate_kr => Calculate_kr_Base_Impedance_Viscosity_KO
        procedure, nopass :: Calculate_Impedance => Calculate_Impedance_Base
        procedure, nopass :: Set_Calculate_Viscosity => Set_Calculate_Viscosity_Base
        procedure :: Calculate_Kflh => Calculate_Kflh_Base_Impedance_Viscosity_KO
        procedure :: Update_Kflh => Update_Kflh_Base_Impedance_Viscosity_KO
    end type Type_HCF_Base_Impedance_Viscosity_KO

    type, extends(Abstract_HCF_Base_Impedance_Viscosity) :: Type_HCF_Base_Impedance_Viscosity_MVG
        real(real64) :: m1
        real(real64) :: hcrit
    contains
        procedure :: Calculate_kr => Calculate_kr_Base_Impedance_Viscosity_MVG
        procedure, nopass :: Calculate_Impedance => Calculate_Impedance_Base
        procedure, nopass :: Set_Calculate_Viscosity => Set_Calculate_Viscosity_Base
        procedure :: Calculate_Kflh => Calculate_Kflh_Base_Impedance_Viscosity_MVG
        procedure :: Update_Kflh => Update_Kflh_Base_Impedance_Viscosity_MVG
    end type Type_HCF_Base_Impedance_Viscosity_MVG

    type, extends(Abstract_HCF_Base_Impedance_Viscosity) :: Type_HCF_Base_Impedance_Viscosity_Durner
        real(real64) :: m1
        real(real64) :: alpha2
        real(real64) :: n2
        real(real64) :: m2
        real(real64) :: w1
        real(real64) :: w2
    contains
        procedure :: Calculate_kr => Calculate_kr_Base_Impedance_Viscosity_Durner
        procedure, nopass :: Calculate_Impedance => Calculate_Impedance_Base
        procedure, nopass :: Set_Calculate_Viscosity => Set_Calculate_Viscosity_Base
        procedure :: Calculate_Kflh => Calculate_Kflh_Base_Impedance_Viscosity_Durner
        procedure :: Update_Kflh => Update_Kflh_Base_Impedance_Viscosity_Durner
    end type Type_HCF_Base_Impedance_Viscosity_Durner

    type, extends(Abstract_HCF_Base_Impedance_Viscosity) :: Type_HCF_Base_Impedance_Viscosity_DVGCH
        real(real64) :: m1
        real(real64) :: n2
        real(real64) :: m2
        real(real64) :: w1
        real(real64) :: w2
    contains
        procedure :: Calculate_kr => Calculate_kr_Base_Impedance_Viscosity_DVGCH
        procedure, nopass :: Calculate_Impedance => Calculate_Impedance_Base
        procedure, nopass :: Set_Calculate_Viscosity => Set_Calculate_Viscosity_Base
        procedure :: Calculate_Kflh => Calculate_Kflh_Base_Impedance_Viscosity_DVGCH
        procedure :: Update_Kflh => Update_Kflh_Base_Impedance_Viscosity_DVGCH
    end type Type_HCF_Base_Impedance_Viscosity_DVGCH

    abstract interface
        function Abstract_Calculate_Kflh_Base(self, h) result(Kflh)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Abstract_HCF_Base
            implicit none
            class(Abstract_HCF_Base), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: Kflh

        end function Abstract_Calculate_Kflh_Base

        function Abstract_Calculate_Kflh_Impedance(self, thetaI) result(Kflh)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Abstract_HCF_Impedance
            implicit none
            class(Abstract_HCF_Impedance), intent(in) :: self
            real(real64), intent(in) :: thetaI
            real(real64) :: Kflh

        end function Abstract_Calculate_Kflh_Impedance

        function Abstract_Calculate_Kflh_Viscosity(self, Temperature) result(Kflh)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Abstract_HCF_Viscosity
            implicit none
            class(Abstract_HCF_Viscosity), intent(in) :: self
            real(real64), intent(in) :: Temperature
            real(real64) :: Kflh

        end function Abstract_Calculate_Kflh_Viscosity

        function Abstract_Calculate_Kflh_Base_Impedance(self, h, thetaI) result(Kflh)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Abstract_HCF_Base_Impedance
            implicit none
            class(Abstract_HCF_Base_Impedance), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(in) :: thetaI
            real(real64) :: Kflh

        end function Abstract_Calculate_Kflh_Base_Impedance

        function Abstract_Calculate_Kflh_Base_Viscosity(self, h, Temperature) result(Kflh)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Abstract_HCF_Base_Viscosity
            implicit none
            class(Abstract_HCF_Base_Viscosity), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(in) :: Temperature
            real(real64) :: Kflh

        end function Abstract_Calculate_Kflh_Base_Viscosity

        function Abstract_Calculate_Kflh_Impedance_Viscosity(self, thetaI, Temperature) result(Kflh)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Abstract_HCF_Impedance_Viscosity
            implicit none
            class(Abstract_HCF_Impedance_Viscosity), intent(in) :: self
            real(real64), intent(in) :: thetaI
            real(real64), intent(in) :: Temperature
            real(real64) :: Kflh

        end function Abstract_Calculate_Kflh_Impedance_Viscosity

        function Abstract_Calculate_Kflh_Base_Impedance_Viscosity(self, h, thetaI, Temperature) result(Kflh)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Abstract_HCF_Base_Impedance_Viscosity
            implicit none
            class(Abstract_HCF_Base_Impedance_Viscosity), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(in) :: thetaI
            real(real64), intent(in) :: Temperature
            real(real64) :: Kflh

        end function Abstract_Calculate_Kflh_Base_Impedance_Viscosity

        subroutine Abstract_Update_Kflh_Base(self, arr_h)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Abstract_HCF_Base
            implicit none
            class(Abstract_HCF_Base), intent(inout) :: self
            real(real64), intent(in) :: arr_h(:)

        end subroutine Abstract_Update_Kflh_Base

        subroutine Abstract_Update_Kflh_Impedance(self, arr_thetaI)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Abstract_HCF_Impedance
            implicit none
            class(Abstract_HCF_Impedance), intent(inout) :: self
            real(real64), intent(in) :: arr_thetaI(:)

        end subroutine Abstract_Update_Kflh_Impedance

        subroutine Abstract_Update_Kflh_Viscosity(self, arr_Temperature)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Abstract_HCF_Viscosity
            implicit none
            class(Abstract_HCF_Viscosity), intent(inout) :: self
            real(real64), intent(in) :: arr_Temperature(:)

        end subroutine Abstract_Update_Kflh_Viscosity

        subroutine Abstract_Update_Kflh_Base_Impedance(self, arr_h, arr_thetaI)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Abstract_HCF_Base_Impedance
            implicit none
            class(Abstract_HCF_Base_Impedance), intent(inout) :: self
            real(real64), intent(in) :: arr_h(:)
            real(real64), intent(in) :: arr_thetaI(:)

        end subroutine Abstract_Update_Kflh_Base_Impedance

        subroutine Abstract_Update_Kflh_Base_Viscosity(self, arr_h, arr_Temperature)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Abstract_HCF_Base_Viscosity
            implicit none
            class(Abstract_HCF_Base_Viscosity), intent(inout) :: self
            real(real64), intent(in) :: arr_h(:)
            real(real64), intent(in) :: arr_Temperature(:)

        end subroutine Abstract_Update_Kflh_Base_Viscosity

        subroutine Abstract_Update_Kflh_Impedance_Viscosity(self, arr_thetaI, arr_Temperature)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Abstract_HCF_Impedance_Viscosity
            implicit none
            class(Abstract_HCF_Impedance_Viscosity), intent(inout) :: self
            real(real64), intent(in) :: arr_thetaI(:)
            real(real64), intent(in) :: arr_Temperature(:)

        end subroutine Abstract_Update_Kflh_Impedance_Viscosity

        subroutine Abstract_Update_Kflh_Base_Impedance_Viscosity(self, arr_h, arr_thetaI, arr_Temperature)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Abstract_HCF_Base_Impedance_Viscosity
            implicit none
            class(Abstract_HCF_Base_Impedance_Viscosity), intent(inout) :: self
            real(real64), intent(in) :: arr_h(:)
            real(real64), intent(in) :: arr_thetaI(:)
            real(real64), intent(in) :: arr_Temperature(:)

        end subroutine Abstract_Update_Kflh_Base_Impedance_Viscosity

        function Abstract_Calculate_kr_HCF_Base(self, h) result(kr)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Abstract_HCF_Base
            implicit none
            class(Abstract_HCF_Base), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr
        end function Abstract_Calculate_kr_HCF_Base

        function Abstract_Calculate_kr_HCF_Base_Impedance(self, h) result(kr)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Abstract_HCF_Base_Impedance
            implicit none
            class(Abstract_HCF_Base_Impedance), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr
        end function Abstract_Calculate_kr_HCF_Base_Impedance

        function Abstract_Calculate_kr_HCF_Base_Viscosity(self, h) result(kr)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Abstract_HCF_Base_Viscosity
            implicit none
            class(Abstract_HCF_Base_Viscosity), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr
        end function Abstract_Calculate_kr_HCF_Base_Viscosity

        function Abstract_Calculate_kr_HCF_Base_Impedance_Viscosity(self, h) result(kr)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Abstract_HCF_Base_Impedance_Viscosity
            implicit none
            class(Abstract_HCF_Base_Impedance_Viscosity), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr
        end function Abstract_Calculate_kr_HCF_Base_Impedance_Viscosity

        function Abstract_Calculate_Impedance(Omega, thetaI) result(Impedance)
            use, intrinsic :: iso_fortran_env, only: real64
            implicit none
            real(real64), intent(in) :: Omega
            real(real64), intent(in) :: thetaI
            real(real64) :: Impedance

        end function Abstract_Calculate_Impedance

        subroutine Abstract_Set_Calculate_HCF_Viscosity(Calculate_Viscosity_Type, Calculate_Viscosity)
            use, intrinsic :: iso_fortran_env, only: int32
            import :: Abstract_Calculate_Viscosity
            implicit none
            integer(int32), intent(in) :: Calculate_Viscosity_Type
            procedure(Abstract_Calculate_Viscosity), pointer, intent(inout) :: Calculate_Viscosity

        end subroutine Abstract_Set_Calculate_HCF_Viscosity

        function Abstract_Calculate_Viscosity(Temperature) result(Viscosity)
            use, intrinsic :: iso_fortran_env, only: real64
            implicit none
            real(real64), intent(in) :: Temperature
            real(real64) :: Viscosity
        end function Abstract_Calculate_Viscosity
    end interface

    interface
        module function Construct_Type_HCF_Base_BC(Ks, alpha1, n1, l, nsize) result(structure_HCF)
            implicit none
            real(real64), intent(in) :: Ks
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1
            real(real64), intent(in) :: l
            integer(int32), intent(in) :: nsize
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_BC

        module function Construct_Type_HCF_Base_BC_minimal() result(structure_HCF)
            implicit none
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_BC_minimal

        module function Construct_Type_HCF_Base_Impedance_BC(Ks, alpha1, n1, l, Omega, nsize) result(structure_HCF)
            implicit none
            real(real64), intent(in) :: Ks
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1
            real(real64), intent(in) :: l
            real(real64), intent(in) :: Omega
            integer(int32), intent(in) :: nsize
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Impedance_BC

        module function Construct_Type_HCF_Base_Impedance_BC_minimal() result(structure_HCF)
            implicit none
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Impedance_BC_minimal

        module function Construct_Type_HCF_Base_Viscosity_BC(Ks, alpha1, n1, l, useViscosity, nsize) result(structure_HCF)
            implicit none
            real(real64), intent(in) :: Ks
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1
            real(real64), intent(in) :: l
            integer(int32), intent(in) :: useViscosity
            integer(int32), intent(in) :: nsize
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Viscosity_BC

        module function Construct_Type_HCF_Base_Viscosity_BC_minimal() result(structure_HCF)
            implicit none
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Viscosity_BC_minimal

        module function Construct_Type_HCF_Base_Impedance_Viscosity_BC(Ks, alpha1, n1, l, Omega, useViscosity, nsize) result(structure_HCF)
            implicit none
            real(real64), intent(in) :: Ks
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1
            real(real64), intent(in) :: l
            real(real64), intent(in) :: Omega
            integer(int32), intent(in) :: useViscosity
            integer(int32), intent(in) :: nsize
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Impedance_Viscosity_BC

        module function Construct_Type_HCF_Base_Impedance_Viscosity_BC_minimal() result(structure_HCF)
            implicit none
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Impedance_Viscosity_BC_minimal

        module function Calculate_kr_BC_Base(alpha1, n1, l, h) result(kr)
            implicit none
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1
            real(real64), intent(in) :: l
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function Calculate_kr_BC_Base

        module function Calculate_kr_Base_BC(self, h) result(kr)
            implicit none
            class(Type_HCF_Base_BC), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function Calculate_kr_Base_BC

        module function Calculate_kr_Base_Impedance_BC(self, h) result(kr)
            implicit none
            class(Type_HCF_Base_Impedance_BC), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function Calculate_kr_Base_Impedance_BC

        module function Calculate_kr_Base_Viscosity_BC(self, h) result(kr)
            implicit none
            class(Type_HCF_Base_Viscosity_BC), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function Calculate_kr_Base_Viscosity_BC

        module function Calculate_kr_Base_Impedance_Viscosity_BC(self, h) result(kr)
            implicit none
            class(Type_HCF_Base_Impedance_Viscosity_BC), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function Calculate_kr_Base_Impedance_Viscosity_BC

        module function Calculate_Kflh_Base_BC(self, h) result(Kflh)
            implicit none
            class(Type_HCF_Base_BC), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: Kflh

        end function Calculate_Kflh_Base_BC

        module function Calculate_Kflh_Base_Impedance_BC(self, h, thetaI) result(Kflh)
            implicit none
            class(Type_HCF_Base_Impedance_BC), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(in) :: thetaI
            real(real64) :: Kflh

        end function Calculate_Kflh_Base_Impedance_BC

        module function Calculate_Kflh_Base_Viscosity_BC(self, h, Temperature) result(Kflh)
            implicit none
            class(Type_HCF_Base_Viscosity_BC), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(in) :: Temperature
            real(real64) :: Kflh

        end function Calculate_Kflh_Base_Viscosity_BC

        module function Calculate_Kflh_Base_Impedance_Viscosity_BC(self, h, thetaI, Temperature) result(Kflh)
            implicit none
            class(Type_HCF_Base_Impedance_Viscosity_BC), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(in) :: thetaI
            real(real64), intent(in) :: Temperature
            real(real64) :: Kflh

        end function Calculate_Kflh_Base_Impedance_Viscosity_BC

        module subroutine Update_Kflh_Base_BC(self, arr_h)
            implicit none
            class(Type_HCF_Base_BC), intent(inout) :: self
            real(real64), intent(in) :: arr_h(:)

        end subroutine Update_Kflh_Base_BC

        module subroutine Update_Kflh_Base_Impedance_BC(self, arr_h, arr_thetaI)
            implicit none
            class(Type_HCF_Base_Impedance_BC), intent(inout) :: self
            real(real64), intent(in) :: arr_h(:)
            real(real64), intent(in) :: arr_thetaI(:)

        end subroutine Update_Kflh_Base_Impedance_BC

        module subroutine Update_Kflh_Base_Viscosity_BC(self, arr_h, arr_Temperature)
            implicit none
            class(Type_HCF_Base_Viscosity_BC), intent(inout) :: self
            real(real64), intent(in) :: arr_h(:)
            real(real64), intent(in) :: arr_Temperature(:)

        end subroutine Update_Kflh_Base_Viscosity_BC

        module subroutine Update_Kflh_Base_Impedance_Viscosity_BC(self, arr_h, arr_thetaI, arr_Temperature)
            implicit none
            class(Type_HCF_Base_Impedance_Viscosity_BC), intent(inout) :: self
            real(real64), intent(in) :: arr_h(:)
            real(real64), intent(in) :: arr_thetaI(:)
            real(real64), intent(in) :: arr_Temperature(:)

        end subroutine Update_Kflh_Base_Impedance_Viscosity_BC

        module function Construct_Type_HCF_Base_VG(Ks, alpha1, n1, l, nsize) result(structure_HCF)
            implicit none
            real(real64), intent(in) :: Ks
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1
            real(real64), intent(in) :: l
            integer(int32), intent(in) :: nsize
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_VG

        module function Construct_Type_HCF_Base_VG_minimal() result(structure_HCF)
            implicit none
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_VG_minimal

        module function Construct_Type_HCF_Base_Impedance_VG(Ks, alpha1, n1, l, Omega, nsize) result(structure_HCF)
            implicit none
            real(real64), intent(in) :: Ks
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1
            real(real64), intent(in) :: l
            real(real64), intent(in) :: Omega
            integer(int32), intent(in) :: nsize
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Impedance_VG

        module function Construct_Type_HCF_Base_Impedance_VG_minimal() result(structure_HCF)
            implicit none
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Impedance_VG_minimal

        module function Construct_Type_HCF_Base_Viscosity_VG(Ks, alpha1, n1, l, useViscosity, nsize) result(structure_HCF)
            implicit none
            real(real64), intent(in) :: Ks
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1
            real(real64), intent(in) :: l
            integer(int32), intent(in) :: useViscosity
            integer(int32), intent(in) :: nsize
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Viscosity_VG

        module function Construct_Type_HCF_Base_Viscosity_VG_minimal() result(structure_HCF)
            implicit none
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Viscosity_VG_minimal

        module function Construct_Type_HCF_Base_Impedance_Viscosity_VG(Ks, alpha1, n1, l, Omega, useViscosity, nsize) result(structure_HCF)
            implicit none
            real(real64), intent(in) :: Ks
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1
            real(real64), intent(in) :: l
            real(real64), intent(in) :: Omega
            integer(int32), intent(in) :: useViscosity
            integer(int32), intent(in) :: nsize
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Impedance_Viscosity_VG

        module function Construct_Type_HCF_Base_Impedance_Viscosity_VG_minimal() result(structure_HCF)
            implicit none
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Impedance_Viscosity_VG_minimal

        module function Calculate_kr_VG_Base(alpha1, n1, m1, l, h) result(kr)
            implicit none
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1
            real(real64), intent(in) :: m1
            real(real64), intent(in) :: l
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function Calculate_kr_VG_Base

        module function Calculate_kr_Base_VG(self, h) result(kr)
            implicit none
            class(Type_HCF_Base_VG), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function Calculate_kr_Base_VG

        module function Calculate_kr_Base_Impedance_VG(self, h) result(kr)
            implicit none
            class(Type_HCF_Base_Impedance_VG), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function Calculate_kr_Base_Impedance_VG

        module function Calculate_kr_Base_Viscosity_VG(self, h) result(kr)
            implicit none
            class(Type_HCF_Base_Viscosity_VG), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function Calculate_kr_Base_Viscosity_VG

        module function Calculate_kr_Base_Impedance_Viscosity_VG(self, h) result(kr)
            implicit none
            class(Type_HCF_Base_Impedance_Viscosity_VG), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function Calculate_kr_Base_Impedance_Viscosity_VG

        module function Calculate_Kflh_Base_VG(self, h) result(Kflh)
            implicit none
            class(Type_HCF_Base_VG), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: Kflh

        end function Calculate_Kflh_Base_VG

        module function Calculate_Kflh_Base_Impedance_VG(self, h, thetaI) result(Kflh)
            implicit none
            class(Type_HCF_Base_Impedance_VG), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(in) :: thetaI
            real(real64) :: Kflh

        end function Calculate_Kflh_Base_Impedance_VG

        module function Calculate_Kflh_Base_Viscosity_VG(self, h, Temperature) result(Kflh)
            implicit none
            class(Type_HCF_Base_Viscosity_VG), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(in) :: Temperature
            real(real64) :: Kflh

        end function Calculate_Kflh_Base_Viscosity_VG

        module function Calculate_Kflh_Base_Impedance_Viscosity_VG(self, h, thetaI, Temperature) result(Kflh)
            implicit none
            class(Type_HCF_Base_Impedance_Viscosity_VG), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(in) :: thetaI
            real(real64), intent(in) :: Temperature
            real(real64) :: Kflh

        end function Calculate_Kflh_Base_Impedance_Viscosity_VG

        module subroutine Update_Kflh_Base_VG(self, arr_h)
            implicit none
            class(Type_HCF_Base_VG), intent(inout) :: self
            real(real64), intent(in) :: arr_h(:)

        end subroutine Update_Kflh_Base_VG

        module subroutine Update_Kflh_Base_Impedance_VG(self, arr_h, arr_thetaI)
            implicit none
            class(Type_HCF_Base_Impedance_VG), intent(inout) :: self
            real(real64), intent(in) :: arr_h(:)
            real(real64), intent(in) :: arr_thetaI(:)

        end subroutine Update_Kflh_Base_Impedance_VG

        module subroutine Update_Kflh_Base_Viscosity_VG(self, arr_h, arr_Temperature)
            implicit none
            class(Type_HCF_Base_Viscosity_VG), intent(inout) :: self
            real(real64), intent(in) :: arr_h(:)
            real(real64), intent(in) :: arr_Temperature(:)

        end subroutine Update_Kflh_Base_Viscosity_VG

        module subroutine Update_Kflh_Base_Impedance_Viscosity_VG(self, arr_h, arr_thetaI, arr_Temperature)
            implicit none
            class(Type_HCF_Base_Impedance_Viscosity_VG), intent(inout) :: self
            real(real64), intent(in) :: arr_h(:)
            real(real64), intent(in) :: arr_thetaI(:)
            real(real64), intent(in) :: arr_Temperature(:)

        end subroutine Update_Kflh_Base_Impedance_Viscosity_VG

        module function Construct_Type_HCF_Base_KO(Ks, alpha1, n1, l, nsize) result(structure_HCF)
            implicit none
            real(real64), intent(in) :: Ks
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1
            real(real64), intent(in) :: l
            integer(int32), intent(in) :: nsize
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_KO

        module function Construct_Type_HCF_Base_KO_minimal() result(structure_HCF)
            implicit none
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_KO_minimal

        module function Construct_Type_HCF_Base_Impedance_KO(Ks, alpha1, n1, l, Omega, nsize) result(structure_HCF)
            implicit none
            real(real64), intent(in) :: Ks
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1
            real(real64), intent(in) :: l
            real(real64), intent(in) :: Omega
            integer(int32), intent(in) :: nsize
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Impedance_KO

        module function Construct_Type_HCF_Base_Impedance_KO_minimal() result(structure_HCF)
            implicit none
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Impedance_KO_minimal

        module function Construct_Type_HCF_Base_Viscosity_KO(Ks, alpha1, n1, l, useViscosity, nsize) result(structure_HCF)
            implicit none
            real(real64), intent(in) :: Ks
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1
            real(real64), intent(in) :: l
            integer(int32), intent(in) :: useViscosity
            integer(int32), intent(in) :: nsize
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Viscosity_KO

        module function Construct_Type_HCF_Base_Viscosity_KO_minimal() result(structure_HCF)
            implicit none
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Viscosity_KO_minimal

        module function Construct_Type_HCF_Base_Impedance_Viscosity_KO(Ks, alpha1, n1, l, Omega, useViscosity, nsize) result(structure_HCF)
            implicit none
            real(real64), intent(in) :: Ks
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1
            real(real64), intent(in) :: l
            real(real64), intent(in) :: Omega
            integer(int32), intent(in) :: useViscosity
            integer(int32), intent(in) :: nsize
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Impedance_Viscosity_KO

        module function Construct_Type_HCF_Base_Impedance_Viscosity_KO_minimal() result(structure_HCF)
            implicit none
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Impedance_Viscosity_KO_minimal

        module function Calculate_kr_KO_Base(alpha1, n1, l, h) result(kr)
            implicit none
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1
            real(real64), intent(in) :: l
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function Calculate_kr_KO_Base

        module function Calculate_kr_Base_KO(self, h) result(kr)
            implicit none
            class(Type_HCF_Base_KO), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function Calculate_kr_Base_KO

        module function Calculate_kr_Base_Impedance_KO(self, h) result(kr)
            implicit none
            class(Type_HCF_Base_Impedance_KO), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function Calculate_kr_Base_Impedance_KO

        module function Calculate_kr_Base_Viscosity_KO(self, h) result(kr)
            implicit none
            class(Type_HCF_Base_Viscosity_KO), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function Calculate_kr_Base_Viscosity_KO

        module function Calculate_kr_Base_Impedance_Viscosity_KO(self, h) result(kr)
            implicit none
            class(Type_HCF_Base_Impedance_Viscosity_KO), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function Calculate_kr_Base_Impedance_Viscosity_KO

        module function Calculate_Kflh_Base_KO(self, h) result(Kflh)
            implicit none
            class(Type_HCF_Base_KO), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: Kflh

        end function Calculate_Kflh_Base_KO

        module function Calculate_Kflh_Base_Impedance_KO(self, h, thetaI) result(Kflh)
            implicit none
            class(Type_HCF_Base_Impedance_KO), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(in) :: thetaI
            real(real64) :: Kflh

        end function Calculate_Kflh_Base_Impedance_KO

        module function Calculate_Kflh_Base_Viscosity_KO(self, h, Temperature) result(Kflh)
            implicit none
            class(Type_HCF_Base_Viscosity_KO), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(in) :: Temperature
            real(real64) :: Kflh

        end function Calculate_Kflh_Base_Viscosity_KO

        module function Calculate_Kflh_Base_Impedance_Viscosity_KO(self, h, thetaI, Temperature) result(Kflh)
            implicit none
            class(Type_HCF_Base_Impedance_Viscosity_KO), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(in) :: thetaI
            real(real64), intent(in) :: Temperature
            real(real64) :: Kflh

        end function Calculate_Kflh_Base_Impedance_Viscosity_KO

        module subroutine Update_Kflh_Base_KO(self, arr_h)
            implicit none
            class(Type_HCF_Base_KO), intent(inout) :: self
            real(real64), intent(in) :: arr_h(:)

        end subroutine Update_Kflh_Base_KO

        module subroutine Update_Kflh_Base_Impedance_KO(self, arr_h, arr_thetaI)
            implicit none
            class(Type_HCF_Base_Impedance_KO), intent(inout) :: self
            real(real64), intent(in) :: arr_h(:)
            real(real64), intent(in) :: arr_thetaI(:)

        end subroutine Update_Kflh_Base_Impedance_KO

        module subroutine Update_Kflh_Base_Viscosity_KO(self, arr_h, arr_Temperature)
            implicit none
            class(Type_HCF_Base_Viscosity_KO), intent(inout) :: self
            real(real64), intent(in) :: arr_h(:)
            real(real64), intent(in) :: arr_Temperature(:)

        end subroutine Update_Kflh_Base_Viscosity_KO

        module subroutine Update_Kflh_Base_Impedance_Viscosity_KO(self, arr_h, arr_thetaI, arr_Temperature)
            implicit none
            class(Type_HCF_Base_Impedance_Viscosity_KO), intent(inout) :: self
            real(real64), intent(in) :: arr_h(:)
            real(real64), intent(in) :: arr_thetaI(:)
            real(real64), intent(in) :: arr_Temperature(:)

        end subroutine Update_Kflh_Base_Impedance_Viscosity_KO

        module function Construct_Type_HCF_Base_MVG(Ks, thetaS, thetaR, alpha1, n1, l, hcrit, nsize) result(structure_HCF)
            implicit none
            real(real64), intent(in) :: Ks
            real(real64), intent(in) :: thetaS
            real(real64), intent(in) :: thetaR
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1
            real(real64), intent(in) :: hcrit
            real(real64), intent(in) :: l
            integer(int32), intent(in) :: nsize
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_MVG

        module function Construct_Type_HCF_Base_MVG_minimal() result(structure_HCF)
            implicit none
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_MVG_minimal

        module function Construct_Type_HCF_Base_Impedance_MVG(Ks, thetaS, thetaR, alpha1, n1, l, hcrit, Omega, nsize) result(structure_HCF)
            implicit none
            real(real64), intent(in) :: Ks
            real(real64), intent(in) :: thetaS
            real(real64), intent(in) :: thetaR
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1
            real(real64), intent(in) :: l
            real(real64), intent(in) :: hcrit
            real(real64), intent(in) :: Omega
            integer(int32), intent(in) :: nsize
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Impedance_MVG

        module function Construct_Type_HCF_Base_Impedance_MVG_minimal() result(structure_HCF)
            implicit none
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Impedance_MVG_minimal

        module function Construct_Type_HCF_Base_Viscosity_MVG(Ks, thetaS, thetaR, alpha1, n1, l, hcrit, useViscosity, nsize) result(structure_HCF)
            implicit none
            real(real64), intent(in) :: Ks
            real(real64), intent(in) :: thetaS
            real(real64), intent(in) :: thetaR
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1
            real(real64), intent(in) :: l
            real(real64), intent(in) :: hcrit
            integer(int32), intent(in) :: useViscosity
            integer(int32), intent(in) :: nsize
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Viscosity_MVG

        module function Construct_Type_HCF_Base_Viscosity_MVG_minimal() result(structure_HCF)
            implicit none
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Viscosity_MVG_minimal

        module function Construct_Type_HCF_Base_Impedance_Viscosity_MVG(Ks, thetaS, thetaR, alpha1, n1, l, hcrit, Omega, useViscosity, nsize) result(structure_HCF)
            implicit none
            real(real64), intent(in) :: Ks
            real(real64), intent(in) :: thetaS
            real(real64), intent(in) :: thetaR
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1
            real(real64), intent(in) :: l
            real(real64), intent(in) :: hcrit
            real(real64), intent(in) :: Omega
            integer(int32), intent(in) :: useViscosity
            integer(int32), intent(in) :: nsize
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Impedance_Viscosity_MVG

        module function Construct_Type_HCF_Base_Impedance_Viscosity_MVG_minimal() result(structure_HCF)
            implicit none
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Impedance_Viscosity_MVG_minimal

        module function Calculate_kr_MVG_Base(thetaS, thetaR, alpha1, n1, m1, l, hcrit, h) result(kr)
            implicit none
            real(real64), intent(in) :: thetaS
            real(real64), intent(in) :: thetaR
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1
            real(real64), intent(in) :: m1
            real(real64), intent(in) :: l
            real(real64), intent(in) :: hcrit
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function Calculate_kr_MVG_Base

        module function Calculate_kr_Base_MVG(self, h) result(kr)
            implicit none
            class(Type_HCF_Base_MVG), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function Calculate_kr_Base_MVG

        module function Calculate_kr_Base_Impedance_MVG(self, h) result(kr)
            implicit none
            class(Type_HCF_Base_Impedance_MVG), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function Calculate_kr_Base_Impedance_MVG

        module function Calculate_kr_Base_Viscosity_MVG(self, h) result(kr)
            implicit none
            class(Type_HCF_Base_Viscosity_MVG), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function Calculate_kr_Base_Viscosity_MVG

        module function Calculate_kr_Base_Impedance_Viscosity_MVG(self, h) result(kr)
            implicit none
            class(Type_HCF_Base_Impedance_Viscosity_MVG), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function Calculate_kr_Base_Impedance_Viscosity_MVG

        module function Calculate_Kflh_Base_MVG(self, h) result(Kflh)
            implicit none
            class(Type_HCF_Base_MVG), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: Kflh

        end function Calculate_Kflh_Base_MVG

        module function Calculate_Kflh_Base_Impedance_MVG(self, h, thetaI) result(Kflh)
            implicit none
            class(Type_HCF_Base_Impedance_MVG), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(in) :: thetaI
            real(real64) :: Kflh

        end function Calculate_Kflh_Base_Impedance_MVG

        module function Calculate_Kflh_Base_Viscosity_MVG(self, h, Temperature) result(Kflh)
            implicit none
            class(Type_HCF_Base_Viscosity_MVG), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(in) :: Temperature
            real(real64) :: Kflh

        end function Calculate_Kflh_Base_Viscosity_MVG

        module function Calculate_Kflh_Base_Impedance_Viscosity_MVG(self, h, thetaI, Temperature) result(Kflh)
            implicit none
            class(Type_HCF_Base_Impedance_Viscosity_MVG), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(in) :: thetaI
            real(real64), intent(in) :: Temperature
            real(real64) :: Kflh

        end function Calculate_Kflh_Base_Impedance_Viscosity_MVG

        module subroutine Update_Kflh_Base_MVG(self, arr_h)
            implicit none
            class(Type_HCF_Base_MVG), intent(inout) :: self
            real(real64), intent(in) :: arr_h(:)

        end subroutine Update_Kflh_Base_MVG

        module subroutine Update_Kflh_Base_Impedance_MVG(self, arr_h, arr_thetaI)
            implicit none
            class(Type_HCF_Base_Impedance_MVG), intent(inout) :: self
            real(real64), intent(in) :: arr_h(:)
            real(real64), intent(in) :: arr_thetaI(:)

        end subroutine Update_Kflh_Base_Impedance_MVG

        module subroutine Update_Kflh_Base_Viscosity_MVG(self, arr_h, arr_Temperature)
            implicit none
            class(Type_HCF_Base_Viscosity_MVG), intent(inout) :: self
            real(real64), intent(in) :: arr_h(:)
            real(real64), intent(in) :: arr_Temperature(:)

        end subroutine Update_Kflh_Base_Viscosity_MVG

        module subroutine Update_Kflh_Base_Impedance_Viscosity_MVG(self, arr_h, arr_thetaI, arr_Temperature)
            implicit none
            class(Type_HCF_Base_Impedance_Viscosity_MVG), intent(inout) :: self
            real(real64), intent(in) :: arr_h(:)
            real(real64), intent(in) :: arr_thetaI(:)
            real(real64), intent(in) :: arr_Temperature(:)

        end subroutine Update_Kflh_Base_Impedance_Viscosity_MVG

        module function Construct_Type_HCF_Base_Durner(Ks, alpha1, n1, w1, alpha2, n2, l, nsize) result(structure_HCF)
            implicit none
            real(real64), intent(in) :: Ks
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1
            real(real64), intent(in) :: w1
            real(real64), intent(in) :: alpha2
            real(real64), intent(in) :: n2
            real(real64), intent(in) :: l
            integer(int32), intent(in) :: nsize
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Durner

        module function Construct_Type_HCF_Base_Durner_minimal() result(structure_HCF)
            implicit none
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Durner_minimal

        module function Construct_Type_HCF_Base_Impedance_Durner(Ks, alpha1, n1, w1, alpha2, n2, l, Omega, nsize) result(structure_HCF)
            implicit none
            real(real64), intent(in) :: Ks
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1
            real(real64), intent(in) :: w1
            real(real64), intent(in) :: alpha2
            real(real64), intent(in) :: n2
            real(real64), intent(in) :: l
            real(real64), intent(in) :: Omega
            integer(int32), intent(in) :: nsize
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Impedance_Durner

        module function Construct_Type_HCF_Base_Impedance_Durner_minimal() result(structure_HCF)
            implicit none
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Impedance_Durner_minimal

        module function Construct_Type_HCF_Base_Viscosity_Durner(Ks, alpha1, n1, w1, alpha2, n2, l, useViscosity, nsize) result(structure_HCF)
            implicit none
            real(real64), intent(in) :: Ks
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1
            real(real64), intent(in) :: w1
            real(real64), intent(in) :: alpha2
            real(real64), intent(in) :: n2
            real(real64), intent(in) :: l
            integer(int32), intent(in) :: useViscosity
            integer(int32), intent(in) :: nsize
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Viscosity_Durner

        module function Construct_Type_HCF_Base_Viscosity_Durner_minimal() result(structure_HCF)
            implicit none
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Viscosity_Durner_minimal

        module function Construct_Type_HCF_Base_Impedance_Viscosity_Durner(Ks, alpha1, n1, w1, alpha2, n2, l, Omega, useViscosity, nsize) result(structure_HCF)
            implicit none
            real(real64), intent(in) :: Ks
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1
            real(real64), intent(in) :: w1
            real(real64), intent(in) :: alpha2
            real(real64), intent(in) :: n2
            real(real64), intent(in) :: l
            real(real64), intent(in) :: Omega
            integer(int32), intent(in) :: useViscosity
            integer(int32), intent(in) :: nsize
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Impedance_Viscosity_Durner

        module function Construct_Type_HCF_Base_Impedance_Viscosity_Durner_minimal() result(structure_HCF)
            implicit none
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Impedance_Viscosity_Durner_minimal

        module function Calculate_kr_Durner_Base(alpha1, n1, m1, w1, alpha2, n2, m2, w2, l, h) result(kr)
            implicit none
            real(real64), intent(in) :: alpha1, alpha2
            real(real64), intent(in) :: n1, n2
            real(real64), intent(in) :: m1, m2
            real(real64), intent(in) :: w1, w2
            real(real64), intent(in) :: l
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function Calculate_kr_Durner_Base

        module function Calculate_kr_Base_Durner(self, h) result(kr)
            implicit none
            class(Type_HCF_Base_Durner), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function Calculate_kr_Base_Durner

        module function Calculate_kr_Base_Impedance_Durner(self, h) result(kr)
            implicit none
            class(Type_HCF_Base_Impedance_Durner), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function Calculate_kr_Base_Impedance_Durner

        module function Calculate_kr_Base_Viscosity_Durner(self, h) result(kr)
            implicit none
            class(Type_HCF_Base_Viscosity_Durner), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function Calculate_kr_Base_Viscosity_Durner

        module function Calculate_kr_Base_Impedance_Viscosity_Durner(self, h) result(kr)
            implicit none
            class(Type_HCF_Base_Impedance_Viscosity_Durner), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function Calculate_kr_Base_Impedance_Viscosity_Durner

        module function Calculate_Kflh_Base_Durner(self, h) result(Kflh)
            implicit none
            class(Type_HCF_Base_Durner), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: Kflh

        end function Calculate_Kflh_Base_Durner

        module function Calculate_Kflh_Base_Impedance_Durner(self, h, thetaI) result(Kflh)
            implicit none
            class(Type_HCF_Base_Impedance_Durner), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(in) :: thetaI
            real(real64) :: Kflh

        end function Calculate_Kflh_Base_Impedance_Durner

        module function Calculate_Kflh_Base_Viscosity_Durner(self, h, Temperature) result(Kflh)
            implicit none
            class(Type_HCF_Base_Viscosity_Durner), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(in) :: Temperature
            real(real64) :: Kflh

        end function Calculate_Kflh_Base_Viscosity_Durner

        module function Calculate_Kflh_Base_Impedance_Viscosity_Durner(self, h, thetaI, Temperature) result(Kflh)
            implicit none
            class(Type_HCF_Base_Impedance_Viscosity_Durner), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(in) :: thetaI
            real(real64), intent(in) :: Temperature
            real(real64) :: Kflh

        end function Calculate_Kflh_Base_Impedance_Viscosity_Durner

        module subroutine Update_Kflh_Base_Durner(self, arr_h)
            implicit none
            class(Type_HCF_Base_Durner), intent(inout) :: self
            real(real64), intent(in) :: arr_h(:)

        end subroutine Update_Kflh_Base_Durner

        module subroutine Update_Kflh_Base_Impedance_Durner(self, arr_h, arr_thetaI)
            implicit none
            class(Type_HCF_Base_Impedance_Durner), intent(inout) :: self
            real(real64), intent(in) :: arr_h(:)
            real(real64), intent(in) :: arr_thetaI(:)

        end subroutine Update_Kflh_Base_Impedance_Durner

        module subroutine Update_Kflh_Base_Viscosity_Durner(self, arr_h, arr_Temperature)
            implicit none
            class(Type_HCF_Base_Viscosity_Durner), intent(inout) :: self
            real(real64), intent(in) :: arr_h(:)
            real(real64), intent(in) :: arr_Temperature(:)

        end subroutine Update_Kflh_Base_Viscosity_Durner

        module subroutine Update_Kflh_Base_Impedance_Viscosity_Durner(self, arr_h, arr_thetaI, arr_Temperature)
            implicit none
            class(Type_HCF_Base_Impedance_Viscosity_Durner), intent(inout) :: self
            real(real64), intent(in) :: arr_h(:)
            real(real64), intent(in) :: arr_thetaI(:)
            real(real64), intent(in) :: arr_Temperature(:)

        end subroutine Update_Kflh_Base_Impedance_Viscosity_Durner

        module function Construct_Type_HCF_Base_DVGCH(Ks, alpha1, n1, w1, n2, l, nsize) result(structure_HCF)
            implicit none
            real(real64), intent(in) :: Ks
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1
            real(real64), intent(in) :: n2
            real(real64), intent(in) :: w1
            real(real64), intent(in) :: l
            integer(int32), intent(in) :: nsize
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_DVGCH

        module function Construct_Type_HCF_Base_DVGCH_minimal() result(structure_HCF)
            implicit none
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_DVGCH_minimal

        module function Construct_Type_HCF_Base_Impedance_DVGCH(Ks, alpha1, n1, w1, n2, l, Omega, nsize) result(structure_HCF)
            implicit none
            real(real64), intent(in) :: Ks
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1
            real(real64), intent(in) :: n2
            real(real64), intent(in) :: w1
            real(real64), intent(in) :: l
            real(real64), intent(in) :: Omega
            integer(int32), intent(in) :: nsize
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Impedance_DVGCH

        module function Construct_Type_HCF_Base_Impedance_DVGCH_minimal() result(structure_HCF)
            implicit none
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Impedance_DVGCH_minimal

        module function Construct_Type_HCF_Base_Viscosity_DVGCH(Ks, alpha1, n1, w1, n2, l, useViscosity, nsize) result(structure_HCF)
            implicit none
            real(real64), intent(in) :: Ks
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1
            real(real64), intent(in) :: n2
            real(real64), intent(in) :: w1
            real(real64), intent(in) :: l
            integer(int32), intent(in) :: useViscosity
            integer(int32), intent(in) :: nsize
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Viscosity_DVGCH

        module function Construct_Type_HCF_Base_Viscosity_DVGCH_minimal() result(structure_HCF)
            implicit none
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Viscosity_DVGCH_minimal

        module function Construct_Type_HCF_Base_Impedance_Viscosity_DVGCH(Ks, alpha1, n1, w1, n2, l, Omega, useViscosity, nsize) result(structure_HCF)
            implicit none
            real(real64), intent(in) :: Ks
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1
            real(real64), intent(in) :: n2
            real(real64), intent(in) :: w1
            real(real64), intent(in) :: l
            real(real64), intent(in) :: Omega
            integer(int32), intent(in) :: useViscosity
            integer(int32), intent(in) :: nsize
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Impedance_Viscosity_DVGCH

        module function Construct_Type_HCF_Base_Impedance_Viscosity_DVGCH_minimal() result(structure_HCF)
            implicit none
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Base_Impedance_Viscosity_DVGCH_minimal

        module function Calculate_kr_DVGCH_Base(alpha1, n1, m1, w1, n2, m2, w2, l, h) result(kr)
            implicit none
            real(real64), intent(in) :: alpha1
            real(real64), intent(in) :: n1, n2
            real(real64), intent(in) :: m1, m2
            real(real64), intent(in) :: w1, w2
            real(real64), intent(in) :: l
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function Calculate_kr_DVGCH_Base

        module function Calculate_kr_Base_DVGCH(self, h) result(kr)
            implicit none
            class(Type_HCF_Base_DVGCH), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function Calculate_kr_Base_DVGCH

        module function Calculate_kr_Base_Impedance_DVGCH(self, h) result(kr)
            implicit none
            class(Type_HCF_Base_Impedance_DVGCH), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function Calculate_kr_Base_Impedance_DVGCH

        module function Calculate_kr_Base_Viscosity_DVGCH(self, h) result(kr)
            implicit none
            class(Type_HCF_Base_Viscosity_DVGCH), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function Calculate_kr_Base_Viscosity_DVGCH

        module function Calculate_kr_Base_Impedance_Viscosity_DVGCH(self, h) result(kr)
            implicit none
            class(Type_HCF_Base_Impedance_Viscosity_DVGCH), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: kr

        end function Calculate_kr_Base_Impedance_Viscosity_DVGCH

        module function Calculate_Kflh_Base_DVGCH(self, h) result(Kflh)
            implicit none
            class(Type_HCF_Base_DVGCH), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: Kflh

        end function Calculate_Kflh_Base_DVGCH

        module function Calculate_Kflh_Base_Impedance_DVGCH(self, h, thetaI) result(Kflh)
            implicit none
            class(Type_HCF_Base_Impedance_DVGCH), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(in) :: thetaI
            real(real64) :: Kflh

        end function Calculate_Kflh_Base_Impedance_DVGCH

        module function Calculate_Kflh_Base_Viscosity_DVGCH(self, h, Temperature) result(Kflh)
            implicit none
            class(Type_HCF_Base_Viscosity_DVGCH), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(in) :: Temperature
            real(real64) :: Kflh

        end function Calculate_Kflh_Base_Viscosity_DVGCH

        module function Calculate_Kflh_Base_Impedance_Viscosity_DVGCH(self, h, thetaI, Temperature) result(Kflh)
            implicit none
            class(Type_HCF_Base_Impedance_Viscosity_DVGCH), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64), intent(in) :: thetaI
            real(real64), intent(in) :: Temperature
            real(real64) :: Kflh

        end function Calculate_Kflh_Base_Impedance_Viscosity_DVGCH

        module subroutine Update_Kflh_Base_DVGCH(self, arr_h)
            implicit none
            class(Type_HCF_Base_DVGCH), intent(inout) :: self
            real(real64), intent(in) :: arr_h(:)

        end subroutine Update_Kflh_Base_DVGCH

        module subroutine Update_Kflh_Base_Impedance_DVGCH(self, arr_h, arr_thetaI)
            implicit none
            class(Type_HCF_Base_Impedance_DVGCH), intent(inout) :: self
            real(real64), intent(in) :: arr_h(:)
            real(real64), intent(in) :: arr_thetaI(:)

        end subroutine Update_Kflh_Base_Impedance_DVGCH

        module subroutine Update_Kflh_Base_Viscosity_DVGCH(self, arr_h, arr_Temperature)
            implicit none
            class(Type_HCF_Base_Viscosity_DVGCH), intent(inout) :: self
            real(real64), intent(in) :: arr_h(:)
            real(real64), intent(in) :: arr_Temperature(:)

        end subroutine Update_Kflh_Base_Viscosity_DVGCH

        module subroutine Update_Kflh_Base_Impedance_Viscosity_DVGCH(self, arr_h, arr_thetaI, arr_Temperature)
            implicit none
            class(Type_HCF_Base_Impedance_Viscosity_DVGCH), intent(inout) :: self
            real(real64), intent(in) :: arr_h(:)
            real(real64), intent(in) :: arr_thetaI(:)
            real(real64), intent(in) :: arr_Temperature(:)

        end subroutine Update_Kflh_Base_Impedance_Viscosity_DVGCH

        module function Construct_Type_HCF_Impedance(Ks, Omega, nsize) result(structure_HCF)
            implicit none
            real(real64), intent(in) :: Ks
            real(real64), intent(in) :: Omega
            integer(int32), intent(in) :: nsize
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Impedance

        module function Construct_Type_HCF_Impedance_minimal() result(structure_HCF)
            implicit none
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Impedance_minimal

        module function Calculate_Impedance_Base(Omega, thetaI) result(Impedance)
            implicit none
            real(real64), intent(in) :: Omega
            real(real64), intent(in) :: thetaI
            real(real64) :: Impedance

        end function Calculate_Impedance_Base

        module function Calculate_Kflh_Impedance(self, thetaI) result(Kflh)
            implicit none
            class(Type_HCF_Impedance), intent(in) :: self
            real(real64), intent(in) :: thetaI
            real(real64) :: Kflh

        end function Calculate_Kflh_Impedance

        module subroutine Update_Kflh_Impedance(self, arr_thetaI)
            implicit none
            class(Type_HCF_Impedance), intent(inout) :: self
            real(real64), intent(in) :: arr_thetaI(:)

        end subroutine Update_Kflh_Impedance

        module function Construct_Type_HCF_Viscosity(Ks, useViscosity, nsize) result(structure_HCF)
            implicit none
            real(real64), intent(in) :: Ks
            integer(int32), intent(in) :: useViscosity
            integer(int32), intent(in) :: nsize
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Viscosity

        module function Construct_Type_HCF_Viscosity_minimal() result(structure_HCF)
            implicit none
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Viscosity_minimal

        module subroutine Set_Calculate_Viscosity_Base(Calculate_Viscosity_Type, Calculate_Viscosity)
            implicit none
            integer(int32), intent(in) :: Calculate_Viscosity_Type
            procedure(Abstract_Calculate_Viscosity), pointer, intent(inout) :: Calculate_Viscosity

        end subroutine Set_Calculate_Viscosity_Base

        module function Calculate_HCF_mu_Exponential(Temperature) result(Viscosity)
            implicit none
            real(real64), intent(in) :: Temperature
            real(real64) :: Viscosity

        end function Calculate_HCF_mu_Exponential

        module function Calculate_HCF_mu_Exponential_Supercooled(Temperature) result(Viscosity)
            implicit none
            real(real64), intent(in) :: Temperature
            real(real64) :: Viscosity

        end function Calculate_HCF_mu_Exponential_Supercooled

        module function Calculate_Kflh_Viscosity(self, Temperature) result(Kflh)
            implicit none
            class(Type_HCF_Viscosity), intent(in) :: self
            real(real64), intent(in) :: Temperature
            real(real64) :: Kflh

        end function Calculate_Kflh_Viscosity

        module subroutine Update_Kflh_Viscosity(self, arr_Temperature)
            implicit none
            class(Type_HCF_Viscosity), intent(inout) :: self
            real(real64), intent(in) :: arr_Temperature(:)

        end subroutine Update_Kflh_Viscosity

        module function Construct_Type_HCF_Impedance_Viscosity(Ks, Omega, useViscosity, nsize) result(structure_HCF)
            implicit none
            real(real64), intent(in) :: Ks
            real(real64), intent(in) :: Omega
            integer(int32), intent(in) :: useViscosity
            integer(int32), intent(in) :: nsize
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Impedance_Viscosity

        module function Construct_Type_HCF_Impedance_Viscosity_minimal() result(structure_HCF)
            implicit none
            class(Abstract_HCF), allocatable :: structure_HCF

        end function Construct_Type_HCF_Impedance_Viscosity_minimal

        module function Calculate_Kflh_Impedance_Viscosity(self, thetaI, Temperature) result(Kflh)
            implicit none
            class(Type_HCF_Impedance_Viscosity), intent(in) :: self
            real(real64), intent(in) :: thetaI
            real(real64), intent(in) :: Temperature
            real(real64) :: Kflh

        end function Calculate_Kflh_Impedance_Viscosity

        module subroutine Update_Kflh_Impedance_Viscosity(self, arr_thetaI, arr_Temperature)
            implicit none
            class(Type_HCF_Impedance_Viscosity), intent(inout) :: self
            real(real64), intent(in) :: arr_thetaI(:)
            real(real64), intent(in) :: arr_Temperature(:)

        end subroutine Update_Kflh_Impedance_Viscosity

    end interface

    interface Type_HCF_Base_BC
        module procedure Construct_Type_HCF_Base_BC
        module procedure Construct_Type_HCF_Base_BC_minimal
    end interface

    interface Type_HCF_Base_Impedance_BC
        module procedure Construct_Type_HCF_Base_Impedance_BC
        module procedure Construct_Type_HCF_Base_Impedance_BC_minimal
    end interface

    interface Type_HCF_Base_Viscosity_BC
        module procedure Construct_Type_HCF_Base_Viscosity_BC
        module procedure Construct_Type_HCF_Base_Viscosity_BC_minimal
    end interface

    interface Type_HCF_Base_Impedance_Viscosity_BC
        module procedure Construct_Type_HCF_Base_Impedance_Viscosity_BC
        module procedure Construct_Type_HCF_Base_Impedance_Viscosity_BC_minimal
    end interface

    interface Type_HCF_Base_VG
        module procedure Construct_Type_HCF_Base_VG
        module procedure Construct_Type_HCF_Base_VG_minimal
    end interface

    interface Type_HCF_Base_Impedance_VG
        module procedure Construct_Type_HCF_Base_Impedance_VG
        module procedure Construct_Type_HCF_Base_Impedance_VG_minimal
    end interface

    interface Type_HCF_Base_Viscosity_VG
        module procedure Construct_Type_HCF_Base_Viscosity_VG
        module procedure Construct_Type_HCF_Base_Viscosity_VG_minimal
    end interface

    interface Type_HCF_Base_Impedance_Viscosity_VG
        module procedure Construct_Type_HCF_Base_Impedance_Viscosity_VG
        module procedure Construct_Type_HCF_Base_Impedance_Viscosity_VG_minimal
    end interface

    interface Type_HCF_Base_KO
        module procedure Construct_Type_HCF_Base_KO
        module procedure Construct_Type_HCF_Base_KO_minimal
    end interface

    interface Type_HCF_Base_Impedance_KO
        module procedure Construct_Type_HCF_Base_Impedance_KO
        module procedure Construct_Type_HCF_Base_Impedance_KO_minimal
    end interface

    interface Type_HCF_Base_Viscosity_KO
        module procedure Construct_Type_HCF_Base_Viscosity_KO
        module procedure Construct_Type_HCF_Base_Viscosity_KO_minimal
    end interface

    interface Type_HCF_Base_Impedance_Viscosity_KO
        module procedure Construct_Type_HCF_Base_Impedance_Viscosity_KO
        module procedure Construct_Type_HCF_Base_Impedance_Viscosity_KO_minimal
    end interface

    interface Type_HCF_Base_MVG
        module procedure Construct_Type_HCF_Base_MVG
        module procedure Construct_Type_HCF_Base_MVG_minimal
    end interface

    interface Type_HCF_Base_Impedance_MVG
        module procedure Construct_Type_HCF_Base_Impedance_MVG
        module procedure Construct_Type_HCF_Base_Impedance_MVG_minimal
    end interface

    interface Type_HCF_Base_Viscosity_MVG
        module procedure Construct_Type_HCF_Base_Viscosity_MVG
        module procedure Construct_Type_HCF_Base_Viscosity_MVG_minimal
    end interface

    interface Type_HCF_Base_Impedance_Viscosity_MVG
        module procedure Construct_Type_HCF_Base_Impedance_Viscosity_MVG
        module procedure Construct_Type_HCF_Base_Impedance_Viscosity_MVG_minimal
    end interface

    interface Type_HCF_Base_Durner
        module procedure Construct_Type_HCF_Base_Durner
        module procedure Construct_Type_HCF_Base_Durner_minimal
    end interface

    interface Type_HCF_Base_Impedance_Durner
        module procedure Construct_Type_HCF_Base_Impedance_Durner
        module procedure Construct_Type_HCF_Base_Impedance_Durner_minimal
    end interface

    interface Type_HCF_Base_Viscosity_Durner
        module procedure Construct_Type_HCF_Base_Viscosity_Durner
        module procedure Construct_Type_HCF_Base_Viscosity_Durner_minimal
    end interface

    interface Type_HCF_Base_Impedance_Viscosity_Durner
        module procedure Construct_Type_HCF_Base_Impedance_Viscosity_Durner
        module procedure Construct_Type_HCF_Base_Impedance_Viscosity_Durner_minimal
    end interface

    interface Type_HCF_Base_DVGCH
        module procedure Construct_Type_HCF_Base_DVGCH
        module procedure Construct_Type_HCF_Base_DVGCH_minimal
    end interface

    interface Type_HCF_Base_Impedance_DVGCH
        module procedure Construct_Type_HCF_Base_Impedance_DVGCH
        module procedure Construct_Type_HCF_Base_Impedance_DVGCH_minimal
    end interface

    interface Type_HCF_Base_Viscosity_DVGCH
        module procedure Construct_Type_HCF_Base_Viscosity_DVGCH
        module procedure Construct_Type_HCF_Base_Viscosity_DVGCH_minimal
    end interface

    interface Type_HCF_Base_Impedance_Viscosity_DVGCH
        module procedure Construct_Type_HCF_Base_Impedance_Viscosity_DVGCH
        module procedure Construct_Type_HCF_Base_Impedance_Viscosity_DVGCH_minimal
    end interface

    interface Type_HCF_Impedance
        module procedure Construct_Type_HCF_Impedance
        module procedure Construct_Type_HCF_Impedance_minimal
    end interface

    interface Type_HCF_Viscosity
        module procedure Construct_Type_HCF_Viscosity
        module procedure Construct_Type_HCF_Viscosity_minimal
    end interface

    interface Type_HCF_Impedance_Viscosity
        module procedure Construct_Type_HCF_Impedance_Viscosity
        module procedure Construct_Type_HCF_Impedance_Viscosity_minimal
    end interface
contains

end module Calculate_HCF
