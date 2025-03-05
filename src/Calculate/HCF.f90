module Calculate_HCF
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Allocate_Allocate, only:Allocate_Array
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

    interface Type_HCF
        module procedure Construct_Type_HCF
        module procedure Construct_Type_HCF_minimal
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

    function Construct_Type_HCF(useHCFType, Ks, thetaS, thetaR, alpha1, n1, w1, alpha2, n2, l, hcrit, Omega, useViscosity, nsize) result(structure_HCF)
        implicit none
        integer(int32), intent(in) :: useHCFType
        real(real64), intent(in) :: Ks
        real(real64), intent(in), optional :: thetaS
        real(real64), intent(in), optional :: thetaR
        real(real64), intent(in), optional :: alpha1
        real(real64), intent(in), optional :: n1
        real(real64), intent(in), optional :: w1
        real(real64), intent(in), optional :: alpha2
        real(real64), intent(in), optional :: n2
        real(real64), intent(in), optional :: l
        real(real64), intent(in), optional :: hcrit
        real(real64), intent(in), optional :: Omega
        integer(int32), intent(in), optional :: useViscosity
        integer(int32), intent(in) :: nsize
        class(Abstract_HCF), allocatable :: structure_HCF

        select case (useHCFType)
        case (11)
            if (.not. present(alpha1) .or. &
                .not. present(n1) .or. &
                .not. present(l) &
                ) stop "Missing parameters for HCF type 11"
            structure_HCF = Type_HCF_Base_BC(Ks=Ks, &
                                             alpha1=alpha1, &
                                             n1=n1, &
                                             l=l, &
                                             nsize=nsize)
        case (12)
            if (.not. present(alpha1) .or. &
                .not. present(n1) .or. &
                .not. present(l) &
                ) stop "Missing parameters for HCF type 12"
            structure_HCF = Type_HCF_Base_VG(Ks=Ks, &
                                             alpha1=alpha1, &
                                             n1=n1, &
                                             l=l, &
                                             nsize=nsize)
        case (13)
            if (.not. present(alpha1) .or. &
                .not. present(n1) .or. &
                .not. present(l) &
                ) stop "Missing parameters for HCF type 13"
            structure_HCF = Type_HCF_Base_KO(Ks=Ks, &
                                             alpha1=alpha1, &
                                             n1=n1, &
                                             l=l, &
                                             nsize=nsize)
        case (14)
            if (.not. present(thetaS) .or. &
                .not. present(thetaR) .or. &
                .not. present(alpha1) .or. &
                .not. present(n1) .or. &
                .not. present(l) .or. &
                .not. present(hcrit) &
                ) stop "Missing parameters for HCF type 14"
            structure_HCF = Type_HCF_Base_MVG(Ks=Ks, &
                                              thetaS=thetaS, &
                                              thetaR=thetaR, &
                                              alpha1=alpha1, &
                                              n1=n1, &
                                              l=l, &
                                              hcrit=hcrit, &
                                              nsize=nsize)
        case (15)
            if (.not. present(alpha1) .or. &
                .not. present(n1) .or. &
                .not. present(w1) .or. &
                .not. present(alpha2) .or. &
                .not. present(n2) .or. &
                .not. present(l) &
                ) stop "Missing parameters for HCF type 15"
            structure_HCF = Type_HCF_Base_Durner(Ks=Ks, &
                                                 alpha1=alpha1, &
                                                 n1=n1, &
                                                 w1=w1, &
                                                 alpha2=alpha2, &
                                                 n2=n2, &
                                                 l=l, &
                                                 nsize=nsize)
        case (16)
            if (.not. present(alpha1) .or. &
                .not. present(n1) .or. &
                .not. present(w1) .or. &
                .not. present(n2) .or. &
                .not. present(l) &
                ) stop "Missing parameters for HCF type 16"
            structure_HCF = Type_HCF_Base_DVGCH(Ks=Ks, &
                                                alpha1=alpha1, &
                                                n1=n1, &
                                                w1=w1, &
                                                n2=n2, &
                                                l=l, &
                                                nsize=nsize)
        case (21)
            if (.not. present(Omega)) stop "Missing Omega for HCF type 21"
            structure_HCF = Type_HCF_Impedance(Ks=Ks, &
                                               Omega=Omega, &
                                               nsize=nsize)
        case (31)
            if (.not. present(useViscosity)) stop "Missing useViscosity for HCF type 31"
            structure_HCF = Type_HCF_Viscosity(Ks=Ks, &
                                               useViscosity=useViscosity, &
                                               nsize=nsize)
        case (41)
            if (.not. present(Omega) .or. &
                .not. present(useViscosity) &
                ) stop "Missing parameters for HCF type 41"
            structure_HCF = Type_HCF_Impedance_Viscosity(Ks=Ks, &
                                                         Omega=Omega, &
                                                         useViscosity=useViscosity, &
                                                         nsize=nsize)
        case (51)
            if (.not. present(alpha1) .or. &
                .not. present(n1) .or. &
                .not. present(l) .or. &
                .not. present(Omega) &
                ) stop "Missing parameters for HCF type 51"
            structure_HCF = Type_HCF_Base_Impedance_BC(Ks=Ks, &
                                                       alpha1=alpha1, &
                                                       n1=n1, &
                                                       l=l, &
                                                       Omega=Omega, &
                                                       nsize=nsize)
        case (52)
            if (.not. present(alpha1) .or. &
                .not. present(n1) .or. &
                .not. present(l) .or. &
                .not. present(Omega) &
                ) stop "Missing parameters for HCF type 52"
            structure_HCF = Type_HCF_Base_Impedance_VG(Ks=Ks, &
                                                       alpha1=alpha1, &
                                                       n1=n1, &
                                                       l=l, &
                                                       Omega=Omega, &
                                                       nsize=nsize)
        case (53)
            if (.not. present(alpha1) .or. &
                .not. present(n1) .or. &
                .not. present(l) .or. &
                .not. present(Omega) &
                ) stop "Missing parameters for HCF type 53"
            structure_HCF = Type_HCF_Base_Impedance_KO(Ks=Ks, &
                                                       alpha1=alpha1, &
                                                       n1=n1, &
                                                       l=l, &
                                                       Omega=Omega, &
                                                       nsize=nsize)
        case (54)
            if (.not. present(thetaS) .or. &
                .not. present(thetaR) .or. &
                .not. present(alpha1) .or. &
                .not. present(n1) .or. &
                .not. present(l) .or. &
                .not. present(hcrit) .or. &
                .not. present(Omega) &
                ) stop "Missing parameters for HCF type 54"
            structure_HCF = Type_HCF_Base_Impedance_MVG(Ks=Ks, &
                                                        thetaS=thetaS, &
                                                        thetaR=thetaR, &
                                                        alpha1=alpha1, &
                                                        n1=n1, &
                                                        l=l, &
                                                        hcrit=hcrit, &
                                                        Omega=Omega, &
                                                        nsize=nsize)
        case (55)
            if (.not. present(alpha1) .or. &
                .not. present(n1) .or. &
                .not. present(w1) .or. &
                .not. present(alpha2) .or. &
                .not. present(n2) .or. &
                .not. present(l) .or. &
                .not. present(Omega) &
                ) stop "Missing parameters for HCF type 55"
            structure_HCF = Type_HCF_Base_Impedance_Durner(Ks=Ks, &
                                                           alpha1=alpha1, &
                                                           n1=n1, &
                                                           w1=w1, &
                                                           alpha2=alpha2, &
                                                           n2=n2, &
                                                           l=l, &
                                                           Omega=Omega, &
                                                           nsize=nsize)
        case (56)
            if (.not. present(alpha1) .or. &
                .not. present(n1) .or. &
                .not. present(w1) .or. &
                .not. present(n2) .or. &
                .not. present(l) .or. &
                .not. present(Omega) &
                ) stop "Missing parameters for HCF type 56"
            structure_HCF = Type_HCF_Base_Impedance_DVGCH(Ks=Ks, &
                                                          alpha1=alpha1, &
                                                          n1=n1, &
                                                          w1=w1, &
                                                          n2=n2, &
                                                          l=l, &
                                                          Omega=Omega, &
                                                          nsize=nsize)
        case (61)
            if (.not. present(alpha1) .or. &
                .not. present(n1) .or. &
                .not. present(l) .or. &
                .not. present(useViscosity) &
                ) stop "Missing parameters for HCF type 61"
            structure_HCF = Type_HCF_Base_Viscosity_BC(Ks=Ks, &
                                                       alpha1=alpha1, &
                                                       n1=n1, &
                                                       l=l, &
                                                       useViscosity=useViscosity, &
                                                       nsize=nsize)
        case (62)
            if (.not. present(alpha1) .or. &
                .not. present(n1) .or. &
                .not. present(l) .or. &
                .not. present(useViscosity) &
                ) stop "Missing parameters for HCF type 62"
            structure_HCF = Type_HCF_Base_Viscosity_VG(Ks=Ks, &
                                                       alpha1=alpha1, &
                                                       n1=n1, &
                                                       l=l, &
                                                       useViscosity=useViscosity, &
                                                       nsize=nsize)
        case (63)
            if (.not. present(alpha1) .or. &
                .not. present(n1) .or. &
                .not. present(l) .or. &
                .not. present(useViscosity) &
                ) stop "Missing parameters for HCF type 63"
            structure_HCF = Type_HCF_Base_Viscosity_KO(Ks=Ks, &
                                                       alpha1=alpha1, &
                                                       n1=n1, &
                                                       l=l, &
                                                       useViscosity=useViscosity, &
                                                       nsize=nsize)
        case (64)
            if (.not. present(thetaS) .or. &
                .not. present(thetaR) .or. &
                .not. present(alpha1) .or. &
                .not. present(n1) .or. &
                .not. present(l) .or. &
                .not. present(hcrit) .or. &
                .not. present(useViscosity) &
                ) stop "Missing parameters for HCF type 64"
            structure_HCF = Type_HCF_Base_Viscosity_MVG(Ks=Ks, &
                                                        thetaS=thetaS, &
                                                        thetaR=thetaR, &
                                                        alpha1=alpha1, &
                                                        n1=n1, &
                                                        l=l, &
                                                        hcrit=hcrit, &
                                                        useViscosity=useViscosity, &
                                                        nsize=nsize)
        case (65)
            if (.not. present(alpha1) .or. &
                .not. present(n1) .or. &
                .not. present(w1) .or. &
                .not. present(alpha2) .or. &
                .not. present(n2) .or. &
                .not. present(l) .or. &
                .not. present(useViscosity) &
                ) stop "Missing parameters for HCF type 65"
            structure_HCF = Type_HCF_Base_Viscosity_Durner(Ks=Ks, &
                                                           alpha1=alpha1, &
                                                           n1=n1, &
                                                           w1=w1, &
                                                           alpha2=alpha2, &
                                                           n2=n2, &
                                                           l=l, &
                                                           useViscosity=useViscosity, &
                                                           nsize=nsize)
        case (66)
            if (.not. present(alpha1) .or. &
                .not. present(n1) .or. &
                .not. present(w1) .or. &
                .not. present(n2) .or. &
                .not. present(l) .or. &
                .not. present(useViscosity) &
                ) stop "Missing parameters for HCF type 66"
            structure_HCF = Type_HCF_Base_Viscosity_DVGCH(Ks=Ks, &
                                                          alpha1=alpha1, &
                                                          n1=n1, &
                                                          w1=w1, &
                                                          n2=n2, &
                                                          l=l, &
                                                          useViscosity=useViscosity, &
                                                          nsize=nsize)
        case (71)
            if (.not. present(alpha1) .or. &
                .not. present(n1) .or. &
                .not. present(l) .or. &
                .not. present(Omega) .or. &
                .not. present(useViscosity) &
                ) stop "Missing parameters for HCF type 71"
            structure_HCF = Type_HCF_Base_Impedance_Viscosity_BC(Ks=Ks, &
                                                                 alpha1=alpha1, &
                                                                 n1=n1, &
                                                                 l=l, &
                                                                 Omega=Omega, &
                                                                 useViscosity=useViscosity, &
                                                                 nsize=nsize)
        case (72)
            if (.not. present(alpha1) .or. &
                .not. present(n1) .or. &
                .not. present(l) .or. &
                .not. present(Omega) .or. &
                .not. present(useViscosity) &
                ) stop "Missing parameters for HCF type 72"
            structure_HCF = Type_HCF_Base_Impedance_Viscosity_VG(Ks=Ks, &
                                                                 alpha1=alpha1, &
                                                                 n1=n1, &
                                                                 l=l, &
                                                                 Omega=Omega, &
                                                                 useViscosity=useViscosity, &
                                                                 nsize=nsize)
        case (73)
            if (.not. present(alpha1) .or. &
                .not. present(n1) .or. &
                .not. present(l) .or. &
                .not. present(Omega) .or. &
                .not. present(useViscosity) &
                ) stop "Missing parameters for HCF type 73"
            structure_HCF = Type_HCF_Base_Impedance_Viscosity_KO(Ks=Ks, &
                                                                 alpha1=alpha1, &
                                                                 n1=n1, &
                                                                 l=l, &
                                                                 Omega=Omega, &
                                                                 useViscosity=useViscosity, &
                                                                 nsize=nsize)
        case (74)
            if (.not. present(thetaS) .or. &
                .not. present(thetaR) .or. &
                .not. present(alpha1) .or. &
                .not. present(n1) .or. &
                .not. present(l) .or. &
                .not. present(hcrit) .or. &
                .not. present(Omega) .or. &
                .not. present(useViscosity) &
                ) stop "Missing parameters for HCF type 74"
            structure_HCF = Type_HCF_Base_Impedance_Viscosity_MVG(Ks=Ks, &
                                                                  thetaS=thetaS, &
                                                                  thetaR=thetaR, &
                                                                  alpha1=alpha1, &
                                                                  n1=n1, &
                                                                  l=l, &
                                                                  hcrit=hcrit, &
                                                                  Omega=Omega, &
                                                                  useViscosity=useViscosity, &
                                                                  nsize=nsize)
        case (75)
            if (.not. present(alpha1) .or. &
                .not. present(n1) .or. &
                .not. present(w1) .or. &
                .not. present(alpha2) .or. &
                .not. present(n2) .or. &
                .not. present(l) .or. &
                .not. present(Omega) .or. &
                .not. present(useViscosity) &
                ) stop "Missing parameters for HCF type 75"
            structure_HCF = Type_HCF_Base_Impedance_Viscosity_Durner(Ks=Ks, &
                                                                     alpha1=alpha1, &
                                                                     n1=n1, &
                                                                     w1=w1, &
                                                                     alpha2=alpha2, &
                                                                     n2=n2, &
                                                                     l=l, &
                                                                     Omega=Omega, &
                                                                     useViscosity=useViscosity, &
                                                                     nsize=nsize)

        case (76)
            if (.not. present(alpha1) .or. &
                .not. present(n1) .or. &
                .not. present(w1) .or. &
                .not. present(n2) .or. &
                .not. present(l) .or. &
                .not. present(Omega) .or. &
                .not. present(useViscosity) &
                ) stop "Missing parameters for HCF type 76"
            structure_HCF = Type_HCF_Base_Impedance_Viscosity_DVGCH(Ks=Ks, &
                                                                    alpha1=alpha1, &
                                                                    n1=n1, &
                                                                    w1=w1, &
                                                                    n2=n2, &
                                                                    l=l, &
                                                                    Omega=Omega, &
                                                                    useViscosity=useViscosity, &
                                                                    nsize=nsize)
        end select

    end function Construct_Type_HCF

    function Construct_Type_HCF_minimal(useHCFType) result(structure_HCF)
        implicit none
        integer(int32), intent(in) :: useHCFType
        class(Abstract_HCF), allocatable :: structure_HCF

        select case (useHCFType)
        case (11)
            structure_HCF = Type_HCF_Base_BC()
        case (12)
            structure_HCF = Type_HCF_Base_VG()
        case (13)
            structure_HCF = Type_HCF_Base_KO()
        case (14)
            structure_HCF = Type_HCF_Base_MVG()
        case (15)
            structure_HCF = Type_HCF_Base_Durner()
        case (16)
            structure_HCF = Type_HCF_Base_DVGCH()
        case (21)
            structure_HCF = Type_HCF_Impedance()
        case (31)
            structure_HCF = Type_HCF_Viscosity()
        case (41)
            structure_HCF = Type_HCF_Impedance_Viscosity()
        case (51)
            structure_HCF = Type_HCF_Base_Impedance_BC()
        case (52)
            structure_HCF = Type_HCF_Base_Impedance_VG()
        case (53)
            structure_HCF = Type_HCF_Base_Impedance_KO()
        case (54)
            structure_HCF = Type_HCF_Base_Impedance_MVG()
        case (55)
            structure_HCF = Type_HCF_Base_Impedance_Durner()
        case (56)
            structure_HCF = Type_HCF_Base_Impedance_DVGCH()
        case (61)
            structure_HCF = Type_HCF_Base_Viscosity_BC()
        case (62)
            structure_HCF = Type_HCF_Base_Viscosity_VG()
        case (63)
            structure_HCF = Type_HCF_Base_Viscosity_KO()
        case (64)
            structure_HCF = Type_HCF_Base_Viscosity_MVG()
        case (65)
            structure_HCF = Type_HCF_Base_Viscosity_Durner()
        case (66)
            structure_HCF = Type_HCF_Base_Viscosity_DVGCH()
        case (71)
            structure_HCF = Type_HCF_Base_Impedance_Viscosity_BC()
        case (72)
            structure_HCF = Type_HCF_Base_Impedance_Viscosity_VG()
        case (73)
            structure_HCF = Type_HCF_Base_Impedance_Viscosity_KO()
        case (74)
            structure_HCF = Type_HCF_Base_Impedance_Viscosity_MVG()
        case (75)
            structure_HCF = Type_HCF_Base_Impedance_Viscosity_Durner()
        case (76)
            structure_HCF = Type_HCF_Base_Impedance_Viscosity_DVGCH()
        end select

    end function Construct_Type_HCF_minimal

end module Calculate_HCF
