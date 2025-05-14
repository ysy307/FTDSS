module Calculate_WRF
    use, intrinsic :: iso_fortran_env
    use :: Inout_Input
#ifdef _OPENMP
    use omp_lib
#endif
    implicit none
    private
    real(real64), parameter :: pi = 4 * atan(1.0d0)

    public :: Abst_WRF
    public :: Type_WRF_BC
    public :: Type_WRF_VG
    public :: Type_WRF_KO
    public :: Type_WRF_MVG
    public :: Type_WRF_Durner
    public :: Type_WRF_DVGCH

    type, abstract :: Abst_WRF
        real(real64) :: thetaR
        real(real64) :: thetaS
    contains
        procedure(Abstract_Calculate_WRF), deferred :: Calc
        procedure(Abstract_Calculate_WRF_Derivative), deferred :: DERIV
    end type Abst_WRF

    type, extends(Abst_WRF) :: Type_WRF_BC
        real(real64) :: alpha1
        real(real64) :: n1
    contains
        procedure :: Calc => Calculate_WRF_BC
        procedure :: DERIV => Calculate_WRF_BC_Derivative
    end type Type_WRF_BC

    type, extends(Abst_WRF) :: Type_WRF_VG
        real(real64) :: alpha1
        real(real64) :: n1
        real(real64) :: m1
    contains
        procedure :: Calc => Calculate_WRF_VG
        procedure :: DERIV => Calculate_WRF_VG_Derivative
    end type Type_WRF_VG

    type, extends(Abst_WRF) :: Type_WRF_KO
        real(real64) :: alpha1
        real(real64) :: n1
    contains
        procedure :: Calc => Calculate_WRF_KO
        procedure :: DERIV => Calculate_WRF_KO_Derivative
    end type Type_WRF_KO

    type, extends(Abst_WRF) :: Type_WRF_MVG
        real(real64) :: alpha1
        real(real64) :: n1
        real(real64) :: m1
        real(real64) :: hcrit
    contains
        procedure :: Calc => Calculate_WRF_MVG
        procedure :: DERIV => Calculate_WRF_MVG_Derivative
    end type Type_WRF_MVG

    type, extends(Abst_WRF) :: Type_WRF_Durner
        real(real64) :: alpha1
        real(real64) :: n1
        real(real64) :: m1
        real(real64) :: alpha2
        real(real64) :: n2
        real(real64) :: m2
        real(real64) :: w1
        real(real64) :: w2
    contains
        procedure :: Calc => Calculate_WRF_Durner
        procedure :: DERIV => Calculate_WRF_Durner_Derivative
    end type Type_WRF_Durner

    type, extends(Abst_WRF) :: Type_WRF_DVGCH
        real(real64) :: alpha1
        real(real64) :: n1
        real(real64) :: n2
        real(real64) :: m1
        real(real64) :: m2
        real(real64) :: w1
        real(real64) :: w2
    contains
        procedure :: Calc => Calculate_WRF_DVGCH
        procedure :: DERIV => Calculate_WRF_DVGCH_Derivative
    end type Type_WRF_DVGCH

    abstract interface
        function Abstract_Calculate_WRF(self, h) result(thetaW)
            import :: Abst_WRF, real64
            implicit none
            class(Abst_WRF), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: thetaW
        end function Abstract_Calculate_WRF

        function Abstract_Calculate_WRF_Derivative(self, h) result(Cw)
            import :: Abst_WRF, real64
            implicit none
            class(Abst_WRF), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: Cw
        end function Abstract_Calculate_WRF_Derivative
    end interface

    interface
        module function Construct_Type_WRF_BC(Input) result(structure)
            implicit none
            type(Input_Region), intent(in) :: Input
            class(Abst_WRF), allocatable :: structure

        end function Construct_Type_WRF_BC

        module function Calculate_WRF_BC(self, h) result(thetaW)
            implicit none
            class(Type_WRF_BC), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: thetaW

        end function Calculate_WRF_BC

        module function Calculate_WRF_BC_Derivative(self, h) result(Cw)
            implicit none
            class(Type_WRF_BC), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: Cw

        end function Calculate_WRF_BC_Derivative
    end interface

    interface
        module function Type_WRF_VG_Construct(Input) result(structure)
            implicit none
            type(Input_Region), intent(in) :: Input
            class(Abst_WRF), allocatable :: structure

        end function Type_WRF_VG_Construct

        module function Calculate_WRF_VG(self, h) result(thetaW)
            implicit none
            class(Type_WRF_VG), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: thetaW

        end function Calculate_WRF_VG

        module function Calculate_WRF_VG_Derivative(self, h) result(Cw)
            implicit none
            class(Type_WRF_VG), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: Cw

        end function Calculate_WRF_VG_Derivative
    end interface

    interface
        module function Construct_Type_WRF_KO(Input) result(structure)
            implicit none
            type(Input_Region), intent(in) :: Input
            class(Abst_WRF), allocatable :: structure

        end function Construct_Type_WRF_KO

        module function Calculate_WRF_KO(self, h) result(thetaW)
            implicit none
            class(Type_WRF_KO), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: thetaW

        end function Calculate_WRF_KO

        module function Calculate_WRF_KO_Derivative(self, h) result(Cw)
            implicit none
            class(Type_WRF_KO), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: Cw

        end function Calculate_WRF_KO_Derivative

    end interface

    interface
        module function Construct_Type_WRF_MVG(Input) result(structure)
            implicit none
            type(Input_Region), intent(in) :: Input
            class(Abst_WRF), allocatable :: structure

        end function Construct_Type_WRF_MVG

        module function Calculate_WRF_MVG(self, h) result(thetaW)
            implicit none
            class(Type_WRF_MVG), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: thetaW

        end function Calculate_WRF_MVG

        module function Calculate_WRF_MVG_Derivative(self, h) result(Cw)
            implicit none
            class(Type_WRF_MVG), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: Cw

        end function Calculate_WRF_MVG_Derivative
    end interface

    interface
        module function Construct_Type_WRF_Durner(Input) result(structure)
            implicit none
            type(Input_Region), intent(in) :: Input
            class(Abst_WRF), allocatable :: structure

        end function Construct_Type_WRF_Durner

        module function Calculate_WRF_Durner(self, h) result(thetaW)
            implicit none
            class(Type_WRF_Durner), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: thetaW

        end function Calculate_WRF_Durner

        module function Calculate_WRF_Durner_Derivative(self, h) result(Cw)
            implicit none
            class(Type_WRF_Durner), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: Cw

        end function Calculate_WRF_Durner_Derivative
    end interface

    interface
        module function Construct_Type_WRF_DVGCH(Input) result(structure)
            implicit none
            type(Input_Region), intent(in) :: Input
            class(Abst_WRF), allocatable :: structure

        end function Construct_Type_WRF_DVGCH

        module function Calculate_WRF_DVGCH(self, h) result(thetaW)
            implicit none
            class(Type_WRF_DVGCH), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: thetaW

        end function Calculate_WRF_DVGCH

        module function Calculate_WRF_DVGCH_Derivative(self, h) result(Cw)
            implicit none
            class(Type_WRF_DVGCH), intent(in) :: self
            real(real64), intent(in) :: h
            real(real64) :: Cw

        end function Calculate_WRF_DVGCH_Derivative
    end interface

    interface Type_WRF_BC
        module procedure Construct_Type_WRF_BC
    end interface

    interface Type_WRF_VG
        module procedure :: Type_WRF_VG_Construct
    end interface

    interface Type_WRF_KO
        module procedure Construct_Type_WRF_KO
    end interface

    interface Type_WRF_MVG
        module procedure Construct_Type_WRF_MVG
    end interface

    interface Type_WRF_Durner
        module procedure Construct_Type_WRF_Durner
    end interface

    interface Type_WRF_DVGCH
        module procedure Construct_Type_WRF_DVGCH
    end interface

end module Calculate_WRF
