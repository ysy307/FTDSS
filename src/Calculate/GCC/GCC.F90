module Calculate_GCC
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Inout_Input, only:Type_Input
#ifdef _OPENMP
    use omp_lib
#endif
    implicit none
    private

    ! public :: Abst_GCC
    ! public :: Type_GCC_NonSegregation_m
    ! public :: Type_GCC_NonSegregation_Pa
    ! public :: Type_GCC_Segregation_m
    ! public :: Type_GCC_Segregation_Pa

    public :: GCCHolder

    type :: GCCHolder
        class(Abst_GCC), allocatable :: g
    contains
        procedure, pass(self) :: initialize => GCCHolder_initialize
    end type GCCHolder

    type, abstract :: Abst_GCC
        real(real64) :: Tf !! Freezing point
        real(real64) :: Lf !! Latent heat of fusion
        real(real64), private :: TtoK = 273.15d0
        real(real64), private :: g = 9.80665d0
    contains
        procedure(Abst_GCC_Calc), pass(self), deferred :: Calc
        procedure(Abst_GCC_Calc_Derivative), pass(self), deferred :: DERIV
        procedure(Abst_GCC_Calc_Derivative), pass(self), deferred :: DERIV2
    end type Abst_GCC

    type, extends(Abst_GCC) :: Type_GCC_NonSegregation_m
    contains
        procedure, pass(self) :: Calc => Calc_GCC_NonSeg_m
        procedure, pass(self) :: DERIV => Calc_GCC_NonSeg_m_Derivative
        procedure, pass(self) :: DERIV2 => Calc_GCC_NonSeg_m_Derivative_2nd
    end type Type_GCC_NonSegregation_m

    type, extends(Abst_GCC) :: Type_GCC_NonSegregation_Pa
    contains
        procedure, pass(self) :: Calc => Calc_GCC_NonSeg_Pa
        procedure, pass(self) :: DERIV => Calc_GCC_NonSeg_Pa_Derivative
        procedure, pass(self) :: DERIV2 => Calc_GCC_NonSeg_Pa_Derivative_2nd
    end type Type_GCC_NonSegregation_Pa

    type, extends(Abst_GCC) :: Type_GCC_Segregation_m
    contains
        procedure, pass(self) :: Calc => Calc_GCC_Seg_m
        procedure, pass(self) :: DERIV => Calc_GCC_Seg_m_Derivative
        procedure, pass(self) :: DERIV2 => Calc_GCC_Seg_m_Derivative_2nd
    end type Type_GCC_Segregation_m

    type, extends(Abst_GCC) :: Type_GCC_Segregation_Pa
    contains
        procedure, pass(self) :: Calc => Calc_GCC_Seg_Pa
        procedure, pass(self) :: DERIV => Calc_GCC_Seg_Pa_Derivative
        procedure, pass(self) :: DERIV2 => Calc_GCC_Seg_Pa_Derivative_2nd
    end type Type_GCC_Segregation_Pa

    abstract interface
        function Abst_GCC_Calc(self, T, Pw, rhoW, rhoI) result(Suction)
            import :: Abst_GCC, real64
            implicit none
            class(Abst_GCC), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction

        end function Abst_GCC_Calc

        function Abst_GCC_Calc_Derivative(self, T, Pw, rhoW, rhoI) result(Suction_Derivative)
            import :: Abst_GCC, real64
            implicit none
            class(Abst_GCC), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction_Derivative

        end function Abst_GCC_Calc_Derivative
    end interface

    interface
        module subroutine GCCHolder_initialize(self, iRegion, Input)
            implicit none
            class(GCCHolder), intent(inout) :: self
            integer(int32), intent(in) :: iRegion
            type(Type_Input), intent(in) :: Input

        end subroutine GCCHolder_initialize
    end interface

    interface
        module function Type_GCC_NonSeg_m_Construct(Tf, Lf) result(structure)
            implicit none
            real(real64), intent(in) :: Tf
            real(real64), intent(in) :: Lf
            class(Abst_GCC), allocatable :: structure

        end function Type_GCC_NonSeg_m_Construct

        module function Calc_GCC_NonSeg_m(self, T, Pw, rhoW, rhoI) result(Suction)
            implicit none
            class(Type_GCC_NonSegregation_m), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction

        end function Calc_GCC_NonSeg_m

        module function Calc_GCC_NonSeg_m_Derivative(self, T, Pw, rhoW, rhoI) result(Suction_Derivative)
            implicit none
            class(Type_GCC_NonSegregation_m), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction_Derivative

        end function Calc_GCC_NonSeg_m_Derivative

        module function Calc_GCC_NonSeg_m_Derivative_2nd(self, T, Pw, rhoW, rhoI) result(Suction_Derivative)
            implicit none
            class(Type_GCC_NonSegregation_m), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction_Derivative

        end function Calc_GCC_NonSeg_m_Derivative_2nd

    end interface

    interface
        module function Type_GCC_NonSeg_Pa_Construct(Tf, Lf) result(structure)
            implicit none
            real(real64), intent(in) :: Tf
            real(real64), intent(in) :: Lf
            class(Abst_GCC), allocatable :: structure

        end function Type_GCC_NonSeg_Pa_Construct

        module function Calc_GCC_NonSeg_Pa(self, T, Pw, rhoW, rhoI) result(Suction)
            implicit none
            class(Type_GCC_NonSegregation_Pa), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction

        end function Calc_GCC_NonSeg_Pa

        module function Calc_GCC_NonSeg_Pa_Derivative(self, T, Pw, rhoW, rhoI) result(Suction_Derivative)
            implicit none
            class(Type_GCC_NonSegregation_Pa), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction_Derivative

        end function Calc_GCC_NonSeg_Pa_Derivative

        module function Calc_GCC_NonSeg_Pa_Derivative_2nd(self, T, Pw, rhoW, rhoI) result(Suction_Derivative)
            implicit none
            class(Type_GCC_NonSegregation_Pa), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction_Derivative

        end function Calc_GCC_NonSeg_Pa_Derivative_2nd

    end interface

    interface
        module function Type_GCC_Seg_m_Construct(Tf, Lf) result(structure)
            implicit none
            real(real64), intent(in) :: Tf
            real(real64), intent(in) :: Lf
            class(Abst_GCC), allocatable :: structure

        end function Type_GCC_Seg_m_Construct

        module function Calc_GCC_Seg_m(self, T, Pw, rhoW, rhoI) result(Suction)
            implicit none
            class(Type_GCC_Segregation_m), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction

        end function Calc_GCC_Seg_m

        module function Calc_GCC_Seg_m_Derivative(self, T, Pw, rhoW, rhoI) result(Suction_Derivative)
            implicit none
            class(Type_GCC_Segregation_m), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction_Derivative

        end function Calc_GCC_Seg_m_Derivative

        module function Calc_GCC_Seg_m_Derivative_2nd(self, T, Pw, rhoW, rhoI) result(Suction_Derivative)
            implicit none
            class(Type_GCC_Segregation_m), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction_Derivative

        end function Calc_GCC_Seg_m_Derivative_2nd

    end interface

    interface
        module function Type_GCC_Seg_Pa_Construct(Tf, Lf) result(structure)
            implicit none
            real(real64), intent(in) :: Tf
            real(real64), intent(in) :: Lf
            class(Abst_GCC), allocatable :: structure

        end function Type_GCC_Seg_Pa_Construct

        module function Calc_GCC_Seg_Pa(self, T, Pw, rhoW, rhoI) result(Suction)
            implicit none
            class(Type_GCC_Segregation_Pa), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction

        end function Calc_GCC_Seg_Pa

        module function Calc_GCC_Seg_Pa_Derivative(self, T, Pw, rhoW, rhoI) result(Suction_Derivative)
            implicit none
            class(Type_GCC_Segregation_Pa), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction_Derivative

        end function Calc_GCC_Seg_Pa_Derivative

        module function Calc_GCC_Seg_Pa_Derivative_2nd(self, T, Pw, rhoW, rhoI) result(Suction_Derivative)
            implicit none
            class(Type_GCC_Segregation_Pa), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction_Derivative

        end function Calc_GCC_Seg_Pa_Derivative_2nd

    end interface

    interface Type_GCC_NonSegregation_m
        module procedure Type_GCC_NonSeg_m_Construct
    end interface

    interface Type_GCC_NonSegregation_Pa
        module procedure Type_GCC_NonSeg_Pa_Construct
    end interface

    interface Type_GCC_Segregation_m
        module procedure Type_GCC_Seg_m_Construct
    end interface

    interface Type_GCC_Segregation_Pa
        module procedure Type_GCC_Seg_Pa_Construct
    end interface

end module Calculate_GCC
