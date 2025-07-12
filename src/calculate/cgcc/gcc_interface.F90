module calculate_gcc
    use, intrinsic :: iso_fortran_env, only: int32, real64
!$  use omp_lib
    use :: Inout_Input, only:type_Input
    implicit none
    private

    public :: holder_gcc
    public :: abst_gcc
    public :: type_gcc_non_segregation_m
    public :: type_gcc_non_segregation_pa
    public :: type_gcc_segregation_m
    public :: type_gcc_segregation_pa

    type :: holder_gcc
        class(abst_gcc), allocatable :: g
    contains
        procedure, pass(self) :: initialize => initialize_holder_gcc
    end type holder_gcc

    type, abstract :: abst_gcc
        real(real64) :: Tf !! Freezing point
        real(real64) :: Lf !! Latent heat of fusion
        real(real64), private :: TtoK = 273.15d0
        real(real64), private :: g = 9.80665d0
    contains
        procedure(abst_gcc_Calc), pass(self), deferred :: Calc
        procedure(abst_gcc_Calc_Derivative), pass(self), deferred :: DERIV
        procedure(abst_gcc_Calc_Derivative), pass(self), deferred :: DERIV2
    end type abst_gcc

    type, extends(abst_gcc) :: type_gcc_non_segregation_m
    contains
        procedure, pass(self) :: Calc => Calc_GCC_NonSeg_m
        procedure, pass(self) :: DERIV => Calc_GCC_NonSeg_m_Derivative
        procedure, pass(self) :: DERIV2 => Calc_GCC_NonSeg_m_Derivative_2nd
    end type type_gcc_non_segregation_m

    type, extends(abst_gcc) :: type_gcc_non_segregation_pa
    contains
        procedure, pass(self) :: Calc => Calc_GCC_NonSeg_Pa
        procedure, pass(self) :: DERIV => Calc_GCC_NonSeg_Pa_Derivative
        procedure, pass(self) :: DERIV2 => Calc_GCC_NonSeg_Pa_Derivative_2nd
    end type type_gcc_non_segregation_pa

    type, extends(abst_gcc) :: type_gcc_segregation_m
    contains
        procedure, pass(self) :: Calc => Calc_GCC_Seg_m
        procedure, pass(self) :: DERIV => Calc_GCC_Seg_m_Derivative
        procedure, pass(self) :: DERIV2 => Calc_GCC_Seg_m_Derivative_2nd
    end type type_gcc_segregation_m

    type, extends(abst_gcc) :: type_gcc_segregation_pa
    contains
        procedure, pass(self) :: Calc => Calc_GCC_Seg_Pa
        procedure, pass(self) :: DERIV => Calc_GCC_Seg_Pa_Derivative
        procedure, pass(self) :: DERIV2 => Calc_GCC_Seg_Pa_Derivative_2nd
    end type type_gcc_segregation_pa

    abstract interface
        function abst_gcc_Calc(self, T, Pw, rhoW, rhoI) result(Suction)
            import :: abst_gcc, real64
            implicit none
            class(abst_gcc), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction

        end function abst_gcc_Calc

        function abst_gcc_Calc_Derivative(self, T, Pw, rhoW, rhoI) result(Suction_Derivative)
            import :: abst_gcc, real64
            implicit none
            class(abst_gcc), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction_Derivative

        end function abst_gcc_Calc_Derivative
    end interface

    interface
        module subroutine initialize_holder_gcc(self, iRegion, Input)
            implicit none
            class(holder_gcc), intent(inout) :: self
            integer(int32), intent(in) :: iRegion
            type(type_Input), intent(in) :: Input

        end subroutine initialize_holder_gcc
    end interface

    interface
        module function type_GCC_NonSeg_m_Construct(Tf, Lf) result(structure)
            implicit none
            real(real64), intent(in) :: Tf
            real(real64), intent(in) :: Lf
            class(abst_gcc), allocatable :: structure

        end function type_GCC_NonSeg_m_Construct

        module function Calc_GCC_NonSeg_m(self, T, Pw, rhoW, rhoI) result(Suction)
            implicit none
            class(type_gcc_non_segregation_m), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction

        end function Calc_GCC_NonSeg_m

        module function Calc_GCC_NonSeg_m_Derivative(self, T, Pw, rhoW, rhoI) result(Suction_Derivative)
            implicit none
            class(type_gcc_non_segregation_m), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction_Derivative

        end function Calc_GCC_NonSeg_m_Derivative

        module function Calc_GCC_NonSeg_m_Derivative_2nd(self, T, Pw, rhoW, rhoI) result(Suction_Derivative)
            implicit none
            class(type_gcc_non_segregation_m), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction_Derivative

        end function Calc_GCC_NonSeg_m_Derivative_2nd

    end interface

    interface
        module function type_GCC_NonSeg_Pa_Construct(Tf, Lf) result(structure)
            implicit none
            real(real64), intent(in) :: Tf
            real(real64), intent(in) :: Lf
            class(abst_gcc), allocatable :: structure

        end function type_GCC_NonSeg_Pa_Construct

        module function Calc_GCC_NonSeg_Pa(self, T, Pw, rhoW, rhoI) result(Suction)
            implicit none
            class(type_gcc_non_segregation_pa), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction

        end function Calc_GCC_NonSeg_Pa

        module function Calc_GCC_NonSeg_Pa_Derivative(self, T, Pw, rhoW, rhoI) result(Suction_Derivative)
            implicit none
            class(type_gcc_non_segregation_pa), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction_Derivative

        end function Calc_GCC_NonSeg_Pa_Derivative

        module function Calc_GCC_NonSeg_Pa_Derivative_2nd(self, T, Pw, rhoW, rhoI) result(Suction_Derivative)
            implicit none
            class(type_gcc_non_segregation_pa), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction_Derivative

        end function Calc_GCC_NonSeg_Pa_Derivative_2nd

    end interface

    interface
        module function type_GCC_Seg_m_Construct(Tf, Lf) result(structure)
            implicit none
            real(real64), intent(in) :: Tf
            real(real64), intent(in) :: Lf
            class(abst_gcc), allocatable :: structure

        end function type_GCC_Seg_m_Construct

        module function Calc_GCC_Seg_m(self, T, Pw, rhoW, rhoI) result(Suction)
            implicit none
            class(type_gcc_segregation_m), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction

        end function Calc_GCC_Seg_m

        module function Calc_GCC_Seg_m_Derivative(self, T, Pw, rhoW, rhoI) result(Suction_Derivative)
            implicit none
            class(type_gcc_segregation_m), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction_Derivative

        end function Calc_GCC_Seg_m_Derivative

        module function Calc_GCC_Seg_m_Derivative_2nd(self, T, Pw, rhoW, rhoI) result(Suction_Derivative)
            implicit none
            class(type_gcc_segregation_m), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction_Derivative

        end function Calc_GCC_Seg_m_Derivative_2nd

    end interface

    interface
        module function type_GCC_Seg_Pa_Construct(Tf, Lf) result(structure)
            implicit none
            real(real64), intent(in) :: Tf
            real(real64), intent(in) :: Lf
            class(abst_gcc), allocatable :: structure

        end function type_GCC_Seg_Pa_Construct

        module function Calc_GCC_Seg_Pa(self, T, Pw, rhoW, rhoI) result(Suction)
            implicit none
            class(type_gcc_segregation_pa), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction

        end function Calc_GCC_Seg_Pa

        module function Calc_GCC_Seg_Pa_Derivative(self, T, Pw, rhoW, rhoI) result(Suction_Derivative)
            implicit none
            class(type_gcc_segregation_pa), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction_Derivative

        end function Calc_GCC_Seg_Pa_Derivative

        module function Calc_GCC_Seg_Pa_Derivative_2nd(self, T, Pw, rhoW, rhoI) result(Suction_Derivative)
            implicit none
            class(type_gcc_segregation_pa), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction_Derivative

        end function Calc_GCC_Seg_Pa_Derivative_2nd

    end interface

    interface type_gcc_non_segregation_m
        module procedure type_GCC_NonSeg_m_Construct
    end interface

    interface type_gcc_non_segregation_pa
        module procedure type_GCC_NonSeg_Pa_Construct
    end interface

    interface type_gcc_segregation_m
        module procedure type_GCC_Seg_m_Construct
    end interface

    interface type_gcc_segregation_pa
        module procedure type_GCC_Seg_Pa_Construct
    end interface

end module calculate_gcc
