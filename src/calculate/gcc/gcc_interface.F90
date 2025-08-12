module calculate_gcc
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_input, only:type_input
    implicit none
    private

    public :: holder_gccs
    public :: abst_gcc
    public :: type_gcc_non_segregation_m
    public :: type_gcc_non_segregation_pa
    public :: type_gcc_segregation_m
    public :: type_gcc_segregation_pa

    type :: holder_gccs
        class(abst_gcc), allocatable :: p
    contains
        procedure, pass(self) :: initialize => initialize_holder_gccs
    end type holder_gccs

    type, abstract :: abst_gcc
        real(real64) :: Tf !! Freezing point
        real(real64) :: Lf !! Latent heat of fusion
        real(real64), private :: TtoK = 273.15d0 !! Conversion from Celsius to Kelvin
        real(real64), private :: g = 9.80665d0 !! Gravitational acceleration
    contains
        procedure(abst_gcc_calc), pass(self), deferred :: calc
        procedure(abst_gcc_calc_derivative), pass(self), deferred :: deriv
        procedure(abst_gcc_calc_derivative), pass(self), deferred :: deriv2
    end type abst_gcc

    type, extends(abst_gcc) :: type_gcc_non_segregation_m
    contains
        procedure, pass(self) :: calc => calc_GCC_NonSeg_m
        procedure, pass(self) :: deriv => calc_GCC_NonSeg_m_derivative
        procedure, pass(self) :: deriv2 => calc_GCC_NonSeg_m_derivative_2nd
    end type type_gcc_non_segregation_m

    type, extends(abst_gcc) :: type_gcc_non_segregation_pa
    contains
        procedure, pass(self) :: calc => calc_GCC_NonSeg_Pa
        procedure, pass(self) :: deriv => calc_GCC_NonSeg_Pa_derivative
        procedure, pass(self) :: deriv2 => calc_GCC_NonSeg_Pa_derivative_2nd
    end type type_gcc_non_segregation_pa

    type, extends(abst_gcc) :: type_gcc_segregation_m
    contains
        procedure, pass(self) :: calc => calc_GCC_Seg_m
        procedure, pass(self) :: deriv => calc_GCC_Seg_m_derivative
        procedure, pass(self) :: deriv2 => calc_GCC_Seg_m_derivative_2nd
    end type type_gcc_segregation_m

    type, extends(abst_gcc) :: type_gcc_segregation_pa
    contains
        procedure, pass(self) :: calc => calc_GCC_Seg_Pa
        procedure, pass(self) :: deriv => calc_GCC_Seg_Pa_derivative
        procedure, pass(self) :: deriv2 => calc_GCC_Seg_Pa_derivative_2nd
    end type type_gcc_segregation_pa

    abstract interface
        function abst_gcc_calc(self, T, Pw, rhoW, rhoI) result(suction)
            import :: abst_gcc, real64
            implicit none
            class(abst_gcc), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: suction

        end function abst_gcc_calc

        function abst_gcc_calc_derivative(self, T, Pw, rhoW, rhoI) result(suction_derivative)
            import :: abst_gcc, real64
            implicit none
            class(abst_gcc), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: suction_derivative

        end function abst_gcc_calc_derivative
    end interface

    interface
        module subroutine initialize_holder_gccs(self, input, i_material)
            implicit none
            class(holder_gccs), intent(inout) :: self
            type(type_input), intent(in) :: input
            integer(int32), intent(in) :: i_material

        end subroutine initialize_holder_gccs
    end interface

    interface
        module function type_GCC_NonSeg_m_Construct(Tf, Lf) result(property)
            implicit none
            real(real64), intent(in) :: Tf
            real(real64), intent(in) :: Lf
            class(abst_gcc), allocatable :: property

        end function type_GCC_NonSeg_m_Construct

        module function calc_GCC_NonSeg_m(self, T, Pw, rhoW, rhoI) result(suction)
            implicit none
            class(type_gcc_non_segregation_m), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: suction

        end function calc_GCC_NonSeg_m

        module function calc_GCC_NonSeg_m_derivative(self, T, Pw, rhoW, rhoI) result(suction_derivative)
            implicit none
            class(type_gcc_non_segregation_m), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: suction_derivative

        end function calc_GCC_NonSeg_m_derivative

        module function calc_GCC_NonSeg_m_derivative_2nd(self, T, Pw, rhoW, rhoI) result(suction_derivative)
            implicit none
            class(type_gcc_non_segregation_m), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: suction_derivative

        end function calc_GCC_NonSeg_m_derivative_2nd

    end interface

    interface
        module function type_GCC_NonSeg_Pa_Construct(Tf, Lf) result(property)
            implicit none
            real(real64), intent(in) :: Tf
            real(real64), intent(in) :: Lf
            class(abst_gcc), allocatable :: property

        end function type_GCC_NonSeg_Pa_Construct

        module function calc_GCC_NonSeg_Pa(self, T, Pw, rhoW, rhoI) result(suction)
            implicit none
            class(type_gcc_non_segregation_pa), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: suction

        end function calc_GCC_NonSeg_Pa

        module function calc_GCC_NonSeg_Pa_derivative(self, T, Pw, rhoW, rhoI) result(suction_derivative)
            implicit none
            class(type_gcc_non_segregation_pa), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: suction_derivative

        end function calc_GCC_NonSeg_Pa_derivative

        module function calc_GCC_NonSeg_Pa_derivative_2nd(self, T, Pw, rhoW, rhoI) result(suction_derivative)
            implicit none
            class(type_gcc_non_segregation_pa), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: suction_derivative

        end function calc_GCC_NonSeg_Pa_derivative_2nd

    end interface

    interface
        module function type_GCC_Seg_m_Construct(Tf, Lf) result(property)
            implicit none
            real(real64), intent(in) :: Tf
            real(real64), intent(in) :: Lf
            class(abst_gcc), allocatable :: property

        end function type_GCC_Seg_m_Construct

        module function calc_GCC_Seg_m(self, T, Pw, rhoW, rhoI) result(suction)
            implicit none
            class(type_gcc_segregation_m), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: suction

        end function calc_GCC_Seg_m

        module function calc_GCC_Seg_m_derivative(self, T, Pw, rhoW, rhoI) result(suction_derivative)
            implicit none
            class(type_gcc_segregation_m), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: suction_derivative

        end function calc_GCC_Seg_m_derivative

        module function calc_GCC_Seg_m_derivative_2nd(self, T, Pw, rhoW, rhoI) result(suction_derivative)
            implicit none
            class(type_gcc_segregation_m), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: suction_derivative

        end function calc_GCC_Seg_m_derivative_2nd

    end interface

    interface
        module function type_GCC_Seg_Pa_Construct(Tf, Lf) result(property)
            implicit none
            real(real64), intent(in) :: Tf
            real(real64), intent(in) :: Lf
            class(abst_gcc), allocatable :: property

        end function type_GCC_Seg_Pa_Construct

        module function calc_GCC_Seg_Pa(self, T, Pw, rhoW, rhoI) result(suction)
            implicit none
            class(type_gcc_segregation_pa), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: suction

        end function calc_GCC_Seg_Pa

        module function calc_GCC_Seg_Pa_derivative(self, T, Pw, rhoW, rhoI) result(suction_derivative)
            implicit none
            class(type_gcc_segregation_pa), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: suction_derivative

        end function calc_GCC_Seg_Pa_derivative

        module function calc_GCC_Seg_Pa_derivative_2nd(self, T, Pw, rhoW, rhoI) result(suction_derivative)
            implicit none
            class(type_gcc_segregation_pa), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: suction_derivative

        end function calc_GCC_Seg_Pa_derivative_2nd

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
