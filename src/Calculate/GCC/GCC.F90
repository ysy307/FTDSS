module Calculate_GCC
    use, intrinsic :: iso_fortran_env, only: int32, real64
#ifdef _OPENMP
    use omp_lib
#endif
    implicit none
    private

    public :: Abstract_GCC
    public :: Type_GCC_NonSegregation_m
    public :: Type_GCC_NonSegregation_Pa
    public :: Type_GCC_Segregation_m
    public :: Type_GCC_Segregation_Pa

    type, abstract :: Abstract_GCC
        real(real64) :: Tf !! Freezing point
        real(real64) :: Lf !! Latent heat of fusion
        real(real64), private :: TtoK = 273.15d0
        real(real64), private :: g = 9.80665d0
    contains
        procedure(Abstract_Calculate_GCC), pass(self), deferred :: Calculate_GCC
        procedure(Abstract_Calculate_GCC_Derivative), pass(self), deferred :: Calculate_GCC_Derivative
    end type Abstract_GCC

    type, extends(Abstract_GCC) :: Type_GCC_NonSegregation_m
    contains
        procedure, pass(self) :: Calculate_GCC => Calculate_GCC_NonSegregation_m
        procedure, pass(self) :: Calculate_GCC_Derivative => Calculate_GCC_NonSegregation_m_Derivative
    end type Type_GCC_NonSegregation_m

    type, extends(Abstract_GCC) :: Type_GCC_NonSegregation_Pa
    contains
        procedure, pass(self) :: Calculate_GCC => Calculate_GCC_NonSegregation_Pa
        procedure, pass(self) :: Calculate_GCC_Derivative => Calculate_GCC_NonSegregation_Pa_Derivative
    end type Type_GCC_NonSegregation_Pa

    type, extends(Abstract_GCC) :: Type_GCC_Segregation_m
    contains
        procedure, pass(self) :: Calculate_GCC => Calculate_GCC_Segregation_m
        procedure, pass(self) :: Calculate_GCC_Derivative => Calculate_GCC_Segregation_m_Derivative
    end type Type_GCC_Segregation_m

    type, extends(Abstract_GCC) :: Type_GCC_Segregation_Pa
    contains
        procedure, pass(self) :: Calculate_GCC => Calculate_GCC_Segregation_Pa
        procedure, pass(self) :: Calculate_GCC_Derivative => Calculate_GCC_Segregation_Pa_Derivative
    end type Type_GCC_Segregation_Pa

    abstract interface
        function Abstract_Calculate_GCC(self, T, Pw, rhoW, rhoI) result(Suction)
            import :: Abstract_GCC, real64
            implicit none
            class(Abstract_GCC), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction

        end function Abstract_Calculate_GCC

        function Abstract_Calculate_GCC_Derivative(self, T, Pw, rhoW, rhoI) result(Suction_Derivative)
            import :: Abstract_GCC, real64
            implicit none
            class(Abstract_GCC), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction_Derivative

        end function Abstract_Calculate_GCC_Derivative
    end interface

    interface
        module function Type_GCC_NonSegregation_m_Construct(Tf, Lf) result(structure)
            implicit none
            real(real64), intent(in) :: Tf
            real(real64), intent(in) :: Lf
            class(Abstract_GCC), allocatable :: structure

        end function Type_GCC_NonSegregation_m_Construct

        module function Calculate_GCC_NonSegregation_m(self, T, Pw, rhoW, rhoI) result(Suction)
            implicit none
            class(Type_GCC_NonSegregation_m), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction

        end function Calculate_GCC_NonSegregation_m

        module function Calculate_GCC_NonSegregation_m_Derivative(self, T, Pw, rhoW, rhoI) result(Suction_Derivative)
            implicit none
            class(Type_GCC_NonSegregation_m), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction_Derivative

        end function Calculate_GCC_NonSegregation_m_Derivative

    end interface

    interface
        module function Type_GCC_NonSegregation_Pa_Construct(Tf, Lf) result(structure)
            implicit none
            real(real64), intent(in) :: Tf
            real(real64), intent(in) :: Lf
            class(Abstract_GCC), allocatable :: structure

        end function Type_GCC_NonSegregation_Pa_Construct

        module function Calculate_GCC_NonSegregation_Pa(self, T, Pw, rhoW, rhoI) result(Suction)
            implicit none
            class(Type_GCC_NonSegregation_Pa), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction

        end function Calculate_GCC_NonSegregation_Pa

        module function Calculate_GCC_NonSegregation_Pa_Derivative(self, T, Pw, rhoW, rhoI) result(Suction_Derivative)
            implicit none
            class(Type_GCC_NonSegregation_Pa), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction_Derivative

        end function Calculate_GCC_NonSegregation_Pa_Derivative

    end interface

    interface
        module function Type_GCC_Segregation_m_Construct(Tf, Lf) result(structure)
            implicit none
            real(real64), intent(in) :: Tf
            real(real64), intent(in) :: Lf
            class(Abstract_GCC), allocatable :: structure

        end function Type_GCC_Segregation_m_Construct

        module function Calculate_GCC_Segregation_m(self, T, Pw, rhoW, rhoI) result(Suction)
            implicit none
            class(Type_GCC_Segregation_m), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction

        end function Calculate_GCC_Segregation_m

        module function Calculate_GCC_Segregation_m_Derivative(self, T, Pw, rhoW, rhoI) result(Suction_Derivative)
            implicit none
            class(Type_GCC_Segregation_m), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction_Derivative

        end function Calculate_GCC_Segregation_m_Derivative

    end interface

    interface
        module function Type_GCC_Segregation_Pa_Construct(Tf, Lf) result(structure)
            implicit none
            real(real64), intent(in) :: Tf
            real(real64), intent(in) :: Lf
            class(Abstract_GCC), allocatable :: structure

        end function Type_GCC_Segregation_Pa_Construct

        module function Calculate_GCC_Segregation_Pa(self, T, Pw, rhoW, rhoI) result(Suction)
            implicit none
            class(Type_GCC_Segregation_Pa), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction

        end function Calculate_GCC_Segregation_Pa

        module function Calculate_GCC_Segregation_Pa_Derivative(self, T, Pw, rhoW, rhoI) result(Suction_Derivative)
            implicit none
            class(Type_GCC_Segregation_Pa), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: rhoW
            real(real64), intent(in), optional :: rhoI
            real(real64) :: Suction_Derivative

        end function Calculate_GCC_Segregation_Pa_Derivative

    end interface

    interface Type_GCC_NonSegregation_m
        module procedure Type_GCC_NonSegregation_m_Construct
    end interface

    interface Type_GCC_NonSegregation_Pa
        module procedure Type_GCC_NonSegregation_Pa_Construct
    end interface

    interface Type_GCC_Segregation_m
        module procedure Type_GCC_Segregation_m_Construct
    end interface

    interface Type_GCC_Segregation_Pa
        module procedure Type_GCC_Segregation_Pa_Construct
    end interface

end module Calculate_GCC
