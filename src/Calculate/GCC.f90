module Calculate_GCC
    use, intrinsic :: iso_fortran_env, only: int32, real64
    implicit none
    private
    real(real64), parameter :: g = 9.80665d0
    real(real64), parameter :: TtoK = 273.15d0

    type, abstract :: Abstract_GCC
        real(real64) :: Tf !! Freezing point
        real(real64) :: Lf !! Latent heat of fusion
    end type Abstract_GCC

    type, extends(Abstract_GCC) :: GCC_NonSegregation
    end type GCC_NonSegregation

    type, extends(Abstract_GCC) :: GCC_Segregation
        real(real64) :: rhoW !! Density of water
        real(real64) :: rhoI !! Density of ice
    end type GCC_Segregation

    public :: Calculate_GCC_NonSegregation
    public :: Calculate_GCC_Segregation
    public :: Set_Calculate_GCC_Segregation

    interface Calculate_GCC_NonSegregation
        module procedure Calculate_GCC_NonSegregation_kPa
        module procedure Calculate_GCC_NonSegregation_m
    end interface Calculate_GCC_NonSegregation

    interface Calculate_GCC_NonSegregation_Derivative
        module procedure Calculate_GCC_NonSegregation_Derivative_kPa
        module procedure Calculate_GCC_NonSegregation_Derivative_m
    end interface Calculate_GCC_NonSegregation_Derivative

    abstract interface
        function Calculate_GCC_Segregation_interface(T, Pw, Tf, Lf, rhoW, rhoI) result(Suction)
            use, intrinsic :: iso_fortran_env, only: real64
            implicit none
            real(real64), intent(in) :: T, Pw, Tf, Lf, rhoW, rhoI
            real(real64) :: Suction
        end function Calculate_GCC_Segregation_interface
    end interface

    procedure(Calculate_GCC_Segregation_interface), pointer :: Calculate_GCC_Segregation => null()
    procedure(Calculate_GCC_Segregation_interface), pointer :: Calculate_GCC_Segregation_Derivative => null()

contains

    function Calculate_GCC_NonSegregation_kPa(T, Tf, Lf, rhoW) result(Suction_kPa)
        implicit none
        real(real64), intent(in) :: T, Tf, Lf, rhoW
        real(real64) :: Suction_kPa

        if (T <= Tf) then
            Suction_kPa = -Lf * rhoW * log((T + TtoK) / (Tf + TtoK))
        else
            Suction_kPa = 0.0d0
        end if

    end function Calculate_GCC_NonSegregation_kPa

    function Calculate_GCC_NonSegregation_m(T, Tf, Lf) result(Suction_m)
        implicit none
        real(real64), intent(in) :: T, Tf, Lf
        real(real64) :: Suction_m

        if (T <= Tf) then
            Suction_m = -Lf * log((T + TtoK) / (Tf + TtoK)) / g
        else
            Suction_m = 0.0d0
        end if

    end function Calculate_GCC_NonSegregation_m

    function Calculate_GCC_NonSegregation_Derivative_kPa(T, Tf, Lf, rhoW) result(Suction_kPa_Derivative)
        implicit none
        real(real64), intent(in) :: T, Tf, Lf, rhoW
        real(real64) :: Suction_kPa_Derivative

        if (T <= Tf) then
            Suction_kPa_Derivative = -Lf * rhoW / (T + TtoK)
        else
            Suction_kPa_Derivative = 0.0d0
        end if

    end function Calculate_GCC_NonSegregation_Derivative_kPa

    function Calculate_GCC_NonSegregation_Derivative_m(T, Tf, Lf) result(Suction_m)
        implicit none
        real(real64), intent(in) :: T, Tf, Lf
        real(real64) :: Suction_m

        if (T <= Tf) then
            Suction_m = -Lf / ((T + TtoK) * g)
        else
            Suction_m = 0.0d0
        end if

    end function Calculate_GCC_NonSegregation_Derivative_m

    subroutine Set_Calculate_GCC_Segregation(Segregation_type)
        implicit none
        integer(int32), intent(in) :: Segregation_type

        if (associated(Calculate_GCC_Segregation)) nullify (Calculate_GCC_Segregation)
        if (associated(Calculate_GCC_Segregation_Derivative)) nullify (Calculate_GCC_Segregation_Derivative)

        select case (Segregation_type)
        case (1)
            Calculate_GCC_Segregation => Calculate_GCC_Segregation_kPa
            Calculate_GCC_Segregation_Derivative => Calculate_GCC_Segregation_Derivative_kPa
        case (2)
            Calculate_GCC_Segregation => Calculate_GCC_Segregation_m
            Calculate_GCC_Segregation_Derivative => Calculate_GCC_Segregation_Derivative_m
        end select
    end subroutine Set_Calculate_GCC_Segregation

    function Calculate_GCC_Segregation_kPa(T, Pw_kPa, Tf, Lf, rhoW, rhoI) result(Suction_kPa)
        implicit none
        real(real64), intent(in) :: T, Pw_kPa, Tf, Lf, rhoW, rhoI
        real(real64) :: Suction_kPa

        if (T <= Tf) then
            Suction_kPa = (rhoI / rhoW - 1.0d0) * Pw_kPa - Lf * rhoI * log((T + TtoK) / (Tf + TtoK))
        else
            Suction_kPa = 0.0d0
        end if

    end function Calculate_GCC_Segregation_kPa

    function Calculate_GCC_Segregation_m(T, Pw_m, Tf, Lf, rhoW, rhoI) result(Suction_m)
        implicit none
        real(real64), intent(in) :: T, Pw_m, Tf, Lf, rhoW, rhoI
        real(real64) :: Suction_m

        if (T <= Tf) then
            Suction_m = ((rhoI / rhoW - 1.0d0) * Pw_m - Lf * rhoI * log((T + TtoK) / (Tf + TtoK))) / (rhoW * g)
        else
            Suction_m = 0.0d0
        end if

    end function Calculate_GCC_Segregation_m

    function Calculate_GCC_Segregation_Derivative_kPa(T, Pw_kPa, Tf, Lf, rhoW, rhoI) result(Suction_kPa_Derivative)
        implicit none
        real(real64), intent(in) :: T, Pw_kPa, Tf, Lf, rhoW, rhoI
        real(real64) :: Suction_kPa_Derivative

        if (T <= Tf) then
            Suction_kPa_Derivative = -Lf * rhoW / (T + TtoK)
        else
            Suction_kPa_Derivative = 0.0d0
        end if

    end function Calculate_GCC_Segregation_Derivative_kPa

    function Calculate_GCC_Segregation_Derivative_m(T, Pw_m, Tf, Lf, rhoW, rhoI) result(Suction_m_Derivative)
        implicit none
        real(real64), intent(in) :: T, Pw_m, Tf, Lf, rhoW, rhoI
        real(real64) :: Suction_m_Derivative

        if (T <= Tf) then
            Suction_m_Derivative = -Lf / ((T + TtoK) * g)
        else
            Suction_m_Derivative = 0.0d0
        end if

    end function Calculate_GCC_Segregation_Derivative_m

end module Calculate_GCC
