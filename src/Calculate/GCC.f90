module Calculate_GCC
    use, intrinsic :: iso_fortran_env, only: int32, real64
    implicit none

    type, abstract :: Abstract_GCC
        real(real64) :: Tf !! Freezing point
        real(real64) :: Lf !! Latent heat of fusion
        real(real64), private :: TtoK = 273.15d0
        real(real64), private :: g = 9.80665d0
    end type Abstract_GCC

    type, extends(Abstract_GCC) :: Type_GCC_NonSegregation_m
    contains
        procedure, pass(self) :: Calculate_GCC => Calculate_GCC_NonSegregation_m
        procedure, pass(self) :: Calculate_GCC_Derivative => Calculate_GCC_NonSegregation_Derivative_m
    end type Type_GCC_NonSegregation_m

    type, extends(Abstract_GCC) :: Type_GCC_NonSegregation_kPa
    contains
        procedure, pass(self) :: Calculate_GCC => Calculate_GCC_NonSegregation_kPa
        procedure, pass(self) :: Calculate_GCC_Derivative => Calculate_GCC_NonSegregation_Derivative_kPa
    end type Type_GCC_NonSegregation_kPa

    type, abstract, extends(Abstract_GCC) :: Abstract_GCC_Segregation
        real(real64) :: rhoI !! Density of ice
    contains
        procedure(Abstract_Calculate_GCC_Segregation), pass(self), deferred :: Calculate_GCC
        procedure(Abstract_Calculate_GCC_Segregation_Derivative), pass(self), deferred :: Calculate_GCC_Derivative
    end type Abstract_GCC_Segregation

    type, extends(Abstract_GCC_Segregation) :: Type_GCC_Segregation_m
    contains
        procedure, pass(self) :: Calculate_GCC => Calculate_GCC_Segregation_m
        procedure, pass(self) :: Calculate_GCC_Derivative => Calculate_GCC_Segregation_Derivative_m
    end type Type_GCC_Segregation_m

    type, extends(Abstract_GCC_Segregation) :: Type_GCC_Segregation_kPa
    contains
        procedure, pass(self) :: Calculate_GCC => Calculate_GCC_Segregation_kPa
        procedure, pass(self) :: Calculate_GCC_Derivative => Calculate_GCC_Segregation_Derivative_kPa
    end type Type_GCC_Segregation_kPa

    interface
        function Abstract_Calculate_GCC_Segregation(self, T, Pw, rhoW) result(Suction)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Abstract_GCC_Segregation
            implicit none
            class(Abstract_GCC_Segregation), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in) :: Pw
            real(real64), intent(in) :: rhoW
            real(real64) :: Suction
        end function Abstract_Calculate_GCC_Segregation

        function Abstract_Calculate_GCC_Segregation_Derivative(self, T, Pw, rhoW) result(Suction_Derivative)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Abstract_GCC_Segregation
            implicit none
            class(Abstract_GCC_Segregation), intent(in) :: self
            real(real64), intent(in) :: T
            real(real64), intent(in) :: Pw
            real(real64), intent(in) :: rhoW
            real(real64) :: Suction_Derivative
        end function Abstract_Calculate_GCC_Segregation_Derivative
    end interface

contains

    function Calculate_GCC_NonSegregation_m(self, T) result(Suction)
        implicit none
        class(Type_GCC_NonSegregation_m), intent(in) :: self
        real(real64), intent(in) :: T
        real(real64) :: Suction

        if (T <= self%Tf) then
            Suction = -self%Lf * log((T + self%TtoK) / (self%Tf + self%TtoK)) / self%g
        else
            Suction = 0.0d0
        end if

    end function Calculate_GCC_NonSegregation_m

    function Calculate_GCC_NonSegregation_Derivative_m(self, T) result(Suction)
        implicit none
        class(Type_GCC_NonSegregation_m), intent(in) :: self
        real(real64), intent(in) :: T
        real(real64) :: Suction

        if (T <= self%Tf) then
            Suction = -self%Lf / ((T + self%TtoK) * self%g)
        else
            Suction = 0.0d0
        end if

    end function Calculate_GCC_NonSegregation_Derivative_m

    function Calculate_GCC_NonSegregation_kPa(self, T, rhoW) result(Suction)
        implicit none
        class(Type_GCC_NonSegregation_kPa), intent(in) :: self
        real(real64), intent(in) :: T, rhoW
        real(real64) :: Suction

        if (T <= self%Tf) then
            Suction = -self%Lf * rhoW * log((T + self%TtoK) / (self%Tf + self%TtoK))
        else
            Suction = 0.0d0
        end if

    end function Calculate_GCC_NonSegregation_kPa

    function Calculate_GCC_NonSegregation_Derivative_kPa(self, T, rhoW) result(Suction_Derivative)
        implicit none
        class(Type_GCC_NonSegregation_kPa), intent(in) :: self
        real(real64), intent(in) :: T, rhoW
        real(real64) :: Suction_Derivative

        if (T <= self%Tf) then
            Suction_Derivative = -self%Lf * rhoW / (T + self%TtoK)
        else
            Suction_Derivative = 0.0d0
        end if

    end function Calculate_GCC_NonSegregation_Derivative_kPa

    function Calculate_GCC_Segregation_m(self, T, Pw, rhoW) result(Suction)
        implicit none
        class(Type_GCC_Segregation_m), intent(in) :: self
        real(real64), intent(in) :: T
        real(real64), intent(in) :: Pw
        real(real64), intent(in) :: rhoW
        real(real64) :: Suction

        if (T <= self%Tf) then
            Suction = ((self%rhoI / rhoW - 1.0d0) * Pw - self%Lf * self%rhoI * log((T + self%TtoK) / (self%Tf + self%TtoK))) / (rhoW * self%g)
        else
            Suction = 0.0d0
        end if

    end function Calculate_GCC_Segregation_m

    function Calculate_GCC_Segregation_Derivative_m(self, T, Pw, rhoW) result(Suction_Derivative)
        implicit none
        class(Type_GCC_Segregation_m), intent(in) :: self
        real(real64), intent(in) :: T
        real(real64), intent(in) :: Pw
        real(real64), intent(in) :: rhoW
        real(real64) :: Suction_Derivative

        if (T <= self%Tf) then
            Suction_Derivative = -self%Lf * self%rhoI / ((T + self%TtoK) * rhoW * self%g)
        else
            Suction_Derivative = 0.0d0
        end if

    end function Calculate_GCC_Segregation_Derivative_m

    function Calculate_GCC_Segregation_kPa(self, T, Pw, rhoW) result(Suction)
        implicit none
        class(Type_GCC_Segregation_kPa), intent(in) :: self
        real(real64), intent(in) :: T
        real(real64), intent(in) :: Pw
        real(real64), intent(in) :: rhoW
        real(real64) :: Suction

        if (T <= self%Tf) then
            Suction = (self%rhoI / rhoW - 1.0d0) * Pw - self%Lf * self%rhoI * log((T + self%TtoK) / (self%Tf + self%TtoK))
        else
            Suction = 0.0d0
        end if

    end function Calculate_GCC_Segregation_kPa

    function Calculate_GCC_Segregation_Derivative_kPa(self, T, Pw, rhoW) result(Suction_Derivative)
        implicit none
        class(Type_GCC_Segregation_kPa), intent(in) :: self
        real(real64), intent(in) :: T
        real(real64), intent(in) :: Pw
        real(real64), intent(in) :: rhoW
        real(real64) :: Suction_Derivative

        if (T <= self%Tf) then
            Suction_Derivative = -self%Lf * self%rhoI / (T + self%TtoK)
        else
            Suction_Derivative = 0.0d0
        end if

    end function Calculate_GCC_Segregation_Derivative_kPa

end module Calculate_GCC
