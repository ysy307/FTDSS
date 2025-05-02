submodule(Calculate_GCC) GCC_NonSegregation_m
    implicit none
contains

    module function Type_GCC_NonSegregation_m_Construct(Tf, Lf) result(structure)
        implicit none
        real(real64), intent(in) :: Tf
        real(real64), intent(in) :: Lf
        class(Abstract_GCC), allocatable :: structure

        if (allocated(structure)) deallocate (structure)
        allocate (Type_GCC_NonSegregation_m :: structure)

        select type (this => structure)
        type is (Type_GCC_NonSegregation_m)
            this%Lf = Lf
            this%Tf = Tf
        end select

    end function Type_GCC_NonSegregation_m_Construct

    module function Calculate_GCC_NonSegregation_m(self, T, Pw, rhoW, rhoI) result(Suction)
        implicit none
        class(Type_GCC_NonSegregation_m), intent(in) :: self
        real(real64), intent(in) :: T
        real(real64), intent(in), optional :: Pw
        real(real64), intent(in), optional :: rhoW
        real(real64), intent(in), optional :: rhoI
        real(real64) :: Suction

        if (T <= self%Tf) then
            Suction = -self%Lf * log((T + self%TtoK) / (self%Tf + self%TtoK)) / self%g
        else
            Suction = 0.0d0
        end if

    end function Calculate_GCC_NonSegregation_m

    module function Calculate_GCC_NonSegregation_m_Derivative(self, T, Pw, rhoW, rhoI) result(Suction_Derivative)
        implicit none
        class(Type_GCC_NonSegregation_m), intent(in) :: self
        real(real64), intent(in) :: T
        real(real64), intent(in), optional :: Pw
        real(real64), intent(in), optional :: rhoW
        real(real64), intent(in), optional :: rhoI
        real(real64) :: Suction_Derivative

        if (T <= self%Tf) then
            Suction_Derivative = -self%Lf / ((T + self%TtoK) * self%g)
        else
            Suction_Derivative = 0.0d0
        end if
    end function Calculate_GCC_NonSegregation_m_Derivative

end submodule GCC_NonSegregation_m
