submodule(calculate_gcc) gcc_non_segregation_m
    implicit none
contains

    module function type_GCC_NonSeg_m_Construct(Tf, Lf) result(property)
        implicit none
        real(real64), intent(in) :: Tf
        real(real64), intent(in) :: Lf
        class(abst_gcc), allocatable :: property

        if (allocated(property)) deallocate (property)
        allocate (type_gcc_non_segregation_m :: property)

        select type (this => property)
        type is (type_gcc_non_segregation_m)
            this%Lf = Lf
            this%Tf = Tf
        end select

    end function type_GCC_NonSeg_m_Construct

    module function Calc_GCC_NonSeg_m(self, T, Pw, rhoW, rhoI) result(Suction)
        implicit none
        class(type_gcc_non_segregation_m), intent(in) :: self
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

    end function Calc_GCC_NonSeg_m

    module function Calc_GCC_NonSeg_m_Derivative(self, T, Pw, rhoW, rhoI) result(Suction_Derivative)
        implicit none
        class(type_gcc_non_segregation_m), intent(in) :: self
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
    end function Calc_GCC_NonSeg_m_Derivative

    module function Calc_GCC_NonSeg_m_Derivative_2nd(self, T, Pw, rhoW, rhoI) result(Suction_Derivative)
        implicit none
        class(type_gcc_non_segregation_m), intent(in) :: self
        real(real64), intent(in) :: T
        real(real64), intent(in), optional :: Pw
        real(real64), intent(in), optional :: rhoW
        real(real64), intent(in), optional :: rhoI
        real(real64) :: Suction_Derivative

        if (T <= self%Tf) then
            Suction_Derivative = self%Lf / ((T + self%TtoK)**2.0d0 * self%g)
        else
            Suction_Derivative = 0.0d0
        end if
    end function Calc_GCC_NonSeg_m_Derivative_2nd

end submodule gcc_non_segregation_m
