submodule(Calculate_GCC) gcc_non_segregation_pa
    implicit none
contains

    module function type_GCC_NonSeg_Pa_Construct(Tf, Lf) result(property)
        implicit none
        real(real64), intent(in) :: Tf
        real(real64), intent(in) :: Lf
        class(abst_gcc), allocatable :: property

        if (allocated(property)) deallocate (property)
        allocate (type_gcc_non_segregation_pa :: property)

        select type (this => property)
        type is (type_gcc_non_segregation_pa)
            this%Lf = Lf
            this%Tf = Tf
        end select

    end function type_GCC_NonSeg_Pa_Construct

    module function Calc_GCC_NonSeg_Pa(self, T, Pw, rhoW, rhoI) result(suction)
        implicit none
        class(type_gcc_non_segregation_pa), intent(in) :: self
        real(real64), intent(in) :: T
        real(real64), intent(in), optional :: Pw
        real(real64), intent(in), optional :: rhoW
        real(real64), intent(in), optional :: rhoI
        real(real64) :: suction

        if (T <= self%Tf) then
            suction = -self%Lf * rhoW * log((T + self%TtoK) / (self%Tf + self%TtoK))
        else
            suction = 0.0d0
        end if

    end function Calc_GCC_NonSeg_Pa

    module function Calc_GCC_NonSeg_Pa_Derivative(self, T, Pw, rhoW, rhoI) result(suction_derivative)
        implicit none
        class(type_gcc_non_segregation_pa), intent(in) :: self
        real(real64), intent(in) :: T
        real(real64), intent(in), optional :: Pw
        real(real64), intent(in), optional :: rhoW
        real(real64), intent(in), optional :: rhoI
        real(real64) :: suction_derivative

        if (T <= self%Tf) then
            suction_derivative = -self%Lf * rhoW / (T + self%TtoK)
        else
            suction_derivative = 0.0d0
        end if

    end function Calc_GCC_NonSeg_Pa_Derivative

    module function Calc_GCC_NonSeg_Pa_Derivative_2nd(self, T, Pw, rhoW, rhoI) result(suction_derivative)
        implicit none
        class(type_gcc_non_segregation_pa), intent(in) :: self
        real(real64), intent(in) :: T
        real(real64), intent(in), optional :: Pw
        real(real64), intent(in), optional :: rhoW
        real(real64), intent(in), optional :: rhoI
        real(real64) :: suction_derivative

        if (T <= self%Tf) then
            suction_derivative = self%Lf * rhoW / (T + self%TtoK)**2.0d0
        else
            suction_derivative = 0.0d0
        end if

    end function Calc_GCC_NonSeg_Pa_Derivative_2nd

end submodule gcc_non_segregation_pa
