submodule(Calculate_GCC) GCC_Segregation_Pa
    implicit none
contains

    module function Type_GCC_Seg_Pa_Construct(Tf, Lf) result(structure)
        implicit none
        real(real64), intent(in) :: Tf
        real(real64), intent(in) :: Lf
        class(Abst_GCC), allocatable :: structure

        if (allocated(structure)) deallocate (structure)
        allocate (Type_GCC_Segregation_Pa :: structure)

        select type (this => structure)
        type is (Type_GCC_Segregation_Pa)
            this%Lf = Lf
            this%Tf = Tf
        end select

    end function Type_GCC_Seg_Pa_Construct

    module function Calc_GCC_Seg_Pa(self, T, Pw, rhoW, rhoI) result(Suction)
        implicit none
        class(Type_GCC_Segregation_Pa), intent(in) :: self
        real(real64), intent(in) :: T
        real(real64), intent(in), optional :: Pw
        real(real64), intent(in), optional :: rhoW
        real(real64), intent(in), optional :: rhoI
        real(real64) :: Suction

        if (T <= self%Tf) then
            Suction = (rhoI / rhoW - 1.0d0) * Pw - self%Lf * rhoI * log((T + self%TtoK) / (self%Tf + self%TtoK))
        else
            Suction = 0.0d0
        end if

    end function Calc_GCC_Seg_Pa

    module function Calc_GCC_Seg_Pa_Derivative(self, T, Pw, rhoW, rhoI) result(Suction_Derivative)
        implicit none
        class(Type_GCC_Segregation_Pa), intent(in) :: self
        real(real64), intent(in) :: T
        real(real64), intent(in), optional :: Pw
        real(real64), intent(in), optional :: rhoW
        real(real64), intent(in), optional :: rhoI
        real(real64) :: Suction_Derivative

        if (T <= self%Tf) then
            Suction_Derivative = -self%Lf * rhoI / (T + self%TtoK)
        else
            Suction_Derivative = 0.0d0
        end if

    end function Calc_GCC_Seg_Pa_Derivative

    module function Calc_GCC_Seg_Pa_Derivative_2nd(self, T, Pw, rhoW, rhoI) result(Suction_Derivative)
        implicit none
        class(Type_GCC_Segregation_Pa), intent(in) :: self
        real(real64), intent(in) :: T
        real(real64), intent(in), optional :: Pw
        real(real64), intent(in), optional :: rhoW
        real(real64), intent(in), optional :: rhoI
        real(real64) :: Suction_Derivative

        if (T <= self%Tf) then
            Suction_Derivative = self%Lf * rhoI / (T + self%TtoK)**2.0d0
        else
            Suction_Derivative = 0.0d0
        end if

    end function Calc_GCC_Seg_Pa_Derivative_2nd

end submodule GCC_Segregation_Pa
