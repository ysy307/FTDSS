submodule(Calculate_WRF) Calculate_WRF_KO
    implicit none
contains
    module function Construct_Type_WRF_KO(Input) result(structure)
        implicit none
        type(Input_Region), intent(in) :: Input
        class(Abst_WRF), allocatable :: structure

        if (allocated(structure)) deallocate (structure)
        allocate (Type_WRF_KO :: structure)

        select type (this => structure)
        type is (Type_WRF_KO)
            this%thetaR = Input%Ice%thetaR
            this%thetaS = Input%Ice%thetaS
            this%alpha1 = Input%Ice%alpha1
            this%n1 = Input%Ice%n1
        end select

    end function Construct_Type_WRF_KO

    module function Calculate_WRF_KO(self, h) result(thetaW)
        implicit none
        class(Type_WRF_KO), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: thetaW

        if (h < 0.0d0) then
            thetaW = self%thetaR + (self%thetaS - self%thetaR) * 0.5d0 * erfc(log(h / self%alpha1) / (self%n1 * sqrt(2.0d0)))
        else
            thetaW = self%thetaS
        end if

    end function Calculate_WRF_KO

    module function Calculate_WRF_KO_Derivative(self, h) result(Cw)
        implicit none
        class(Type_WRF_KO), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: Cw

        if (h < 0.0d0) then
            Cw = -(self%thetaS - self%thetaR) * &
                 exp(-(log(h / self%alpha1))**2.0d0 / (2.0d0 * self%n1**2.0d0)) / &
                 (sqrt(2.0d0 * pi) * h * self%n1)
        else
            Cw = 0.0d0
        end if
    end function Calculate_WRF_KO_Derivative

end submodule Calculate_WRF_KO
