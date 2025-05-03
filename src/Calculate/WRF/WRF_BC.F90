submodule(Calculate_WRF) Calculate_WRF_BC
    implicit none
contains
    module function Construct_Type_WRF_BC(Input) result(structure)
        implicit none
        type(Input_Region), intent(in) :: Input
        class(Abstract_WRF), allocatable :: structure

        if (allocated(structure)) deallocate (structure)
        allocate (Type_WRF_BC :: structure)

        select type (this => structure)
        type is (Type_WRF_BC)
            this%thetaR = Input%Ice%thetaR
            this%thetaS = Input%Ice%thetaS
            this%alpha1 = Input%Ice%alpha1
            this%n1 = Input%Ice%n1
        end select

    end function Construct_Type_WRF_BC

    module function Calculate_WRF_BC(self, h) result(thetaW)
        implicit none
        class(Type_WRF_BC), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: thetaW

        if (h < self%alpha1) then
            thetaW = self%thetaR + (self%thetaS - self%thetaR) * (self%alpha1 / h)**self%n1
        else
            thetaW = self%thetaS
        end if

    end function Calculate_WRF_BC

    module function Calculate_WRF_BC_Derivative(self, h) result(Cw)
        implicit none
        class(Type_WRF_BC), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: Cw

        !@note alpha1 must be negative
        if (h < self%alpha1) then
            Cw = -(self%thetaS - self%thetaR) * self%n1 * (self%alpha1 / h)**(self%n1 + 1.0d0) / self%alpha1
        else
            Cw = 0.0d0
        end if
    end function Calculate_WRF_BC_Derivative

end submodule Calculate_WRF_BC
