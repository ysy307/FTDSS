submodule(Calculate_WRF) Calculate_WRF_VG
    implicit none
contains
    module function Construct_Type_WRF_VG(Input) result(structure)
        implicit none
        type(Input_Region), intent(in) :: Input
        class(Abstract_WRF), allocatable :: structure

        if (allocated(structure)) deallocate (structure)
        allocate (Type_WRF_VG :: structure)

        select type (this => structure)
        type is (Type_WRF_VG)
            this%thetaR = Input%Ice%thetaR
            this%thetaS = Input%Ice%thetaS
            this%alpha1 = Input%Ice%alpha1
            this%n1 = Input%Ice%n1
            this%m1 = 1.0d0 - 1.0d0 / Input%Ice%n1
        end select

    end function Construct_Type_WRF_VG

    module function Calculate_WRF_VG(self, h) result(thetaW)
        implicit none
        class(Type_WRF_VG), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: thetaW

        if (h < 0.0d0) then
            thetaW = self%thetaR + (self%thetaS - self%thetaR) * (1.0d0 + (-self%alpha1 * h)**self%n1)**(-self%m1)
        else
            thetaW = self%thetaS
        end if

    end function Calculate_WRF_VG

    module function Calculate_WRF_VG_Derivative(self, h) result(Cw)
        implicit none
        class(Type_WRF_VG), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: Cw

        if (h < 0.0d0) then
            Cw = (self%thetaS - self%thetaR) * &
                 self%alpha1**self%n1 * self%m1 * self%n1 * (-h)**(self%n1 - 1.0d0) &
                 * (1.0d0 + (-self%alpha1 * h)**self%n1)**(-self%m1 - 1.0d0)
        else
            Cw = 0.0d0
        end if
    end function Calculate_WRF_VG_Derivative

end submodule Calculate_WRF_VG
