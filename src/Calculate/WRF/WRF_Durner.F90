submodule(Calculate_WRF) Calculate_WRF_Durner
    implicit none
contains
    module function Construct_Type_WRF_Durner(Input) result(structure)
        implicit none
        type(Input_Region), intent(in) :: Input
        class(Abstract_WRF), allocatable :: structure

        if (allocated(structure)) deallocate (structure)
        allocate (Type_WRF_Durner :: structure)

        select type (this => structure)
        type is (Type_WRF_Durner)
            this%thetaR = Input%Ice%thetaR
            this%thetaS = Input%Ice%thetaS
            this%alpha1 = Input%Ice%alpha1
            this%n1 = Input%Ice%n1
            this%m1 = 1.0d0 - 1.0d0 / this%n1
            this%w1 = Input%Ice%w1
            this%alpha2 = Input%Ice%alpha2
            this%n2 = Input%Ice%n2
            this%m2 = 1.0d0 - 1.0d0 / this%n2
            this%w2 = 1.0d0 - this%w1
        end select

    end function Construct_Type_WRF_Durner

    module function Calculate_WRF_Durner(self, h) result(thetaW)
        implicit none
        class(Type_WRF_Durner), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: thetaW

        if (h < 0.0d0) then
            thetaW = self%thetaR + (self%thetaS - self%thetaR) * &
                     (self%w1 * (1.0d0 + abs(self%alpha1 * h)**self%n1)**(-self%m1) &
                      + self%w2 * (1.0d0 + abs(self%alpha2 * h)**self%n2)**(-self%m2))
        else
            thetaW = self%thetaS
        end if

    end function Calculate_WRF_Durner

    module function Calculate_WRF_Durner_Derivative(self, h) result(Cw)
        implicit none
        class(Type_WRF_Durner), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: Cw

        if (h < 0.0d0) then
            Cw = (self%thetaS - self%thetaR) * &
                 (self%w1 * self%alpha1**self%n1 * self%m1 * self%n1 * (-h)**(self%n1 - 1.0d0) * &
                  (1.0d0 + (-self%alpha1 * h)**self%n1)**(-self%m1 - 1.0d0) &
                  + self%w2 * self%alpha2**self%n2 * self%m2 * self%n2 * (-h)**(self%n2 - 1.0d0) * &
                  (1.0d0 + (-self%alpha2 * h)**self%n2)**(-self%m2 - 1.0d0))
        else
            Cw = 0.0d0
        end if

    end function Calculate_WRF_Durner_Derivative

end submodule Calculate_WRF_Durner
