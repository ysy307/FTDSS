submodule(calculate_wrf) calculate_wrf_durner
    implicit none
contains
    module function construct_type_wrf_durner(Input) result(property)
        implicit none
        type(Input_Region), intent(in) :: Input
        class(abst_wrf), allocatable :: property

        if (allocated(property)) deallocate (property)
        allocate (type_wrf_durner :: property)

        select type (this => property)
        type is (type_wrf_durner)
            this%theta_r = Input%Ice%thetaR
            this%theta_s = Input%Ice%thetaS
            this%alpha1 = Input%Ice%alpha1
            this%n1 = Input%Ice%n1
            this%m1 = 1.0d0 - 1.0d0 / this%n1
            this%w1 = Input%Ice%w1
            this%alpha2 = Input%Ice%alpha2
            this%n2 = Input%Ice%n2
            this%m2 = 1.0d0 - 1.0d0 / this%n2
            this%w2 = 1.0d0 - this%w1
        end select

    end function construct_type_wrf_durner

    module function calculate_wrf_durner(self, h) result(theta_w)
        implicit none
        class(type_wrf_durner), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: theta_w

        if (h < 0.0d0) then
            theta_w = self%theta_r + (self%theta_s - self%theta_r) * &
                      (self%w1 * (1.0d0 + abs(self%alpha1 * h)**self%n1)**(-self%m1) &
                       + self%w2 * (1.0d0 + abs(self%alpha2 * h)**self%n2)**(-self%m2))
        else
            theta_w = self%theta_s
        end if

    end function calculate_wrf_durner

    module function calculate_wrf_durner_derivative(self, h) result(dqw_dh)
        implicit none
        class(type_wrf_durner), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: dqw_dh

        if (h < 0.0d0) then
            dqw_dh = (self%theta_s - self%theta_r) * &
                     (self%w1 * self%alpha1**self%n1 * self%m1 * self%n1 * (-h)**(self%n1 - 1.0d0) * &
                      (1.0d0 + (-self%alpha1 * h)**self%n1)**(-self%m1 - 1.0d0) &
                      + self%w2 * self%alpha2**self%n2 * self%m2 * self%n2 * (-h)**(self%n2 - 1.0d0) * &
                      (1.0d0 + (-self%alpha2 * h)**self%n2)**(-self%m2 - 1.0d0))
        else
            dqw_dh = 0.0d0
        end if

    end function calculate_wrf_durner_derivative

end submodule calculate_wrf_durner
