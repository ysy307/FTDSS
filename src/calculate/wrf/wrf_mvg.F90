submodule(calculate_wrf) calculate_wrf_mvg
    implicit none
contains
    module function construct_type_wrf_mvg(Input) result(property)
        implicit none
        type(Input_Region), intent(in) :: Input
        class(Abst_WRF), allocatable :: property

        if (allocated(property)) deallocate (property)
        allocate (type_wrf_mvg :: property)

        select type (this => property)
        type is (type_wrf_mvg)
            this%theta_r = Input%Ice%thetaR
            this%theta_s = Input%Ice%thetaS
            this%alpha1 = Input%Ice%alpha1
            this%n1 = Input%Ice%n1
            this%m1 = 1.0d0 - 1.0d0 / Input%Ice%n1
            this%h_crit = Input%Ice%hcrit
        end select

    end function construct_type_wrf_mvg

    module function calculate_wrf_mvg(self, h) result(theta_w)
        implicit none
        class(type_wrf_mvg), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: theta_w

        if (h < self%h_crit) then
            theta_w = self%theta_r + (self%theta_s - self%theta_r) * (1.0d0 + abs(self%alpha1 * h)**self%n1)**(-self%m1)
        else
            theta_w = self%theta_s
        end if

    end function calculate_wrf_mvg

    module function calculate_wrf_mvg_derivative(self, h) result(dvw_dh)
        implicit none
        class(type_wrf_mvg), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: dvw_dh

        if (h < self%h_crit) then
            dvw_dh = (self%theta_s - self%theta_r) * &
                     self%alpha1**self%n1 * self%m1 * self%n1 * (-h)**(self%n1 - 1.0d0) * &
                     (1.0d0 + (-self%alpha1 * h)**self%n1)**(-self%m1 - 1.0d0)
        else
            dvw_dh = 0.0d0
        end if

    end function calculate_wrf_mvg_derivative

end submodule calculate_wrf_mvg
