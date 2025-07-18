submodule(calculate_wrf) calculate_wrf_dvgch
    implicit none
contains
    module function construct_type_wrf_dvgch(input) result(property)
        implicit none
        type(type_materials_wrf), intent(in) :: input
        class(Abst_WRF), allocatable :: property

        if (allocated(property)) deallocate (property)
        allocate (type_wrf_dvgch :: property)

        select type (this => property)
        type is (type_wrf_dvgch)
            this%theta_r = input%theta_r
            this%theta_s = input%theta_s
            this%alpha1 = input%alpha1
            this%n1 = input%n1
        end select

    end function construct_type_wrf_dvgch

    module function calculate_wrf_dvgch(self, h) result(theta_w)
        implicit none
        class(type_wrf_dvgch), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: theta_w

        if (h < 0.0d0) then
            theta_w = self%theta_r + (self%theta_s - self%theta_r) * &
                      (self%w1 * (1.0d0 + abs(self%alpha1 * h)**self%n1)**(-self%m1) &
                       + self%w2 * (1.0d0 + abs(self%alpha1 * h)**self%n2)**(-self%m2))
        else
            theta_w = self%theta_s
        end if

    end function calculate_wrf_dvgch

    module function calculate_wrf_dvgch_Derivative(self, h) result(dqw_dh)
        implicit none
        class(type_wrf_dvgch), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: dqw_dh

        if (h < 0.0d0) then
            dqw_dh = (self%theta_s - self%theta_r) * &
                     (self%w1 * self%alpha1**self%n1 * self%m1 * self%n1 * (-h)**(self%n1 - 1.0d0) * &
                      (1.0d0 + (-self%alpha1 * h)**self%n1)**(-self%m1 - 1.0d0) &
                      + self%w2 * self%alpha1**self%n2 * self%m2 * self%n2 * (-h)**(self%n2 - 1.0d0) * &
                      (1.0d0 + (-self%alpha1 * h)**self%n2)**(-self%m2 - 1.0d0))
        else
            dqw_dh = 0.0d0
        end if
    end function calculate_wrf_dvgch_Derivative

end submodule calculate_wrf_dvgch
