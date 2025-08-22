submodule(calculate_wrf) calculate_wrf_vg
    implicit none
contains
    module function construct_type_wrf_vg(input) result(property)
        implicit none
        type(type_materials_wrf), intent(in) :: input
        class(abst_wrf), allocatable :: property

        if (allocated(property)) deallocate (property)
        allocate (type_wrf_vg :: property)

        select type (this => property)
        type is (type_wrf_vg)
            this%theta_r = input%theta_r
            this%theta_s = input%theta_s
            this%alpha1 = input%alpha1
            this%n1 = input%n1
            this%m1 = 1.0d0 - 1.0d0 / input%n1
        end select

    end function construct_type_wrf_vg

    module pure function calculate_wrf_vg(self, h) result(theta_w)
        implicit none
        class(type_wrf_vg), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: theta_w

        if (h < 0.0d0) then
            theta_w = self%theta_r + (self%theta_s - self%theta_r) * (1.0d0 + (-self%alpha1 * h)**self%n1)**(-self%m1)
        else
            theta_w = self%theta_s
        end if

    end function calculate_wrf_vg

    module pure function calculate_wrf_vg_derivative(self, h) result(dqw_dh)
        implicit none
        class(type_wrf_vg), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: dqw_dh

        if (h < 0.0d0) then
            dqw_dh = (self%theta_s - self%theta_r) * &
                     self%alpha1**self%n1 * self%m1 * self%n1 * (-h)**(self%n1 - 1.0d0) &
                     * (1.0d0 + (-self%alpha1 * h)**self%n1)**(-self%m1 - 1.0d0)
        else
            dqw_dh = 0.0d0
        end if
    end function calculate_wrf_vg_derivative

end submodule calculate_wrf_vg
