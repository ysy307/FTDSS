submodule(physics_models_wrf) wrf_ko
    implicit none
contains
    module function construct_type_wrf_ko(input, i_material) result(property)
        implicit none
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: i_material
        class(abst_wrf), allocatable :: property

        if (allocated(property)) deallocate (property)
        allocate (type_wrf_ko :: property)

        select type (this => property)
        type is (type_wrf_ko)
            this%theta_r = input%basic%materials(i_material)%thermal%phase_change%wrf%theta_r
            this%theta_s = input%basic%materials(i_material)%thermal%phase_change%wrf%theta_s
            this%alpha1 = input%basic%materials(i_material)%thermal%phase_change%wrf%alpha1
            this%n1 = input%basic%materials(i_material)%thermal%phase_change%wrf%n1
        end select

    end function construct_type_wrf_ko

    module pure elemental function calculate_wrf_ko(self, h) result(theta_w)
        implicit none
        class(type_wrf_ko), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: theta_w

        if (h < 0.0d0) then
            theta_w = self%theta_r + (self%theta_s - self%theta_r) * 0.5d0 * erfc(log(h / self%alpha1) / (self%n1 * sqrt(2.0d0)))
        else
            theta_w = self%theta_s
        end if

    end function calculate_wrf_ko

    module pure elemental function calculate_wrf_ko_derivative(self, h) result(dqw_dh)
        implicit none
        class(type_wrf_ko), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: dqw_dh

        if (h < 0.0d0) then
            dqw_dh = -(self%theta_s - self%theta_r) * &
                     exp(-(log(h / self%alpha1))**2.0d0 / (2.0d0 * self%n1**2.0d0)) / &
                     (sqrt(2.0d0 * pi) * h * self%n1)
        else
            dqw_dh = 0.0d0
        end if
    end function calculate_wrf_ko_derivative

end submodule wrf_ko
