submodule(physics_models_wrf) wrf_ko
    implicit none
contains
    module pure elemental subroutine calculate_wrf_ko(self, h, Qw)
        implicit none
        class(type_wrf_ko), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: Qw

        if (h < 0.0d0) then
            Qw = self%params%theta_r + (self%params%theta_s - self%params%theta_r) &
                 * 0.5d0 * erfc(log(h / self%params%alpha1) / (self%params%n1 * sqrt(2.0d0)))
        else
            Qw = self%params%theta_s
        end if

    end subroutine calculate_wrf_ko

    module pure elemental subroutine calculate_wrf_ko_derivative(self, h, dQw_dh)
        implicit none
        class(type_wrf_ko), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: dQw_dh

        if (h < 0.0d0) then
            dqw_dh = -(self%params%theta_s - self%params%theta_r) * &
                     exp(-(log(h / self%params%alpha1))**2.0d0 / (2.0d0 * self%params%n1**2.0d0)) / &
                     (sqrt(2.0d0 * pi) * h * self%params%n1)
        else
            dqw_dh = 0.0d0
        end if
    end subroutine calculate_wrf_ko_derivative

end submodule wrf_ko
