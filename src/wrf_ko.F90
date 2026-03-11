submodule(models_wrf) wrf_ko
    implicit none
contains
    module subroutine calculate_wrf_ko(self, h, Qw)
        implicit none
        class(type_wrf_ko), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: Qw

        if (h < 0.0d0) then
            Qw = self%config%theta_r + (self%config%theta_s - self%config%theta_r) &
                 * 0.5d0 * erfc(log(h / self%config%alpha1) / (self%config%n1 * sqrt(2.0d0)))
        else
            Qw = self%config%theta_s
        end if

    end subroutine calculate_wrf_ko

    module subroutine calculate_wrf_ko_derivative(self, h, dQw_dh)
        implicit none
        class(type_wrf_ko), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: dQw_dh

        if (h < 0.0d0) then
            dqw_dh = -(self%config%theta_s - self%config%theta_r) * &
                     exp(-(log(h / self%config%alpha1))**2.0d0 / (2.0d0 * self%config%n1**2.0d0)) / &
                     (sqrt(2.0d0 * pi) * h * self%config%n1)
        else
            dqw_dh = 0.0d0
        end if
    end subroutine calculate_wrf_ko_derivative

end submodule wrf_ko
