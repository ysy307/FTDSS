submodule(physics_models_wrf) wrf_mvg
    implicit none
contains
    module subroutine calculate_wrf_mvg(self, h, Qw)
        implicit none
        class(type_wrf_mvg), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: Qw

        if (h < self%config%h_crit) then
            Qw = self%config%theta_r + (self%config%theta_s - self%config%theta_r) * &
                 (1.0d0 + abs(self%config%alpha1 * h)**self%config%n1)**(-self%config%m1)
        else
            Qw = self%config%theta_s
        end if

    end subroutine calculate_wrf_mvg

    module subroutine calculate_wrf_mvg_derivative(self, h, dQw_dh)
        implicit none
        class(type_wrf_mvg), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: dQw_dh

        if (h < self%config%h_crit) then
            dQw_dh = (self%config%theta_s - self%config%theta_r) * &
                     self%config%alpha1**self%config%n1 * self%config%m1 * self%config%n1 * (-h)**(self%config%n1 - 1.0d0) * &
                     (1.0d0 + (-self%config%alpha1 * h)**self%config%n1)**(-self%config%m1 - 1.0d0)
        else
            dQw_dh = 0.0d0
        end if

    end subroutine calculate_wrf_mvg_derivative

end submodule wrf_mvg
