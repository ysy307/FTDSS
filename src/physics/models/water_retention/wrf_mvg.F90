submodule(physics_models_wrf) wrf_mvg
    implicit none
contains
    module subroutine calculate_wrf_mvg(self, h, Qw)
        implicit none
        class(type_wrf_mvg), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: Qw

        if (h < self%params%h_crit) then
            Qw = self%params%theta_r + (self%params%theta_s - self%params%theta_r) * &
                 (1.0d0 + abs(self%params%alpha1 * h)**self%params%n1)**(-self%params%m1)
        else
            Qw = self%params%theta_s
        end if

    end subroutine calculate_wrf_mvg

    module subroutine calculate_wrf_mvg_derivative(self, h, dQw_dh)
        implicit none
        class(type_wrf_mvg), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: dQw_dh

        if (h < self%params%h_crit) then
            dQw_dh = (self%params%theta_s - self%params%theta_r) * &
                     self%params%alpha1**self%params%n1 * self%params%m1 * self%params%n1 * (-h)**(self%params%n1 - 1.0d0) * &
                     (1.0d0 + (-self%params%alpha1 * h)**self%params%n1)**(-self%params%m1 - 1.0d0)
        else
            dQw_dh = 0.0d0
        end if

    end subroutine calculate_wrf_mvg_derivative

end submodule wrf_mvg
