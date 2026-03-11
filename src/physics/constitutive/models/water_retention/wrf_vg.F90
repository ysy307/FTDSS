submodule(models_wrf) wrf_vg
    implicit none
contains
    module subroutine calculate_wrf_vg(self, h, Qw)
        implicit none
        class(type_wrf_vg), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: Qw

        if (h < 0.0d0) then
            Qw = self%config%theta_r + (self%config%theta_s - self%config%theta_r) &
                 * (1.0d0 + (-self%config%alpha1 * h)**self%config%n1)**(-self%config%m1)
        else
            Qw = self%config%theta_s
        end if

    end subroutine calculate_wrf_vg

    module subroutine calculate_wrf_vg_derivative(self, h, dQw_dh)
        implicit none
        class(type_wrf_vg), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: dQw_dh
        if (h < 0.0d0) then
            dQw_dh = (self%config%theta_s - self%config%theta_r) * &
                     self%config%alpha1**self%config%n1 * self%config%m1 * self%config%n1 * (-h)**(self%config%n1 - 1.0d0) &
                     * (1.0d0 + (-self%config%alpha1 * h)**self%config%n1)**(-self%config%m1 - 1.0d0)
        else
            dQw_dh = 0.0d0
        end if
    end subroutine calculate_wrf_vg_derivative

end submodule wrf_vg
