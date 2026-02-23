submodule(physics_models_wrf) wrf_bc
    implicit none
contains

    module subroutine calculate_wrf_bc(self, h, Qw)
        implicit none
        class(type_wrf_bc), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: Qw

        if (h < self%config%alpha1) then
            Qw = self%config%theta_r + (self%config%theta_s - self%config%theta_r) * (self%config%alpha1 / h)**self%config%n1
        else
            Qw = self%config%theta_s
        end if

    end subroutine calculate_wrf_bc

    module subroutine calculate_wrf_bc_derivative(self, h, dQw_dh)
        implicit none
        class(type_wrf_bc), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: dQw_dh

        !@note alpha1 must be negative
        if (h < self%config%alpha1) then
            dQw_dh = -(self%config%theta_s - self%config%theta_r) * self%config%n1 &
                     * (self%config%alpha1 / h)**(self%config%n1 + 1.0d0) / self%config%alpha1
        else
            dQw_dh = 0.0d0
        end if
    end subroutine calculate_wrf_bc_derivative

end submodule wrf_bc
