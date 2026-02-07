submodule(physics_models_wrf) wrf_bc
    implicit none
contains

    module subroutine calculate_wrf_bc(self, h, Qw)
        implicit none
        class(type_wrf_bc), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: Qw

        if (h < self%params%alpha1) then
            Qw = self%params%theta_r + (self%params%theta_s - self%params%theta_r) * (self%params%alpha1 / h)**self%params%n1
        else
            Qw = self%params%theta_s
        end if

    end subroutine calculate_wrf_bc

    module subroutine calculate_wrf_bc_derivative(self, h, dQw_dh)
        implicit none
        class(type_wrf_bc), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: dQw_dh

        !@note alpha1 must be negative
        if (h < self%params%alpha1) then
            dQw_dh = -(self%params%theta_s - self%params%theta_r) * self%params%n1 &
                     * (self%params%alpha1 / h)**(self%params%n1 + 1.0d0) / self%params%alpha1
        else
            dQw_dh = 0.0d0
        end if
    end subroutine calculate_wrf_bc_derivative

end submodule wrf_bc
