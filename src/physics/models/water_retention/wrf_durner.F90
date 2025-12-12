submodule(physics_models_wrf) wrf_durner
    implicit none
contains
    module pure elemental subroutine calculate_wrf_durner(self, h, Qw)
        implicit none
        class(type_wrf_durner), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: Qw

        if (h < 0.0d0) then
            Qw = self%params%theta_r + (self%params%theta_s - self%params%theta_r) * &
                 (self%params%w1 * (1.0d0 + abs(self%params%alpha1 * h)**self%params%n1)**(-self%params%m1) &
                  + self%params%w2 * (1.0d0 + abs(self%params%alpha2 * h)**self%params%n2)**(-self%params%m2))
        else
            Qw = self%params%theta_s
        end if

    end subroutine calculate_wrf_durner

    module pure elemental subroutine calculate_wrf_durner_derivative(self, h, dQw_dh)
        implicit none
        class(type_wrf_durner), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: dQw_dh

        if (h < 0.0d0) then
            dQw_dh = (self%params%theta_s - self%params%theta_r) * &
                     (self%params%w1 * self%params%alpha1**self%params%n1 * &
                      self%params%m1 * self%params%n1 * (-h)**(self%params%n1 - 1.0d0) * &
                      (1.0d0 + (-self%params%alpha1 * h)**self%params%n1)**(-self%params%m1 - 1.0d0) &
                      + self%params%w2 * self%params%alpha2**self%params%n2 * &
                      self%params%m2 * self%params%n2 * (-h)**(self%params%n2 - 1.0d0) * &
                      (1.0d0 + (-self%params%alpha2 * h)**self%params%n2)**(-self%params%m2 - 1.0d0))
        else
            dQw_dh = 0.0d0
        end if

    end subroutine calculate_wrf_durner_derivative

end submodule wrf_durner
