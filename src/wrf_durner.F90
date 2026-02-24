submodule(physics_models_wrf) wrf_durner
    implicit none
contains
    module subroutine calculate_wrf_durner(self, h, Qw)
        implicit none
        class(type_wrf_durner), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: Qw

        if (h < 0.0d0) then
            Qw = self%config%theta_r + (self%config%theta_s - self%config%theta_r) * &
                 (self%config%w1 * (1.0d0 + abs(self%config%alpha1 * h)**self%config%n1)**(-self%config%m1) &
                  + self%config%w2 * (1.0d0 + abs(self%config%alpha2 * h)**self%config%n2)**(-self%config%m2))
        else
            Qw = self%config%theta_s
        end if

    end subroutine calculate_wrf_durner

    module subroutine calculate_wrf_durner_derivative(self, h, dQw_dh)
        implicit none
        class(type_wrf_durner), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: dQw_dh

        if (h < 0.0d0) then
            dQw_dh = (self%config%theta_s - self%config%theta_r) * &
                     (self%config%w1 * self%config%alpha1**self%config%n1 * &
                      self%config%m1 * self%config%n1 * (-h)**(self%config%n1 - 1.0d0) * &
                      (1.0d0 + (-self%config%alpha1 * h)**self%config%n1)**(-self%config%m1 - 1.0d0) &
                      + self%config%w2 * self%config%alpha2**self%config%n2 * &
                      self%config%m2 * self%config%n2 * (-h)**(self%config%n2 - 1.0d0) * &
                      (1.0d0 + (-self%config%alpha2 * h)**self%config%n2)**(-self%config%m2 - 1.0d0))
        else
            dQw_dh = 0.0d0
        end if

    end subroutine calculate_wrf_durner_derivative

end submodule wrf_durner
