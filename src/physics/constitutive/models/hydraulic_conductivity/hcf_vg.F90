submodule(physics_models_hcf) hcf_vg
    implicit none
contains

    !----------------------------------------------------------------------------------------------------
    ! Calculate kr for van-Genuchten model
    !----------------------------------------------------------------------------------------------------
    subroutine calc_kr_vg(alpha1, n1, m1, l, h, kr)
        implicit none
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: m1
        real(real64), intent(in) :: l
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: kr

        real(real64) :: Sw

        if (h < 0.0d0) then
            Sw = (1.0d0 + (-alpha1 * h)**n1)**(-m1)
        else
            Sw = 1.0d0
        end if

        kr = Sw**l * (1.0d0 - (1.0d0 - Sw**(1.0d0 / m1))**m1)**2

    end subroutine calc_kr_vg

    !----------------------------------------------------------------------------------------------------
    ! Wrapper of calculating kr for van-Genuchten model bounding different derived types
    !----------------------------------------------------------------------------------------------------
    module subroutine calc_kr_base_vg(self, h, kr)
        implicit none
        class(type_hcf_base_vg), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: kr

        associate (params => self%parent%config)
            call calc_kr_vg(params%alpha1, params%n1, params%m1, params%l, h, kr)
        end associate
    end subroutine calc_kr_base_vg

end submodule hcf_vg
