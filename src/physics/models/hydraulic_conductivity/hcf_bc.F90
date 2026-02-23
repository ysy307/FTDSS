submodule(physics_models_hcf) hcf_bc
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Calculate kr for Brooks and Corey model
    !----------------------------------------------------------------------------------------------------
    subroutine calc_kr_bc(alpha1, n1, l, h, kr)
        implicit none
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: l
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: kr

        real(real64) :: Sw

        if (h < alpha1) then
            Sw = (h / alpha1)**(-n1)
        else
            Sw = 1.0d0
        end if

        kr = Sw**(2.0d0 / n1 + l + 2.0d0)

    end subroutine calc_kr_bc

    module subroutine calc_kr_base_bc(self, h, kr)
        implicit none
        class(type_hcf_base_bc), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: kr

        associate (params => self%parent%config)
            call calc_kr_bc(params%alpha1, params%n1, params%l, h, kr)
        end associate

    end subroutine calc_kr_base_bc

end submodule hcf_bc
