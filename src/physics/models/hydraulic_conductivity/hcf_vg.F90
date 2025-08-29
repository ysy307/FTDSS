submodule(physics_models_hcf) hcf_vg
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Constructe each types by using van-Genuchten model
    !----------------------------------------------------------------------------------------------------
    module function construct_type_hcf_base_vg(alpha1, n1, l) result(structure)
        implicit none
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: l
        class(abst_hcf_base), allocatable :: structure

        if (allocated(structure)) deallocate (structure)
        allocate (type_hcf_base_vg :: structure)

        structure%alpha1 = alpha1
        structure%n1 = n1
        structure%m1 = 1.0d0 - 1.0d0 / n1
        structure%l = l

    end function construct_type_hcf_base_vg

    !----------------------------------------------------------------------------------------------------
    ! Calculate kr for van-Genuchten model
    !----------------------------------------------------------------------------------------------------
    pure elemental function calc_kr_vg(alpha1, n1, m1, l, h) result(kr)
        implicit none
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: m1
        real(real64), intent(in) :: l
        real(real64), intent(in) :: h
        real(real64) :: kr
        real(real64) :: s_w

        if (h < 0.0d0) then
            s_w = (1.0d0 + (-alpha1 * h)**n1)**(-m1)
        else
            s_w = 1.0d0
        end if

        kr = s_w**l * (1.0d0 - (1.0d0 - s_w**(1.0d0 / m1))**m1)**2.0d0

    end function calc_kr_vg

    !----------------------------------------------------------------------------------------------------
    ! Wrapper of calculating kr for van-Genuchten model bounding different derived types
    !----------------------------------------------------------------------------------------------------
    module pure elemental function calc_kr_base_vg(self, h) result(kr)
        implicit none
        class(type_hcf_base_vg), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = calc_kr_vg(self%alpha1, self%n1, self%m1, self%l, h)

    end function calc_kr_base_vg

end submodule hcf_vg
