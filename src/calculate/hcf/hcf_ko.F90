submodule(calculate_hcf) calculate_hcf_ko
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Constructe each types by using Kosugi model
    !----------------------------------------------------------------------------------------------------
    module function construct_type_hcf_base_ko(alpha1, n1, l) result(structure)
        implicit none
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: l
        class(abst_hcf_base), allocatable :: structure

        if (allocated(structure)) deallocate (structure)
        allocate (type_hcf_base_ko :: structure)

        structure%alpha1 = alpha1
        structure%n1 = n1
        structure%l = l

    end function construct_type_hcf_base_ko
    !----------------------------------------------------------------------------------------------------
    ! Calculate kr for Kosugi model
    !----------------------------------------------------------------------------------------------------
    pure elemental function calc_kr_ko(alpha1, n1, l, h) result(kr)
        implicit none
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: l
        real(real64), intent(in) :: h
        real(real64) :: kr
        real(real64) :: s_w

        if (h < 0.0d0) then
            s_w = 0.5d0 * erfc(log(h / alpha1) / (n1 * sqrt(2.0d0)))
            kr = s_w**l * (0.5d0 * erfc(log(h / alpha1) / (n1 * sqrt(2.0d0)) + n1 / sqrt(2.0d0)))**2.0d0
        else
            kr = 1.0d0
        end if

    end function calc_kr_ko

    module pure elemental function calc_kr_base_ko(self, h) result(kr)
        implicit none
        class(type_hcf_base_ko), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = calc_kr_ko(self%alpha1, self%n1, self%l, h)

    end function calc_kr_base_ko

end submodule calculate_hcf_ko
