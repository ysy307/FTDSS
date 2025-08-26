submodule(calculate_hcf) calculate_hcf_bc
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Constructe each types by using Brooks and Corey model
    !----------------------------------------------------------------------------------------------------
    module function construct_type_hcf_base_bc(alpha1, n1, l) result(structure)
        implicit none
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: l
        class(abst_hcf_base), allocatable :: structure

        if (allocated(structure)) deallocate (structure)
        allocate (type_hcf_base_bc :: structure)

        structure%alpha1 = alpha1
        structure%n1 = n1
        structure%l = l

    end function construct_type_hcf_base_bc
    !----------------------------------------------------------------------------------------------------
    ! Calculate kr for Brooks and Corey model
    !----------------------------------------------------------------------------------------------------
    pure elemental function calc_kr_bc(alpha1, n1, l, h) result(kr)
        implicit none
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: l
        real(real64), intent(in) :: h
        real(real64) :: kr
        real(real64) :: s_w

        if (h < alpha1) then
            s_w = (h / alpha1)**(-n1)
        else
            s_w = 1.0d0
        end if

        kr = s_w**(2.0d0 / n1 + l + 2.0d0)

    end function calc_kr_bc

    module pure elemental function calc_kr_base_bc(self, h) result(kr)
        implicit none
        class(type_hcf_base_bc), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = calc_kr_bc(self%alpha1, self%n1, self%l, h)

    end function calc_kr_base_bc

end submodule calculate_hcf_bc
