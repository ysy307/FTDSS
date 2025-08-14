submodule(calculate_hcf) calculate_hcf_impedance
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Constructe the type
    !----------------------------------------------------------------------------------------------------
    module function construct_type_hcf_impedance(omega) result(structure)
        implicit none
        real(real64), intent(in) :: omega
        class(abst_hcf_impedance), allocatable :: structure

        if (allocated(structure)) deallocate (structure)
        allocate (type_hcf_impedance_exp :: structure)

        structure%omega = omega

    end function construct_type_hcf_impedance

    !----------------------------------------------------------------------------------------------------
    ! Wrapper of the function to calculate the impedance factor
    !----------------------------------------------------------------------------------------------------
    module function calc_impedance_exp(self, q_ice) result(kr)
        implicit none
        class(type_hcf_impedance_exp), intent(in) :: self
        real(real64), intent(in) :: q_ice
        real(real64) :: kr

        kr = calc_impedance_exponential(self%omega, q_ice)

    end function calc_impedance_exp

    function calc_impedance_exponential(omega, q_ice) result(impedance)
        implicit none
        real(real64), intent(in) :: omega
        real(real64), intent(in) :: q_ice
        real(real64) :: impedance

        impedance = 10.0d0**(-omega * q_ice)

    end function calc_impedance_exponential

end submodule calculate_hcf_impedance
