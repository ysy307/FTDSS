submodule(calculate_hcf) calculate_hcf_base
    implicit none
contains

    module function calc_kflh_base(self, state) result(kflh)
        implicit none
        class(type_hcf_base), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64) :: kflh

        kflh = self%k_s * self%base%calc_kr(state%pressure)

    end function calc_kflh_base

    module function calc_kflh_impedance(self, state) result(kflh)
        implicit none
        class(type_hcf_impedance), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64) :: kflh

        kflh = self%k_s * self%impedance%calc_impedance(state%ice_content)

    end function calc_kflh_impedance

    module function calc_kflh_viscosity(self, state) result(kflh)
        implicit none
        class(type_hcf_viscosity), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64) :: kflh

        kflh = self%k_s * self%viscosity%calc_viscosity(state%temperature)

    end function calc_kflh_viscosity

    module function calc_kflh_base_impedance(self, state) result(kflh)
        implicit none
        class(type_hcf_base_impedance), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64) :: kflh

        kflh = self%k_s * self%base%calc_kr(state%pressure) & !&
                        * self%impedance%calc_impedance(state%ice_content)

    end function calc_kflh_base_impedance

    module function calc_kflh_base_viscosity(self, state) result(kflh)
        implicit none
        class(type_hcf_base_viscosity), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64) :: kflh

        kflh = self%k_s * self%base%calc_kr(state%pressure) & !&
                        * self%viscosity%calc_viscosity(state%temperature)

    end function calc_kflh_base_viscosity

    module function calc_kflh_impedance_viscosity(self, state) result(kflh)
        implicit none
        class(type_hcf_impedance_viscosity), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64) :: kflh

        kflh = self%k_s * self%impedance%calc_impedance(state%ice_content) & !&
                        * self%viscosity%calc_viscosity(state%temperature)

    end function calc_kflh_impedance_viscosity

    module function calc_kflh_base_impedance_viscosity(self, state) result(kflh)
        implicit none
        class(type_hcf_base_impedance_viscosity), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64) :: kflh

        kflh = self%k_s * self%base%calc_kr(state%pressure) & !&
                        * self%impedance%calc_impedance(state%ice_content) & !&
                        * self%viscosity%calc_viscosity(state%temperature)

    end function calc_kflh_base_impedance_viscosity

end submodule calculate_hcf_base
