submodule(calculate_hcf) calculate_hcf_base
    implicit none
contains

    module subroutine initialize_holder_hcfs(self, input, material_id)
        implicit none
        class(holder_hcfs), intent(inout) :: self
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: material_id

        select case (input%basic%materials(material_id)%hydraulic%model_number)
        case (1)
            self%p = create_type_hcf_base(input, material_id)
        case (2)
            self%p = create_type_hcf_impedance(input, material_id)
        case (3)
            self%p = create_type_hcf_viscosity(input, material_id)
        case (4)
            self%p = create_type_hcf_base_impedance(input, material_id)
        case (5)
            self%p = create_type_hcf_base_viscosity(input, material_id)
        case (6)
            self%p = create_type_hcf_impedance_viscosity(input, material_id)
        case (7)
            self%p = create_type_hcf_base_impedance_viscosity(input, material_id)
        end select

    end subroutine initialize_holder_hcfs

    module pure elemental function calc_kflh_base(self, state) result(kflh)
        implicit none
        class(type_hcf_base), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64) :: kflh

        kflh = self%k_s * self%base%calc_kr(state%pressure)

    end function calc_kflh_base

    module pure elemental function calc_kflh_impedance(self, state) result(kflh)
        implicit none
        class(type_hcf_impedance), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64) :: kflh

        kflh = self%k_s * self%impedance%calc_impedance(state%ice_content)

    end function calc_kflh_impedance

    module pure elemental function calc_kflh_viscosity(self, state) result(kflh)
        implicit none
        class(type_hcf_viscosity), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64) :: kflh

        kflh = self%k_s * self%viscosity%calc_viscosity(state%temperature)

    end function calc_kflh_viscosity

    module pure elemental function calc_kflh_base_impedance(self, state) result(kflh)
        implicit none
        class(type_hcf_base_impedance), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64) :: kflh

        kflh = self%k_s * self%base%calc_kr(state%pressure) & !&
                        * self%impedance%calc_impedance(state%ice_content)

    end function calc_kflh_base_impedance

    module pure elemental function calc_kflh_base_viscosity(self, state) result(kflh)
        implicit none
        class(type_hcf_base_viscosity), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64) :: kflh

        kflh = self%k_s * self%base%calc_kr(state%pressure) & !&
                        * self%viscosity%calc_viscosity(state%temperature)

    end function calc_kflh_base_viscosity

    module pure elemental function calc_kflh_impedance_viscosity(self, state) result(kflh)
        implicit none
        class(type_hcf_impedance_viscosity), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64) :: kflh

        kflh = self%k_s * self%impedance%calc_impedance(state%ice_content) & !&
                        * self%viscosity%calc_viscosity(state%temperature)

    end function calc_kflh_impedance_viscosity

    module pure elemental function calc_kflh_base_impedance_viscosity(self, state) result(kflh)
        implicit none
        class(type_hcf_base_impedance_viscosity), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64) :: kflh

        kflh = self%k_s * self%base%calc_kr(state%pressure) & !&
                        * self%impedance%calc_impedance(state%ice_content) & !&
                        * self%viscosity%calc_viscosity(state%temperature)

    end function calc_kflh_base_impedance_viscosity

end submodule calculate_hcf_base
