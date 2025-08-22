submodule(calculate_hcf) calculate_hcf_base
    implicit none
contains

    module subroutine initialize_holder_hcfs(self, input, i_material)
        implicit none
        class(holder_hcfs), intent(inout) :: self
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: i_material

        select case (input%basic%materials(i_material)%hydraulic%model_number)
        case (1)
            self%p = create_type_hcf_base(input, i_material)
        case (2)
            self%p = create_type_hcf_impedance(input, i_material)
        case (3)
            self%p = create_type_hcf_viscosity(input, i_material)
        case (4)
            self%p = create_type_hcf_base_impedance(input, i_material)
        case (5)
            self%p = create_type_hcf_base_viscosity(input, i_material)
        case (6)
            self%p = create_type_hcf_impedance_viscosity(input, i_material)
        case (7)
            self%p = create_type_hcf_base_impedance_viscosity(input, i_material)
        end select

    end subroutine initialize_holder_hcfs

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
