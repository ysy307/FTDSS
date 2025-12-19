module physics_materials_base
    use, intrinsic :: iso_fortran_env
    use :: iapws, only:type_iapws97, type_iapws06
    use :: module_core, only:type_state, type_physics_info
    use :: physics_constants, only:TtoK => celsius_to_kelvin, P_atm => standard_atmospheric_pressure
    implicit none
    private

    public :: abst_material

    type, abstract :: abst_material
        integer(int32) :: material_id = -1
        real(real64) :: material1 = 0.0d0 !! like a soil or a rock, a concrete
        real(real64) :: material2 = 0.0d0 !! like a water
        real(real64) :: material3 = 0.0d0 !! like a ice
        real(real64) :: material4 = 0.0d0 !! like a gas
        type(type_iapws97), pointer :: water => null()
        type(type_iapws06), pointer :: ice => null()
    contains
        procedure, pass(self), public :: initialize => initialize_abst_material
        procedure, pass(self), public :: get_phi => get_material_phi
        procedure, pass(self), public :: calc_water_density => calc_water_density_abst_material
        procedure, pass(self), public :: calc_ice_density => calc_ice_density_abst_material
        procedure, pass(self), public :: calc_vapor_density => calc_vapor_density_abst_material
        procedure, pass(self), public :: calc_water_cp => calc_water_cp_abst_material
        procedure, pass(self), public :: calc_ice_cp => calc_ice_cp_abst_material
        procedure, pass(self), public :: calc_vapor_cp => calc_vapor_cp_abst_material
        procedure, pass(self), public :: shift_temperature_absolute => shift_temperature_absolute_abst_material
        procedure, pass(self), public :: shift_pressure_absolute => shift_pressure_absolute_abst_material
    end type abst_material

contains

    subroutine initialize_abst_material(self, material_id, physics_info, water, ice)
        implicit none
        class(abst_material), intent(inout) :: self
        integer(int32), intent(in) :: material_id
        type(type_physics_info), intent(in) :: physics_info
        type(type_iapws97), intent(in), target :: water
        type(type_iapws06), intent(in), target :: ice

        self%material_id = material_id

        self%material1 = physics_info%solid
        self%material2 = physics_info%water
        self%material3 = physics_info%ice
        self%material4 = physics_info%vapor

        self%water => water
        self%ice => ice

    end subroutine initialize_abst_material

    pure elemental subroutine get_material_phi(self, state, phi1, phi2, phi3, phi4)
        implicit none
        class(abst_material), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: phi1
        real(real64), intent(inout) :: phi2
        real(real64), intent(inout), optional :: phi3
        real(real64), intent(inout), optional :: phi4

        phi1 = 1.0d0 - state%porosity
        phi2 = state%water_content
        if (present(phi3)) phi3 = state%ice_content
        if (present(phi4)) phi4 = 1.0d0 - phi1 - phi2 - phi3

    end subroutine get_material_phi

    pure elemental subroutine calc_water_density_abst_material(self, state, density)
        implicit none
        class(abst_material), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: density

        real(real64) :: temperature_K
        real(real64) :: pressure_absolute

        call self%shift_temperature_absolute(state%temperature, temperature_K)
        call self%shift_pressure_absolute(state%pressure, pressure_absolute)

        if (associated(self%water)) then
            call self%water%calc_rho(temperature_K, pressure_absolute, density)
        else
            density = self%material2
        end if

    end subroutine calc_water_density_abst_material

    pure elemental subroutine calc_ice_density_abst_material(self, state, density)
        implicit none
        class(abst_material), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: density

        real(real64) :: temperature_K
        real(real64) :: pressure_absolute

        call self%shift_temperature_absolute(state%temperature, temperature_K)
        call self%shift_pressure_absolute(state%pressure, pressure_absolute)

        if (associated(self%ice)) then
            call self%ice%calc_rho(temperature_K, pressure_absolute, density)
        else
            density = self%material3
        end if

    end subroutine calc_ice_density_abst_material

    pure elemental subroutine calc_vapor_density_abst_material(self, state, density)
        implicit none
        class(abst_material), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: density

        real(real64) :: temperature_K

        call self%shift_temperature_absolute(state%temperature, temperature_K)

        if (associated(self%water)) then
            call self%water%calc_saturation_density(temperature_K, density)
            density = max(density * state%relative_humidity, 1.0d-8)
        else
            density = self%material4
        end if

    end subroutine calc_vapor_density_abst_material

    pure elemental subroutine calc_water_cp_abst_material(self, state, cp)
        implicit none
        class(abst_material), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: cp

        real(real64) :: temperature_K
        real(real64) :: pressure_absolute

        call self%shift_temperature_absolute(state%temperature, temperature_K)
        call self%shift_pressure_absolute(state%pressure, pressure_absolute)

        if (associated(self%water)) then
            call self%water%calc_cp(temperature_K, pressure_absolute, cp)
        else
            cp = self%material2
        end if

    end subroutine calc_water_cp_abst_material

    pure elemental subroutine calc_ice_cp_abst_material(self, state, cp)
        implicit none
        class(abst_material), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: cp

        real(real64) :: temperature_K
        real(real64) :: pressure_absolute

        call self%shift_temperature_absolute(state%temperature, temperature_K)
        call self%shift_pressure_absolute(state%pressure, pressure_absolute)

        if (associated(self%ice)) then
            call self%ice%calc_cp(temperature_K, pressure_absolute, cp)
        else
            cp = self%material3
        end if

    end subroutine calc_ice_cp_abst_material

    pure elemental subroutine calc_vapor_cp_abst_material(self, state, cp)
        implicit none
        class(abst_material), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: cp

        real(real64) :: temperature_K

        call self%shift_temperature_absolute(state%temperature, temperature_K)

        if (associated(self%water)) then
            call self%water%calc_saturation_cp(temperature_K, cp)
        else
            cp = self%material4
        end if

    end subroutine calc_vapor_cp_abst_material

    pure elemental subroutine shift_temperature_absolute_abst_material(self, temperature_degree, temperature_K)
        implicit none
        class(abst_material), intent(in) :: self
        real(real64), intent(in) :: temperature_degree
        real(real64), intent(inout) :: temperature_K

        temperature_K = temperature_degree + TtoK
    end subroutine shift_temperature_absolute_abst_material

    pure elemental subroutine shift_pressure_absolute_abst_material(self, pressure_gauge, pressure_absolute)
        implicit none
        class(abst_material), intent(in) :: self
        real(real64), intent(in) :: pressure_gauge
        real(real64), intent(inout) :: pressure_absolute

        if (pressure_gauge < 0.0d0) then
            pressure_absolute = P_atm
        else
            pressure_absolute = P_atm + pressure_gauge
        end if
    end subroutine shift_pressure_absolute_abst_material

end module physics_materials_base
