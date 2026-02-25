module module_physics_materials
    use :: iso_fortran_env, only:int32, real64
    use :: iapws, only:type_iapws97, type_iapws06
    use :: module_core, only:type_physics_info, type_state
    use :: physics_materials_base
    use :: physics_materials_density
    use :: physics_materials_specific_heat
    use :: physics_materials_thermal_conductivity
    use :: physics_materials_heat_capacity
    implicit none
    private

    public :: type_material_manager
    public :: type_thc_dispersivity

    type :: type_material_manager
        private
        type(holder_dens) :: den
        type(holder_sphs) :: sph
        type(holder_vhcs) :: vhc
        type(holder_thcs) :: thc
    contains
        procedure, public :: initialize
        procedure, public :: calc_density
        procedure, public :: get_density_solid
        procedure, public :: get_specific_heat_solid
        procedure, public :: calc_density_water_derivatives
        procedure, public :: calc_density_ice_derivatives
        procedure, public :: calc_density_vapor_derivatives
        procedure, public :: calc_specific_heat
        procedure, private :: calc_thermal_conductivity_nondispersivity
        procedure, private :: calc_thermal_conductivity_dispersivity
        generic, public :: calc_thermal_conductivity => calc_thermal_conductivity_nondispersivity, &
            calc_thermal_conductivity_dispersivity
        procedure, public :: calc_vol_heat_capacity
    end type type_material_manager

contains

    subroutine initialize(self, material_id, den_info, sph_info, vhc_info, thc_info, water, ice)
        implicit none
        class(type_material_manager), intent(inout) :: self
        integer(int32), intent(in) :: material_id
        type(type_physics_info), intent(in), optional :: den_info
        type(type_physics_info), intent(in), optional :: sph_info
        type(type_physics_info), intent(in), optional :: vhc_info
        type(type_physics_info), intent(in), optional :: thc_info

        type(type_iapws97), intent(in), optional, target :: water
        type(type_iapws06), intent(in), optional, target :: ice

        logical :: is_present_iapws = .false.

        if (.not. present(water) .or. .not. present(ice)) then
            is_present_iapws = .false.
        else
            is_present_iapws = .true.
        end if

        if (present(den_info)) then
            if (.not. is_present_iapws) then
                error stop "Error in material_manager%initialize: water and ice must be provided when den_info is provided."
            end if
            call self%den%initialize(material_id, den_info, water, ice)
        end if
        if (present(sph_info)) then
            if (.not. is_present_iapws) then
                error stop "Error in material_manager%initialize: water and ice must be provided when sph_info is provided."
            end if
            call self%sph%initialize(material_id, sph_info, water, ice)
        end if
        if (present(vhc_info)) then
            if (.not. is_present_iapws) then
                error stop "Error in material_manager%initialize: water and ice must be provided when vhc_info is provided."
            end if
            call self%vhc%initialize(material_id, vhc_info, water, ice)
        end if
        if (present(thc_info)) then
            if (.not. is_present_iapws) then
                error stop "Error in material_manager%initialize: water and ice must be provided when thc_info is provided."
            end if
            call self%thc%initialize(material_id, thc_info, water, ice)
        end if
    end subroutine initialize

    subroutine calc_density(self, state, density)
        implicit none
        class(type_material_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: density

        call self%den%p%calc(state, density)

    end subroutine calc_density

    subroutine get_density_solid(self, density_solid)
        implicit none
        class(type_material_manager), intent(in) :: self
        real(real64), intent(inout) :: density_solid

        call self%den%p%get_solid(density_solid)

    end subroutine get_density_solid

    subroutine get_specific_heat_solid(self, cp)
        implicit none
        class(type_material_manager), intent(in) :: self
        real(real64), intent(inout) :: cp

        call self%sph%p%get_solid(cp)

    end subroutine get_specific_heat_solid

    subroutine calc_density_water_derivatives(self, state, dden_dT, dden_dP)
        implicit none
        class(type_material_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout), optional :: dden_dT
        real(real64), intent(inout), optional :: dden_dP

        if (present(dden_dT)) then
            call self%den%p%calc_drho_water_dT(state, dden_dT)
        end if
        if (present(dden_dP)) then
            call self%den%p%calc_drho_water_dP(state, dden_dP)
        end if

    end subroutine calc_density_water_derivatives

    subroutine calc_density_ice_derivatives(self, state, dden_dT, dden_dP)
        implicit none
        class(type_material_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout), optional :: dden_dT
        real(real64), intent(inout), optional :: dden_dP

        if (present(dden_dT)) then
            call self%den%p%calc_drho_ice_dT(state, dden_dT)
        end if
        if (present(dden_dP)) then
            call self%den%p%calc_drho_ice_dP(state, dden_dP)
        end if

    end subroutine calc_density_ice_derivatives

    subroutine calc_density_vapor_derivatives(self, state, dden_dT, dden_dP)
        implicit none
        class(type_material_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout), optional :: dden_dT
        real(real64), intent(inout), optional :: dden_dP

        if (present(dden_dT)) then
            call self%den%p%calc_drho_vapor_dT(state, dden_dT)
        end if
        if (present(dden_dP)) then
            call self%den%p%calc_drho_vapor_dP(state, dden_dP)
        end if

    end subroutine calc_density_vapor_derivatives

    subroutine calc_specific_heat(self, state, specific_heat)
        implicit none
        class(type_material_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: specific_heat

        call self%sph%p%calc(state, specific_heat)

    end subroutine calc_specific_heat

    subroutine calc_thermal_conductivity_nondispersivity(self, state, lambda)
        implicit none
        class(type_material_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: lambda

        call self%thc%p%calc(state, lambda)

    end subroutine calc_thermal_conductivity_nondispersivity

    subroutine calc_thermal_conductivity_dispersivity(self, state, lambda)
        implicit none
        class(type_material_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        type(type_thc_dispersivity), intent(inout) :: lambda

        call self%thc%p%calc(state, lambda)

    end subroutine calc_thermal_conductivity_dispersivity

    subroutine calc_vol_heat_capacity(self, state, vhc)
        implicit none
        class(type_material_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: vhc

        call self%vhc%p%calc(state, vhc)

    end subroutine calc_vol_heat_capacity
end module module_physics_materials
