module materials_base
    use, intrinsic :: iso_fortran_env
    use :: iapws, only:type_iapws97, type_iapws06
    use :: module_core, only:type_state, type_config_constitutive
    use :: constitutive_constants, only:TtoK => celsius_to_kelvin, P_atm => standard_atmospheric_pressure
    use :: physics_constitutive_base, only:abst_constitutive
    implicit none
    private

    public :: abst_material

    type, extends(abst_constitutive), abstract :: abst_material
        integer(int32) :: material_id = -1
        real(real64) :: material1 = 0.0d0 !! like a soil or a rock, a concrete
        real(real64) :: material2 = 0.0d0 !! like a water
        real(real64) :: material3 = 0.0d0 !! like a ice
        real(real64) :: material4 = 0.0d0 !! like a gas
    contains
        ! ---- Lifecycle ----
        ! initialize, destroy, reset, etc.
        procedure, pass(self), public :: initialize => initialize_abst_material

        ! ---- Mutator ----
        ! set_XXX, increment_XXX, update_XXX, etc.

        ! ---- Algorithm / Operation ----
        ! compute_XXX, check_XXX, solve_XXX, etc.
        procedure, pass(self), public :: calc_water_density => calc_water_density_abst_material
        procedure, pass(self), public :: calc_ice_density => calc_ice_density_abst_material
        procedure, pass(self), public :: calc_vapor_density => calc_vapor_density_abst_material
        procedure, pass(self), public :: calc_water_cp => calc_water_cp_abst_material
        procedure, pass(self), public :: calc_ice_cp => calc_ice_cp_abst_material
        procedure, pass(self), public :: calc_vapor_cp => calc_vapor_cp_abst_material

        ! ---- Inquiry ----
        ! is_XXX, has_XXX, should_XXX, etc.

        ! ---- Getter ----
        ! get_XXX, etc.
        procedure, pass(self), public :: get_phi => get_material_phi
        procedure, pass(self), public :: get_solid => get_solid_abst_material

        ! ---- Meta / Utility ----
        ! display, to_string, etc.

        ! ---- Operator ----
    end type abst_material

contains

    subroutine initialize_abst_material(self, material_id, constitutive_info, water, ice)
        implicit none
        class(abst_material), intent(inout) :: self
        integer(int32), intent(in) :: material_id
        type(type_config_constitutive), intent(in) :: constitutive_info
        type(type_iapws97), intent(in), target :: water
        type(type_iapws06), intent(in), target :: ice

        self%material_id = material_id

        self%material1 = constitutive_info%solid
        self%material2 = constitutive_info%water
        self%material3 = constitutive_info%ice
        self%material4 = constitutive_info%vapor

        self%water => water
        self%ice => ice

        self%initialized = .true.
    end subroutine initialize_abst_material

    subroutine get_material_phi(self, state, phi1, phi2, phi3, phi4)
        implicit none
        class(abst_material), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: phi1
        real(real64), intent(inout) :: phi2
        real(real64), intent(inout), optional :: phi3
        real(real64), intent(inout), optional :: phi4

        real(real64) :: porosity, water_content, ice_content

        call state%porosity%get(porosity)
        call state%water_content%get(water_content)
        call state%ice_content%get(ice_content)

        phi1 = 1.0d0 - porosity
        phi2 = water_content
        if (present(phi3)) phi3 = ice_content
        if (present(phi4)) phi4 = 1.0d0 - phi1 - phi2 - phi3

    end subroutine get_material_phi

    subroutine get_solid_abst_material(self, solid)
        implicit none
        class(abst_material), intent(in) :: self
        real(real64), intent(inout) :: solid

        solid = self%material1

    end subroutine get_solid_abst_material

    subroutine calc_water_density_abst_material(self, state, density)
        implicit none
        class(abst_material), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: density

        if (associated(self%water)) then
            call self%calc_rho_water(state, density)
        else
            density = self%material2
        end if

    end subroutine calc_water_density_abst_material

    subroutine calc_ice_density_abst_material(self, state, density)
        implicit none
        class(abst_material), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: density

        if (associated(self%ice)) then
            call self%calc_rho_ice(state, density)
        else
            density = self%material3
        end if

    end subroutine calc_ice_density_abst_material

    subroutine calc_vapor_density_abst_material(self, state, density)
        implicit none
        class(abst_material), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: density

        if (associated(self%water)) then
            call self%calc_rho_vapor(state, density)
        else
            density = self%material4
        end if

    end subroutine calc_vapor_density_abst_material

    subroutine calc_water_cp_abst_material(self, state, cp)
        implicit none
        class(abst_material), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: cp

        if (associated(self%water)) then
            call self%calc_cp_water(state, cp)
        else
            cp = self%material2
        end if

    end subroutine calc_water_cp_abst_material

    subroutine calc_ice_cp_abst_material(self, state, cp)
        implicit none
        class(abst_material), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: cp

        if (associated(self%ice)) then
            call self%calc_cp_ice(state, cp)
        else
            cp = self%material3
        end if

    end subroutine calc_ice_cp_abst_material

    subroutine calc_vapor_cp_abst_material(self, state, cp)
        implicit none
        class(abst_material), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: cp

        if (associated(self%water)) then
            call self%calc_cp_vapor(state, cp)
        else
            cp = self%material4
        end if

    end subroutine calc_vapor_cp_abst_material

end module materials_base
