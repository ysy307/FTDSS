module module_physics_models
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: iapws, only:type_iapws97, type_iapws06
    use :: module_core, only:type_state
    use :: physics_models_wrf
    use :: physics_models_hcf
    use :: physics_models_phase_change_liquid_solid_gcc
    use :: physics_models_phase_change_liquid_solid_fusion
    use :: physics_models_phase_change_liquid_vapor_vaporization
    use :: physics_models_phase_systems
    implicit none
    private

    public :: type_models_manager
    public :: type_wrf_params
    public :: type_hcf_params

    type :: type_models_manager
        private
        type(holder_wrfs) :: wrf
        type(holder_hcfs) :: hcf
        type(holder_gccs) :: gcc
        type(type_phase_manager) :: phase_manager
    contains
        procedure, public :: initialize
        procedure, public :: update_water_phases
        procedure, public :: calc_Kflh
        procedure, public :: calc_KlT
        procedure, public :: calc_Kvh
        procedure, public :: calc_KvT
        procedure, public :: calc_latent_heat_fusion
        procedure, public :: calc_latent_heat_vaporization
        procedure, public :: calc_pressure_ice_water_derivative
    end type type_models_manager

contains

    subroutine initialize(self, material_id, wrf_id, wrf_params, hcf_id, hcf_params, gcc_id, water, ice)
        implicit none
        class(type_models_manager), intent(inout) :: self
        integer(int32), intent(in) :: material_id
        integer(int32), intent(in), optional :: wrf_id
        type(type_wrf_params), intent(in), optional :: wrf_params
        integer(int32), intent(in), optional :: hcf_id
        type(type_hcf_params), intent(in), optional :: hcf_params
        integer(int32), intent(in), optional :: gcc_id
        type(type_iapws97), intent(in), optional, target :: water
        type(type_iapws06), intent(in), optional, target :: ice

        if (present(wrf_id) .and. present(wrf_params)) then
            call self%wrf%initialize(wrf_id, wrf_params)
        end if
        if (present(hcf_id) .and. present(hcf_params) .and. present(water)) then
            call self%hcf%initialize(hcf_id, hcf_params, water, ice)
        end if
        if (present(gcc_id) .and. present(water) .and. present(ice)) then
            call self%gcc%initialize(material_id, gcc_id, water, ice)
        end if

        if (allocated(self%wrf%p) .and. allocated(self%gcc%p)) then
            if (self%wrf%p%is_initialized() .and. self%gcc%p%is_initialized()) then
                call self%phase_manager%initialize(self%gcc%p, self%wrf%p, water, ice)
            end if
        end if
    end subroutine initialize

    pure elemental subroutine update_water_phases(self, state)
        implicit none
        class(type_models_manager), intent(inout) :: self
        type(type_state), intent(inout) :: state

        call self%phase_manager%update_water_phases(state)
    end subroutine update_water_phases

    pure elemental subroutine calc_Kflh(self, state, Kflh)
        implicit none
        class(type_models_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: Kflh

        call self%hcf%p%calc_Kflh(state, Kflh)
    end subroutine calc_Kflh

    pure elemental subroutine calc_KlT(self, state, KlT)
        implicit none
        class(type_models_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: KlT

        call self%hcf%p%calc_KlT(state, KlT)
    end subroutine calc_KlT

    pure elemental subroutine calc_Kvh(self, state, Kvh)
        implicit none
        class(type_models_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: Kvh

        call self%hcf%p%calc_Kvh(state, Kvh)
    end subroutine calc_Kvh

    pure elemental subroutine calc_KvT(self, state, KvT)
        implicit none
        class(type_models_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: KvT

        call self%hcf%p%calc_KvT(state, KvT)
    end subroutine calc_KvT

    pure elemental subroutine calc_latent_heat_fusion(self, state, L_fusion)
        implicit none
        class(type_models_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: L_fusion

        call self%phase_manager%calc_latent_heat_fusion(state, L_fusion)
    end subroutine calc_latent_heat_fusion

    pure elemental subroutine calc_latent_heat_vaporization(self, state, L_vaporization)
        implicit none
        class(type_models_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: L_vaporization

        call self%phase_manager%calc_latent_heat_vaporization(state, L_vaporization)
    end subroutine calc_latent_heat_vaporization

    pure elemental subroutine calc_pressure_ice_water_derivative(self, state, deriv)
        implicit none
        class(type_models_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: deriv

        call self%phase_manager%deriv_pressure_ice_water(state, deriv)
    end subroutine calc_pressure_ice_water_derivative

end module module_physics_models
