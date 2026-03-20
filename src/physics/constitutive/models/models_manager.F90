module constitutive_models_manager
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: iapws, only:type_iapws97, type_iapws06
    use :: module_core
    use :: models_wrf
    use :: models_hcf
    use :: models_phase_change_gcc
    use :: models_phase_change_fusion
    use :: models_phase_change_vaporization
    use :: models_phase_change_manager
    implicit none
    private

    public :: type_models_manager

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
        procedure, public :: calc_cryo_suction_deriv_T
    end type type_models_manager

contains

    subroutine initialize(self, material_id, config_wrf, config_hcf, config_gcc, water, ice)
        implicit none
        class(type_models_manager), intent(inout) :: self
        integer(int32), intent(in) :: material_id
        type(type_config_wrf), intent(in), optional :: config_wrf
        type(type_config_hcf), intent(in), optional :: config_hcf
        type(type_config_gcc), intent(in), optional :: config_gcc
        type(type_iapws97), intent(in), optional, target :: water
        type(type_iapws06), intent(in), optional, target :: ice

        if (present(config_wrf)) then
            call self%wrf%initialize(config_wrf)
        end if
        if (present(config_hcf) .and. present(water)) then
            call self%hcf%initialize(config_hcf, water, ice)
        end if
        if (present(config_gcc) .and. present(water) .and. present(ice)) then
            call self%gcc%initialize(material_id, config_gcc, water, ice)
        end if

        if (allocated(self%wrf%p) .and. allocated(self%gcc%p)) then
            if (self%wrf%p%is_initialized() .and. self%gcc%p%is_initialized()) then
                call self%phase_manager%initialize(self%gcc%p, self%wrf%p, water, ice)
            end if
        end if
    end subroutine initialize

    subroutine update_water_phases(self, state)
        implicit none
        class(type_models_manager), intent(in) :: self
        type(type_state), intent(inout) :: state

        call self%phase_manager%update_water_phases(state)
    end subroutine update_water_phases

    subroutine calc_Kflh(self, state, Kflh)
        implicit none
        class(type_models_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: Kflh

        call self%hcf%p%calc_Kflh(state, Kflh)
    end subroutine calc_Kflh

    subroutine calc_KlT(self, state, KlT)
        implicit none
        class(type_models_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: KlT

        call self%hcf%p%calc_KlT(state, KlT)
    end subroutine calc_KlT

    subroutine calc_Kvh(self, state, Kvh)
        implicit none
        class(type_models_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: Kvh

        call self%hcf%p%calc_Kvh(state, Kvh)
    end subroutine calc_Kvh

    subroutine calc_KvT(self, state, KvT)
        implicit none
        class(type_models_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: KvT

        call self%hcf%p%calc_KvT(state, KvT)
    end subroutine calc_KvT

    subroutine calc_latent_heat_fusion(self, state, L_fusion)
        implicit none
        class(type_models_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: L_fusion

        call self%phase_manager%calc_latent_heat_fusion(state, L_fusion)
    end subroutine calc_latent_heat_fusion

    subroutine calc_latent_heat_vaporization(self, state, L_vaporization)
        implicit none
        class(type_models_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: L_vaporization

        call self%phase_manager%calc_latent_heat_vaporization(state, L_vaporization)
    end subroutine calc_latent_heat_vaporization

    subroutine calc_pressure_ice_water_derivative(self, state, deriv)
        implicit none
        class(type_models_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: deriv

        call self%phase_manager%deriv_pressure_ice_water(state, deriv)
    end subroutine calc_pressure_ice_water_derivative

    subroutine calc_cryo_suction_deriv_T(self, state, deriv)
        implicit none
        class(type_models_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: deriv

        call self%gcc%deriv_temperature(state, deriv)
    end subroutine calc_cryo_suction_deriv_T

end module constitutive_models_manager
