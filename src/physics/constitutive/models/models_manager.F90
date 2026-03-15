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
        procedure, public :: calc_cryogenic_suction
        procedure, public :: calc_cryogenic_suction_derivatives
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

        real(real64) :: pressure, psi_cap, psi_cryo
        type(type_state) :: local_state

        ! When cryogenic suction (psi_cryo) exceeds the capillary suction (psi_cap),
        ! the soil is in the frozen regime.  In that case the liquid-water pressure is
        ! effectively -psi_cryo (Clausius-Clapeyron), which strongly reduces the
        ! relative permeability.  Use this effective pressure for the HCF evaluation
        ! so that the frozen zone becomes a hydraulic barrier and drives flow toward
        ! the freezing front.
        call state%pressure%get(pressure)
        psi_cap = max(0.0d0, -pressure)
        call self%gcc%calc(state, psi_cryo)

        if (psi_cryo > psi_cap) then
            call local_state%copy(state)
            call local_state%pressure%set(-psi_cryo)
            call self%hcf%p%calc_Kflh(local_state, Kflh)
        else
            call self%hcf%p%calc_Kflh(state, Kflh)
        end if
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

    subroutine calc_cryogenic_suction(self, state, suction)
        implicit none
        class(type_models_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: suction

        call self%gcc%calc(state, suction)
    end subroutine calc_cryogenic_suction

    subroutine calc_cryogenic_suction_derivatives(self, state, deriv_dP, deriv_dT)
        implicit none
        class(type_models_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout), optional :: deriv_dP
        real(real64), intent(inout), optional :: deriv_dT

        if (present(deriv_dP)) then
            call self%gcc%deriv_pressure(state, deriv_dP)
        end if
        if (present(deriv_dT)) then
            call self%gcc%deriv_temperature(state, deriv_dT)
        end if
    end subroutine calc_cryogenic_suction_derivatives

end module constitutive_models_manager
