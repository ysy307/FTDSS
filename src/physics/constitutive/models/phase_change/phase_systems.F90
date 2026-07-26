!>
!> @brief Manages phase composition (water, ice, gas, vapor) and their derivatives.
!> Integrates Fusion and Vaporization models to ensure thermodynamic consistency.
!>
module models_phase_change_manager
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: ieee_arithmetic, only:ieee_is_finite
    use :: iapws, only:type_iapws97, type_iapws06
    use :: module_core, only:type_state
    use :: constitutive_constants, only:latent_heat_fusion_water_0C, &
        TtoK => celsius_to_kelvin, Rg => universal_gas_constant, Mw => molar_mass_water, rho_std => reference_water_density
    use :: models_phase_change_gcc, only:abst_gcc
    use :: models_wrf, only:abst_wrf
    use :: models_phase_change_fusion, only:type_fusion
    use :: models_phase_change_vaporization, only:type_evaporation
    implicit none
    private

    public :: type_phase_manager
    public :: phase_return_relaxation

    ! Block relaxation of the eliminated local ice return map. The same
    ! factor multiplies its algorithmic tangent and its committed increment,
    ! preserving the Modified Picard linearization.
    real(real64), parameter :: phase_return_relaxation = 0.3d0

    !>
    !> @brief Phase Manager Class
    !>
    type :: type_phase_manager
        private
        type(type_fusion) :: fusion
        type(type_evaporation) :: evap
    contains
        procedure, public :: initialize
        procedure, public :: update_water_phases
        procedure, public :: project_ice_content
        procedure, public :: calc_cryo_head_dT
        procedure, public :: calc_conserved_target
        procedure, public :: solve_local_conserved_equilibrium
        procedure, public :: calc_latent_heat_fusion
        procedure, public :: calc_latent_heat_vaporization
        procedure, public :: calc_saturation_pressure
        procedure, public :: deriv_pressure_ice_water
        ! procedure, public :: update_phases_array ! Implement as needed
    end type type_phase_manager

contains

    subroutine initialize(self, gcc, wrf, water, ice)
        implicit none
        class(type_phase_manager), intent(inout) :: self
        class(abst_gcc), intent(in), target :: gcc
        class(abst_wrf), intent(in), target :: wrf
        type(type_iapws97), intent(in), target :: water
        type(type_iapws06), intent(in), target :: ice

        call self%fusion%initialize(wrf, gcc, water, ice)
        call self%evap%initialize(water)

    end subroutine initialize

    !>
    !> @brief Update all phase quantities and their derivatives from P and T.
    !>
    subroutine update_water_phases(self, state)
        implicit none
        class(type_phase_manager), intent(in) :: self
        type(type_state), intent(inout) :: state

        real(real64) :: water_content, ice_content, air_content, vapor_content, porosity
        real(real64) :: relative_humidity
        real(real64) :: temperature, pressure, temperature_K, exponent
        logical :: temperature_set, pressure_set, ice_content_set

        ! Local variables for derivatives
        real(real64) :: dQw_dP, dQw_dT
        real(real64) :: dQi_dP, dQi_dT
        real(real64) :: dQa_dP, dQa_dT
        real(real64) :: dQv_dP, dQv_dT
        real(real64) :: psi_eff
        real(real64) :: projected_ice, ice_increment, equilibrium_error
        integer(int32) :: phase_active_bound

        ! 0. The liquid retention and permeability argument is the capillary
        !    suction of the actual pore-water pressure.
        psi_eff = 0.0d0
        call self%fusion%calc_effective_suction(state, psi_eff)
        call state%effective_suction%set(psi_eff)

        ! 1. Get porosity
        call state%porosity%get(porosity)
        porosity = min(max(porosity, 0.0d0), 1.0d0)

        ! 2. Ice is a local constitutive state, not a third global DOF. Its
        !    value at the current Picard iterate remains in the conservative
        !    mass and enthalpy residuals, while the tangent of the bounded
        !    Clapeyron return map enters the T-p iteration matrix. This is the
        !    modified-Picard split of Hansson et al. (2004), Eq. (19)-(20):
        !    theta_i^k is retained in the history defect and
        !    d(theta_i)/d(T,p)^k multiplies the new primary-variable increment.
        call state%ice_content%get(ice_content, ice_content_set)
        if (.not. ice_content_set) ice_content = 0.0d0
        projected_ice = ice_content
        ice_increment = 0.0d0
        equilibrium_error = 0.0d0
        phase_active_bound = 0
        call self%fusion%project_ice_content(state, projected_ice, ice_increment, equilibrium_error, &
                                              phase_active_bound, dQi_dT, dQi_dP)
        dQi_dT = phase_return_relaxation * dQi_dT
        dQi_dP = phase_return_relaxation * dQi_dP

        ! 3. Liquid water follows the retention curve at the solved pore-water
        !    pressure. The Clapeyron-equivalent content is used only by the
        !    phase projection; replacing the actual pressure here would make
        !    liquid loss cancel ice gain locally and suppress cryosuction.
        call self%fusion%calc_water_content(state, water_content)
        call self%fusion%calc_water_content_derivatives(state, dQw_dP, dQw_dT)
        water_content = max(0.0d0, water_content)

        ! Phase-volume bounds. Keep the one-sided thermodynamic derivatives
        ! while a phase is active; tapering dQi/dT, dQi/dP to zero right at
        ! theta_i = 0 would remove the apparent heat capacity exactly at the
        ! freezing front and make the Picard map jump across it.
        if (ice_content < 0.0d0) then
            ice_content = 0.0d0
            dQi_dP = 0.0d0
            dQi_dT = 0.0d0
        end if
        if (ice_content > porosity) then
            ice_content = porosity
            dQi_dP = 0.0d0
            dQi_dT = 0.0d0
        end if

        if (water_content + ice_content > porosity) then
            water_content = max(0.0d0, porosity - ice_content)
            dQw_dP = -dQi_dP
            dQw_dT = -dQi_dT
        end if

        air_content = max(0.0d0, porosity - water_content - ice_content)
        dQa_dP = -(dQw_dP + dQi_dP)
        dQa_dT = -(dQw_dT + dQi_dT)

        ! 4. Set values (consistency ensured)
        call state%ice_content%set(ice_content)
        call state%dQi_dP%set(dQi_dP)
        call state%dQi_dT%set(dQi_dT)

        call state%water_content%set(water_content)
        call state%dQw_dP%set(dQw_dP)
        call state%dQw_dT%set(dQw_dT)

        call state%air_content%set(air_content)
        call state%dQa_dP%set(dQa_dP)
        call state%dQa_dT%set(dQa_dT)

        ! 5. Update relative humidity and vapor content (theta_v)
        call state%temperature%get(temperature, temperature_set)
        call state%pressure%get(pressure, pressure_set)
        if (.not. temperature_set) then
            write (*, '(A,L1,A,L1)') 'Error: phase state unset before RH. T_set=', temperature_set, ', P_set=', pressure_set
            error stop 'update_water_phases: temperature unset before RH.'
        end if
        if (.not. pressure_set) then
            pressure = 0.0d0
        end if

        if (.not. ieee_is_finite(temperature) .or. .not. ieee_is_finite(pressure)) then
            write (*, '(A,2(1X,ES13.5))') 'Error: phase state non-finite before RH T/P =', temperature, pressure
            error stop 'update_water_phases: non-finite T/P before RH.'
        end if

        temperature_K = temperature + TtoK
        if (.not. ieee_is_finite(temperature_K) .or. temperature_K <= tiny(1.0d0)) then
            write (*, '(A,2(1X,ES13.5))') 'Error: phase invalid absolute temperature T/Tk =', temperature, temperature_K
            error stop 'update_water_phases: invalid absolute temperature before RH.'
        end if

        exponent = (pressure * Mw)/(rho_std * Rg * temperature_K)
        if (.not. ieee_is_finite(exponent)) then
            write (*, '(A,3(1X,ES13.5))') 'Error: phase RH exponent non-finite P/Tk/exp =', pressure, temperature_K, exponent
            error stop 'update_water_phases: invalid RH exponent before RH.'
        end if

        if (abs(exponent) > 700.0d0) then
            write (*, '(A,3(1X,ES13.5))') 'Error: phase RH exponent out-of-range P/Tk/exp =', pressure, temperature_K, exponent
            error stop 'update_water_phases: RH exponent outside exp-safe range.'
        end if

        call self%evap%calc_relative_humidity(state, relative_humidity)
        call state%relative_humidity%set(relative_humidity)

        !    Note: If the model depends on gas-phase volume, the logic for
        !    vapor=0 when air_content=0 should be handled inside evap,
        !    but here we call it independently.
        call self%evap%calc_vapor_content_with_derivatives(state, vapor_content, dQv_dP, dQv_dT)

        ! Guard: if air_content is zero, vapor cannot physically exist
        if (air_content <= epsilon(0.0d0)) then
            vapor_content = 0.0d0
            dQv_dP = 0.0d0
            dQv_dT = 0.0d0
        end if

        if (vapor_content < 0.0d0) then
            vapor_content = 0.0d0
            dQv_dP = 0.0d0
            dQv_dT = 0.0d0
        end if

        call state%vapor_content%set(vapor_content)
        call state%dQv_dP%set(dQv_dP)
        call state%dQv_dT%set(dQv_dT)

    end subroutine update_water_phases

    subroutine calc_cryo_head_dT(self, state, dh_dT)
        implicit none
        class(type_phase_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: dh_dT

        call self%fusion%calc_cryo_head_dT(state, dh_dT)
    end subroutine calc_cryo_head_dT

    subroutine project_ice_content(self, state, projected_ice, ice_increment, equilibrium_error, active_bound, &
                                    dice_dT, dice_dP)
        implicit none
        class(type_phase_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: projected_ice
        real(real64), intent(inout) :: ice_increment
        real(real64), intent(inout) :: equilibrium_error
        integer(int32), intent(inout), optional :: active_bound
        real(real64), intent(inout), optional :: dice_dT
        real(real64), intent(inout), optional :: dice_dP

        call self%fusion%project_ice_content(state, projected_ice, ice_increment, equilibrium_error, active_bound, &
                                              dice_dT, dice_dP)
    end subroutine project_ice_content

    subroutine calc_conserved_target(self, state, target_total_water, available)
        implicit none
        class(type_phase_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: target_total_water
        logical, intent(inout) :: available

        call self%fusion%calc_conserved_target(state, target_total_water, available)
    end subroutine calc_conserved_target

    subroutine solve_local_conserved_equilibrium(self, state, target_total_water, new_pressure, new_ice, converged)
        implicit none
        class(type_phase_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(in) :: target_total_water
        real(real64), intent(inout) :: new_pressure
        real(real64), intent(inout) :: new_ice
        logical, intent(inout) :: converged

        call self%fusion%solve_local_conserved_equilibrium(state, target_total_water, new_pressure, new_ice, converged)
    end subroutine solve_local_conserved_equilibrium

    subroutine calc_latent_heat_fusion(self, state, Lf)
        implicit none
        class(type_phase_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: Lf

        Lf = latent_heat_fusion_water_0C

    end subroutine calc_latent_heat_fusion

    subroutine calc_latent_heat_vaporization(self, state, Lv)
        implicit none
        class(type_phase_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: Lv

        real(real64) :: temperature

        call state%temperature%get(temperature)
        call self%evap%calc_latent_heat_vaporization(temperature, Lv)

    end subroutine calc_latent_heat_vaporization

    subroutine calc_saturation_pressure(self, state, saturation_pressure, is_saturated)
        implicit none
        class(type_phase_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: saturation_pressure
        logical, intent(inout) :: is_saturated

        call self%fusion%calc_saturation_pressure(state, saturation_pressure, is_saturated)
    end subroutine calc_saturation_pressure

    subroutine deriv_pressure_ice_water(self, state, deriv)
        implicit none
        class(type_phase_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: deriv

        call self%fusion%deriv_pressure_ice_water(state, deriv)

    end subroutine deriv_pressure_ice_water

    !> C1 cubic Hermite weight: 1 when x<=0, smoothly decays to 0 at x=delta.
    pure function smooth_weight(x, delta) result(w)
        implicit none
        real(real64), intent(in) :: x, delta
        real(real64) :: w, t

        if (x <= 0.0d0) then
            w = 1.0d0
        else if (x >= delta) then
            w = 0.0d0
        else
            t = x / delta
            w = 1.0d0 - t * t * (3.0d0 - 2.0d0 * t)
        end if
    end function smooth_weight

end module models_phase_change_manager
