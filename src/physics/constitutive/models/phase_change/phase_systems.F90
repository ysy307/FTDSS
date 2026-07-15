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
        logical :: temperature_set, pressure_set

        ! Temporary raw values
        real(real64) :: raw_ice_content
        real(real64) :: raw_water_content

        ! Local variables for derivatives
        real(real64) :: dQw_dP, dQw_dT
        real(real64) :: dQi_dP, dQi_dT
        real(real64) :: dQa_dP, dQa_dT
        real(real64) :: dQv_dP, dQv_dT
        real(real64) :: psi_eff

        ! 0. Generalized suction of the liquid phase: retention-based
        !    properties (theta_w, relative permeability) are evaluated at it.
        psi_eff = 0.0d0
        call self%fusion%calc_effective_suction(state, psi_eff)
        call state%effective_suction%set(psi_eff)

        ! 1. Get porosity
        call state%porosity%get(porosity)
        porosity = min(max(porosity, 0.0d0), 1.0d0)

        ! 2. Preliminary ice content (theta_i) calculation
        call self%fusion%calc_ice_content(state, raw_ice_content)
        call self%fusion%calc_ice_content_derivatives(state, dQi_dP, dQi_dT)

        ! --- Ice upper-bound check (Ice Cap) ---
        if (raw_ice_content > porosity) then
            ! Ice exceeds porosity: all pore space is ice
            ice_content = porosity

            ! Derivatives are zero since ice is capped at the upper limit
            dQi_dP = 0.0d0
            dQi_dT = 0.0d0

            ! No water or air can exist
            water_content = 0.0d0
            dQw_dP = 0.0d0
            dQw_dT = 0.0d0

            air_content = 0.0d0
            dQa_dP = 0.0d0
            dQa_dT = 0.0d0
        else
            ! Normal case: ice content as computed
            ice_content = raw_ice_content
            ! Use computed derivatives (dQi_dP, dQi_dT) as-is

            ! 3. Preliminary liquid water content (theta_w) calculation
            call self%fusion%calc_water_content(state, raw_water_content)
            call self%fusion%calc_water_content_derivatives(state, dQw_dP, dQw_dT)

            ! --- Water upper-bound check (Water Cap) ---
            if (raw_water_content + ice_content > porosity) then
                ! Oversaturated: water fills remaining pore space after ice
                water_content = max(0.0d0, porosity - ice_content)

                ! Water content depends on (Porosity - Ice), so derivative is opposite sign of ice
                dQw_dP = -1.0d0 * dQi_dP
                dQw_dT = -1.0d0 * dQi_dT

                ! No gas phase
                air_content = 0.0d0
                dQa_dP = 0.0d0
                dQa_dT = 0.0d0
            else
                ! Unsaturated: use computed values
                water_content = raw_water_content
                ! Use computed derivatives (dQw_dP, dQw_dT) as-is

                ! Gas phase = porosity - water - ice
                air_content = porosity - water_content - ice_content

                ! Gas phase derivatives (from conservation)
                dQa_dP = -1.0d0 * (dQw_dP + dQi_dP)
                dQa_dT = -1.0d0 * (dQw_dT + dQi_dT)
            end if
        end if

        ! 4. Physical projection to avoid negative/oversaturated phase fractions.
        ! Keep the thermodynamic one-sided derivatives while a phase is active. The
        ! freezing front needs dQi/dT and dQi/dP immediately after ice appears;
        ! tapering them to zero near Qi=0 removes the latent/apparent capacity and
        ! makes the Picard map jump across the front.
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

        if (water_content < 0.0d0) then
            water_content = 0.0d0
            dQw_dP = 0.0d0
            dQw_dT = 0.0d0
        end if

        if (water_content > porosity - ice_content) then
            water_content = max(0.0d0, porosity - ice_content)
            dQw_dP = -1.0d0 * dQi_dP
            dQw_dT = -1.0d0 * dQi_dT
        end if

        air_content = porosity - water_content - ice_content
        if (air_content < 0.0d0) then
            air_content = 0.0d0
            dQa_dP = 0.0d0
            dQa_dT = 0.0d0
        else
            dQa_dP = -1.0d0 * (dQw_dP + dQi_dP)
            dQa_dT = -1.0d0 * (dQw_dT + dQi_dT)
        end if

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
