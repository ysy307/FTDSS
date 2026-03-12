!>
!> @brief Manages phase composition (water, ice, gas, vapor) and their derivatives.
!> Integrates Fusion and Vaporization models to ensure thermodynamic consistency.
!>
module models_phase_change_manager
    use, intrinsic :: iso_fortran_env
    use :: iapws, only:type_iapws97, type_iapws06
    use :: module_core, only:type_state
    use :: constitutive_constants, only:latent_heat_fusion_water_0C
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

        ! Temporary raw values
        real(real64) :: raw_ice_content
        real(real64) :: raw_water_content

        ! Local variables for derivatives
        real(real64) :: dQw_dP, dQw_dT
        real(real64) :: dQi_dP, dQi_dT
        real(real64) :: dQa_dP, dQa_dT
        real(real64) :: dQv_dP, dQv_dT
        logical :: is_clipped

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

        ! 4. Physical projection to avoid negative/oversaturated phase fractions
        is_clipped = .false.

        if (ice_content < 0.0d0) then
            ice_content = 0.0d0
            is_clipped = .true.
        end if
        if (ice_content > porosity) then
            ice_content = porosity
            is_clipped = .true.
        end if

        if (water_content < 0.0d0) then
            water_content = 0.0d0
            is_clipped = .true.
        end if
        if (water_content > porosity - ice_content) then
            water_content = max(0.0d0, porosity - ice_content)
            is_clipped = .true.
        end if

        air_content = porosity - water_content - ice_content
        if (air_content < 0.0d0) then
            air_content = 0.0d0
            is_clipped = .true.
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
        call self%evap%calc_relative_humidity(state, relative_humidity)
        call state%relative_humidity%set(relative_humidity)

        !    Note: If the model depends on gas-phase volume, the logic for
        !    vapor=0 when air_content=0 should be handled inside evap,
        !    but here we call it independently.
        call self%evap%calc_vapor_content(state, vapor_content)
        call self%evap%calc_vapor_content_derivatives(state, dQv_dP, dQv_dT)

        ! Guard: if air_content is zero, vapor cannot physically exist
        if (air_content <= epsilon(0.0d0)) then
            vapor_content = 0.0d0
            dQv_dP = 0.0d0
            dQv_dT = 0.0d0
        end if

        if (vapor_content < 0.0d0) then
            vapor_content = 0.0d0
            is_clipped = .true.
        end if

        if (is_clipped) then
            ! Keep Jacobian bounded when projection is active.
            dQw_dP = 0.0d0
            dQw_dT = 0.0d0
            dQi_dP = 0.0d0
            dQi_dT = 0.0d0
            dQa_dP = 0.0d0
            dQa_dT = 0.0d0
            dQv_dP = 0.0d0
            dQv_dT = 0.0d0
            call state%dQi_dP%set(dQi_dP)
            call state%dQi_dT%set(dQi_dT)
            call state%dQw_dP%set(dQw_dP)
            call state%dQw_dT%set(dQw_dT)
            call state%dQa_dP%set(dQa_dP)
            call state%dQa_dT%set(dQa_dT)
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

    subroutine deriv_pressure_ice_water(self, state, deriv)
        implicit none
        class(type_phase_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: deriv

        call self%fusion%deriv_pressure_ice_water(state, deriv)

    end subroutine deriv_pressure_ice_water

end module models_phase_change_manager
