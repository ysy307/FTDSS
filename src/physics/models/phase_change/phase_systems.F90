!>
!> @brief Manages phase composition (water, ice, gas, vapor) and their derivatives.
!> Integrates Fusion and Vaporization models to ensure thermodynamic consistency.
!>
module physics_models_phase_systems
    use, intrinsic :: iso_fortran_env
    use :: iapws, only:type_iapws97, type_iapws06
    use :: module_core, only:type_state
    use :: physics_constants, only:latent_heat_fusion_water_0C
    use :: physics_models_phase_change_liquid_solid_gcc, only:abst_gcc
    use :: physics_models_wrf, only:abst_wrf
    use :: physics_models_phase_change_liquid_solid_fusion, only:type_fusion
    use :: physics_models_phase_change_liquid_vapor_vaporization, only:type_evaporation
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
        ! procedure, public :: update_phases_array ! 必要に応じて実装
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
    !> @brief P, T から全ての相の状態量(量と微分)を一括更新する
    !>
    pure elemental subroutine update_water_phases(self, state)
        implicit none
        class(type_phase_manager), intent(in) :: self
        type(type_state), intent(inout) :: state

        real(real64) :: water_content, ice_content, air_content, vapor_content, porosity

        ! Local variables for derivatives
        real(real64) :: dQw_dP, dQw_dT
        real(real64) :: dQi_dP, dQi_dT
        real(real64) :: dQa_dP, dQa_dT
        real(real64) :: dQv_dP, dQv_dT

        ! 2. 液状水分量 (theta_w) の更新
        call self%fusion%calc_water_content(state, water_content)
        call self%fusion%calc_water_content_derivatives(state, dQw_dP, dQw_dT)
        call state%water_content%set(water_content)
        call state%dQw_dP%set(dQw_dP)
        call state%dQw_dT%set(dQw_dT)

        ! 3. 氷含有量 (theta_i) の更新
        call self%fusion%calc_ice_content(state, ice_content)
        call self%fusion%calc_ice_content_derivatives(state, dQi_dP, dQi_dT)
        call state%ice_content%set(ice_content)
        call state%dQi_dP%set(dQi_dP)
        call state%dQi_dT%set(dQi_dT)

        ! 4. 空気相 (theta_g) の更新
        call state%porosity%get(porosity)
        dQa_dP = -1.0d0 * (dQw_dP + dQi_dP)
        dQa_dT = -1.0d0 * (dQw_dT + dQi_dT)
        call state%air_content%set(porosity - water_content - ice_content)
        call state%dQa_dP%set(dQa_dP)
        call state%dQa_dT%set(dQa_dT)

        ! 5. 蒸気量 (theta_v) の更新
        call self%evap%calc_vapor_content(state, vapor_content)
        call self%evap%calc_vapor_content_derivatives(state, dQv_dP, dQv_dT)
        call state%vapor_content%set(vapor_content)
        call state%dQv_dP%set(dQv_dP)
        call state%dQv_dT%set(dQv_dT)

    end subroutine update_water_phases

    pure elemental subroutine calc_latent_heat_fusion(self, state, Lf)
        implicit none
        class(type_phase_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: Lf

        Lf = latent_heat_fusion_water_0C

    end subroutine calc_latent_heat_fusion

    pure elemental subroutine calc_latent_heat_vaporization(self, state, Lv)
        implicit none
        class(type_phase_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: Lv

        real(real64) :: temperature, temperature_K

        call state%temperature%get(temperature)
        call self%evap%shift_temperature_absolute(temperature, temperature_K)

        call self%evap%calc_latent_heat_vaporization(temperature_K, Lv)

    end subroutine calc_latent_heat_vaporization

    pure elemental subroutine deriv_pressure_ice_water(self, state, deriv)
        implicit none
        class(type_phase_manager), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: deriv

        call self%fusion%deriv_pressure_ice_water(state, deriv)

    end subroutine deriv_pressure_ice_water

end module physics_models_phase_systems
