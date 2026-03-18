!>
!> @brief Manages phase composition (water, ice, gas, vapor) and their derivatives.
!> Integrates Fusion and Vaporization models to ensure thermodynamic consistency.
!>
module models_phase_change_manager
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
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
        type(type_fusion), private :: fusion
        type(type_evaporation), private :: evaporation
    contains
        procedure, public :: initialize
        procedure, public :: update_water_phases
        procedure, public :: calc_latent_heat_fusion
        procedure, public :: calc_latent_heat_vaporization
        ! procedure, public :: deriv_pressure_ice_water
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
        call self%evaporation%initialize(water)

    end subroutine initialize

!>
    !> @brief Update all phase quantities and their derivatives from P and T.
    !>
    subroutine update_water_phases(self, state)
        implicit none
        class(type_phase_manager), intent(in) :: self
        type(type_state), intent(inout) :: state

        real(real64) :: water_content, ice_content, air_content, vapor_content, porosity
        real(real64) :: dQw_dP, dQw_dT
        real(real64) :: dQi_dP, dQi_dT
        real(real64) :: dQa_dP, dQa_dT
        real(real64) :: dQv_dP, dQv_dT
        real(real64) :: relative_humidity

        ! 1. Porosity の取得
        call state%porosity%get(porosity)

        ! 2. Fusion モデルによる液相量と各相微分の計算
        ! インターフェースに基づき state の微分項を更新する
        call self%fusion%calc_derivatives(state)

        ! 3. Fusion の結果を state から取得し，空気量（気相）を決定
        call state%water_content%get(water_content)
        call state%ice_content%get(ice_content) ! ソルバ保持の履歴値

        call state%dQw_dP%get(dQw_dP)
        call state%dQw_dT%get(dQw_dT)
        call state%dQi_dP%get(dQi_dP)
        call state%dQi_dT%get(dQi_dT)

        ! 体積保存則 n = theta_w + theta_i + theta_a より空気量を決定
        air_content = porosity - water_content - ice_content

        if (air_content <= 0.0d0) then
            air_content = 0.0d0
            dQa_dP = 0.0d0
            dQa_dT = 0.0d0
        else
            ! 微分も体積保存則に従う
            dQa_dP = -1.0d0 * (dQw_dP + dQi_dP)
            dQa_dT = -1.0d0 * (dQw_dT + dQi_dT)
        end if

        call state%air_content%set(air_content)
        call state%dQa_dP%set(dQa_dP)
        call state%dQa_dT%set(dQa_dT)

        ! 4. Vaporization モデルによる計算
        ! インターフェース定義に基づきサフィックスを維持して呼び出し
        call self%evaporation%calc_relative_humidity(state, relative_humidity)
        call state%relative_humidity%set(relative_humidity)

        call self%evaporation%calc_vapor_content(state, vapor_content)
        call state%vapor_content%set(vapor_content)

        call self%evaporation%calc_vapor_content_derivatives(state, dQv_dP, dQv_dT)
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
        call self%evaporation%calc_latent_heat_vaporization(temperature, Lv)

    end subroutine calc_latent_heat_vaporization

    ! subroutine deriv_pressure_ice_water(self, state, deriv)
    !     implicit none
    !     class(type_phase_manager), intent(in) :: self
    !     type(type_state), intent(in) :: state
    !     real(real64), intent(inout) :: deriv

    !     call self%fusion%deriv_pressure_ice_water(state, deriv)

    ! end subroutine deriv_pressure_ice_water

end module models_phase_change_manager
