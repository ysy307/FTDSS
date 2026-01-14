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

        ! Temporary raw values
        real(real64) :: raw_ice_content
        real(real64) :: raw_water_content

        ! Local variables for derivatives
        real(real64) :: dQw_dP, dQw_dT
        real(real64) :: dQi_dP, dQi_dT
        real(real64) :: dQa_dP, dQa_dT
        real(real64) :: dQv_dP, dQv_dT

        ! 1. 空隙率の取得
        call state%porosity%get(porosity)

        ! 2. 氷含有量 (theta_i) の仮計算
        call self%fusion%calc_ice_content(state, raw_ice_content)
        call self%fusion%calc_ice_content_derivatives(state, dQi_dP, dQi_dT)

        ! --- 氷の物理的上限チェック (Ice Cap) ---
        if (raw_ice_content > porosity) then
            ! [氷過剰] 空隙すべてが氷
            ice_content = porosity

            ! 上限に張り付いているため、微分は0とする（これ以上増えない）
            ! dQi_dP = 0.0d0
            ! dQi_dT = 0.0d0

            ! 水・空気は存在できない
            water_content = 0.0d0
            dQw_dP = 0.0d0
            dQw_dT = 0.0d0

            air_content = 0.0d0
            dQa_dP = 0.0d0
            dQa_dT = 0.0d0
        else
            ! [通常] 氷は計算値通り
            ice_content = raw_ice_content
            ! 微分係数はそのまま使用 (dQi_dP, dQi_dT)

            ! 3. 液状水分量 (theta_w) の仮計算
            call self%fusion%calc_water_content(state, raw_water_content)
            call self%fusion%calc_water_content_derivatives(state, dQw_dP, dQw_dT)

            ! --- 水の物理的上限チェック (Water Cap) ---
            if (raw_water_content + ice_content > porosity) then
                ! [過飽和] 氷以外の残りの空隙を水が埋める
                water_content = max(0.0d0, porosity - ice_content)

                ! 水分量は (Porosity - Ice) に従属するため、微分は氷の逆符号になる
                dQw_dP = -1.0d0 * dQi_dP
                dQw_dT = -1.0d0 * dQi_dT

                ! 気相は無し
                air_content = 0.0d0
                dQa_dP = 0.0d0
                dQa_dT = 0.0d0
            else
                ! [不飽和] 計算値通り
                water_content = raw_water_content
                ! 微分係数はそのまま使用 (dQw_dP, dQw_dT)

                ! 気相 = 空隙 - 水 - 氷
                air_content = porosity - water_content - ice_content

                ! 気相の微分 (保存則より)
                dQa_dP = -1.0d0 * (dQw_dP + dQi_dP)
                dQa_dT = -1.0d0 * (dQw_dT + dQi_dT)
            end if
        end if

        ! 4. 値のセット（整合性確保済み）
        call state%ice_content%set(ice_content)
        call state%dQi_dP%set(dQi_dP)
        call state%dQi_dT%set(dQi_dT)

        call state%water_content%set(water_content)
        call state%dQw_dP%set(dQw_dP)
        call state%dQw_dT%set(dQw_dT)

        call state%air_content%set(air_content)
        call state%dQa_dP%set(dQa_dP)
        call state%dQa_dT%set(dQa_dT)

        ! 5. 蒸気量 (theta_v) の更新
        !    ※ 蒸気は気相体積に依存するモデルの場合、air_content=0なら蒸気も0になるロジックが
        !       evap内部に含まれている必要がありますが、ここでは独立として呼び出します。
        call self%evap%calc_vapor_content(state, vapor_content)
        call self%evap%calc_vapor_content_derivatives(state, dQv_dP, dQv_dT)

        ! air_contentが0なら物理的に蒸気も存在できないため、ここでガードしても良い
        if (air_content <= epsilon(0.0d0)) then
            vapor_content = 0.0d0
            dQv_dP = 0.0d0
            dQv_dT = 0.0d0
        end if

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
