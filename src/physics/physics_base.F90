!> @brief 物理計算のための抽象型とインターフェース定義モジュール
!> @details 状態変数(State)と具体的なIAPWS物質モデル(水・氷)を接続し、物性値を計算します。
module physics_base
    use, intrinsic :: iso_fortran_env
    use :: iapws, only:type_iapws97, type_iapws06
    use :: module_core, only:type_state
    use :: physics_constants, only:TtoK => celsius_to_kelvin, &
        P_atm => standard_atmospheric_pressure, &
        min_vapor_density, &
        reference_water_density
    implicit none
    private

    public :: abst_physics
    public :: type_iapws_wrapper

    !> @brief 物理計算の抽象基底クラス
    !> @details 水、氷、水蒸気の熱力学的物性値の参照・計算を管理します。
    type, abstract :: abst_physics
        !> IAPWS-97 水物性計算オブジェクトへのポインタ
        type(type_iapws97), pointer :: water => null()
        !> IAPWS-06 氷物性計算オブジェクトへのポインタ
        type(type_iapws06), pointer :: ice => null()
        !> 初期化済みフラグ
        logical :: initialized = .false.
    contains
        !-----------------------------------------------------------------------
        ! Helper Procedures (Converters)
        !-----------------------------------------------------------------------
        !> 摂氏温度をケルビンに変換
        procedure, pass(self), public :: shift_temperature_absolute => shift_temperature_absolute_abst_physics
        !> ゲージ圧を絶対圧に変換
        procedure, pass(self), public :: shift_pressure_absolute => shift_pressure_absolute_abst_physics

        !-----------------------------------------------------------------------
        ! Liquid Water Properties
        !-----------------------------------------------------------------------
        !> 液体の水密度を計算
        procedure, pass(self), public :: calc_rho_water => calc_rho_water_abst_physics
        !> 水密度の温度微分を計算
        procedure, pass(self), public :: calc_drho_water_dT => calc_drho_water_dT_abst_physics
        !> 水密度の圧力微分を計算
        procedure, pass(self), public :: calc_drho_water_dP => calc_drho_water_dP_abst_physics
        !> 液体の水の定圧比熱を計算
        procedure, pass(self), public :: calc_cp_water => calc_cp_water_abst_physics

        !-----------------------------------------------------------------------
        ! Ice Properties
        !-----------------------------------------------------------------------
        !> 氷密度を計算
        procedure, pass(self), public :: calc_rho_ice => calc_rho_ice_abst_physics
        !> 氷密度の温度微分を計算
        procedure, pass(self), public :: calc_drho_ice_dT => calc_drho_ice_dT_abst_physics
        !> 氷密度の圧力微分を計算
        procedure, pass(self), public :: calc_drho_ice_dP => calc_drho_ice_dP_abst_physics
        !> 氷の定圧比熱を計算
        procedure, pass(self), public :: calc_cp_ice => calc_cp_ice_abst_physics

        !-----------------------------------------------------------------------
        ! Vapor Properties
        !-----------------------------------------------------------------------
        !> 相対湿度に基づき水蒸気密度を計算
        procedure, pass(self), public :: calc_rho_vapor => calc_rho_vapor_abst_physics
        !> 水蒸気密度の温度微分を計算
        procedure, pass(self), public :: calc_drho_vapor_dT => calc_drho_vapor_dT_abst_physics
        !> 水蒸気密度の圧力微分を計算
        procedure, pass(self), public :: calc_drho_vapor_dP => calc_drho_vapor_dP_abst_physics
        !> 飽和水蒸気密度を計算
        procedure, pass(self), public :: calc_rho_vapor_saturation => calc_rho_vapor_saturation_abst_physics
        !> 飽和水蒸気密度の温度微分を計算
        procedure, pass(self), public :: calc_drho_vapor_saturation_dT => calc_drho_vapor_saturation_dT_abst_physics
        !> 飽和水蒸気密度の圧力微分を計算
        procedure, pass(self), public :: calc_drho_vapor_saturation_dP => calc_drho_vapor_saturation_dP_abst_physics
        !> 水蒸気の定圧比熱(飽和状態)を計算
        procedure, pass(self), public :: calc_cp_vapor => calc_cp_vapor_abst_physics

        !-----------------------------------------------------------------------
        ! Private Helpers
        !-----------------------------------------------------------------------
        !> 状態変数から絶対温度と絶対圧力を一括取得する内部ヘルパー
        procedure, pass(self), private :: get_thermo_state_TP
        !> 状態変数から絶対温度のみを取得する内部ヘルパー
        procedure, pass(self), private :: get_thermo_state_T

        !>
        procedure, pass(self), public :: is_initialized => is_initialized_abst_physics
    end type abst_physics

    type, extends(abst_physics) :: type_iapws_wrapper
    contains
        !> @brief IAPWS物質モデルの初期化
        procedure, public :: initialize => initialize_iapws_wrapper
    end type type_iapws_wrapper

contains

    !===========================================================================
    ! Helper Procedures
    !===========================================================================

    !> @brief 摂氏温度をケルビンに変換します。
    subroutine shift_temperature_absolute_abst_physics(self, temperature_degree, temperature_K)
        implicit none
        class(abst_physics), intent(in) :: self
        real(real64), intent(in) :: temperature_degree !< 摂氏温度 [C]
        real(real64), intent(inout) :: temperature_K !< 絶対温度 [K]

        temperature_K = temperature_degree + TtoK
    end subroutine shift_temperature_absolute_abst_physics

    !> @brief ゲージ圧を絶対圧に変換します。
    !> @details 負圧(不飽和)の場合は大気圧を返します。
    subroutine shift_pressure_absolute_abst_physics(self, pressure_gauge, pressure_absolute)
        implicit none
        class(abst_physics), intent(in) :: self
        real(real64), intent(in) :: pressure_gauge !< ゲージ圧 [Pa]
        real(real64), intent(inout) :: pressure_absolute !< 絶対圧 [Pa]

        if (pressure_gauge < 0.0d0) then
            pressure_absolute = P_atm
        else
            pressure_absolute = P_atm + pressure_gauge
        end if
    end subroutine shift_pressure_absolute_abst_physics

    !> @brief Stateオブジェクトから絶対温度[K]と絶対圧力[Pa]を取得する内部ヘルパー
    subroutine get_thermo_state_TP(self, state, T_K, P_abs)
        implicit none
        class(abst_physics), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: T_K, P_abs

        real(real64) :: temp_c, press_g
        logical :: is_set

        call state%temperature%get(temp_c, is_set=is_set)
        if (is_set) then
            call self%shift_temperature_absolute(temp_c, T_K)
        else
            T_K = 273.15d0
        end if
        call state%pressure%get(press_g, is_set=is_set)
        if (is_set) then
            call self%shift_pressure_absolute(press_g, P_abs)
        else
            P_abs = P_atm
        end if

    end subroutine get_thermo_state_TP

    !> @brief Stateオブジェクトから絶対温度[K]のみを取得する内部ヘルパー
    subroutine get_thermo_state_T(self, state, T_K)
        implicit none
        class(abst_physics), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: T_K

        real(real64) :: temp_c
        logical :: is_set

        call state%temperature%get(temp_c, is_set=is_set)
        if (is_set) then
            call self%shift_temperature_absolute(temp_c, T_K)
        else
            T_K = 273.15d0
        end if
    end subroutine get_thermo_state_T

    !===========================================================================
    ! Liquid Water Implementation
    !===========================================================================

    !> @brief IAPWS-97を用いて液体の水密度を計算します。
    subroutine calc_rho_water_abst_physics(self, state, density)
        implicit none
        class(abst_physics), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: density
        real(real64) :: T_K, P_abs

        call self%get_thermo_state_TP(state, T_K, P_abs)
        if (T_K < 273.15d0) then
            density = reference_water_density
            return
        end if
        call self%water%calc_rho(T_K, P_abs, density)
    end subroutine calc_rho_water_abst_physics

    !> @brief 水密度の温度微分 (dRho/dT) を計算します。
    subroutine calc_drho_water_dT_abst_physics(self, state, deriv_density)
        implicit none
        class(abst_physics), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: deriv_density
        real(real64) :: T_K, P_abs

        call self%get_thermo_state_TP(state, T_K, P_abs)
        if (T_K < 273.15d0) then
            deriv_density = 0.0d0
            return
        end if
        call self%water%calc_drho_dT(T_K, P_abs, deriv_density)
    end subroutine calc_drho_water_dT_abst_physics

    !> @brief 水密度の圧力微分 (dRho/dP) を計算します。
    subroutine calc_drho_water_dP_abst_physics(self, state, deriv_density)
        implicit none
        class(abst_physics), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: deriv_density
        real(real64) :: T_K, P_abs

        call self%get_thermo_state_TP(state, T_K, P_abs)
        if (T_K < 273.15d0) then
            deriv_density = 0.0d0
            return
        end if
        call self%water%calc_drho_dP(T_K, P_abs, deriv_density)
    end subroutine calc_drho_water_dP_abst_physics

    !> @brief 液体の水の定圧比熱 (Cp) を計算します。
    subroutine calc_cp_water_abst_physics(self, state, cp)
        implicit none
        class(abst_physics), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: cp
        real(real64) :: T_K, P_abs

        call self%get_thermo_state_TP(state, T_K, P_abs)
        if (T_K < 273.15d0) then
            cp = 4181.3d0 ! 約定値 (水の比熱 4.1813 kJ/kg-K)
            return
        end if
        call self%water%calc_cp(T_K, P_abs, cp)
    end subroutine calc_cp_water_abst_physics

    !===========================================================================
    ! Ice Implementation
    !===========================================================================

    !> @brief IAPWS-06を用いて氷密度を計算します。
    subroutine calc_rho_ice_abst_physics(self, state, density)
        implicit none
        class(abst_physics), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: density
        real(real64) :: T_K, P_abs

        call self%get_thermo_state_TP(state, T_K, P_abs)
        call self%ice%calc_rho(T_K, P_abs, density)
    end subroutine calc_rho_ice_abst_physics

    !> @brief 氷密度の温度微分 (dRho/dT) を計算します。
    subroutine calc_drho_ice_dT_abst_physics(self, state, deriv_density)
        implicit none
        class(abst_physics), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: deriv_density
        real(real64) :: T_K, P_abs

        call self%get_thermo_state_TP(state, T_K, P_abs)
        call self%ice%calc_drho_dT(T_K, P_abs, deriv_density)
    end subroutine calc_drho_ice_dT_abst_physics

    !> @brief 氷密度の圧力微分 (dRho/dP) を計算します。
    subroutine calc_drho_ice_dP_abst_physics(self, state, deriv_density)
        implicit none
        class(abst_physics), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: deriv_density
        real(real64) :: T_K, P_abs

        call self%get_thermo_state_TP(state, T_K, P_abs)
        call self%ice%calc_drho_dP(T_K, P_abs, deriv_density)
    end subroutine calc_drho_ice_dP_abst_physics

    !> @brief 氷の定圧比熱 (Cp) を計算します。
    subroutine calc_cp_ice_abst_physics(self, state, cp)
        implicit none
        class(abst_physics), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: cp
        real(real64) :: T_K, P_abs

        call self%get_thermo_state_TP(state, T_K, P_abs)
        call self%ice%calc_cp(T_K, P_abs, cp)
    end subroutine calc_cp_ice_abst_physics

    !===========================================================================
    ! Vapor Implementation
    !===========================================================================

    !> @brief 相対湿度と飽和密度に基づき蒸気密度を計算します。
    !> @details 数値計算上の問題を避けるため、最小蒸気密度(min_vapor_density)を下限とします。
    subroutine calc_rho_vapor_abst_physics(self, state, density)
        implicit none
        class(abst_physics), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: density

        real(real64) :: T_K, relative_humidity

        call self%get_thermo_state_T(state, T_K)
        call state%relative_humidity%get(relative_humidity)

        call self%water%calc_saturation_density(T_K, density)
        density = max(density * relative_humidity, min_vapor_density)
    end subroutine calc_rho_vapor_abst_physics

    !> @brief 蒸気密度の温度微分 (dRho_v/dT) を計算します。
    !> @details d(Rho_sat * RH)/dT = dRho_sat/dT * RH (RHは定数と仮定)
    subroutine calc_drho_vapor_dT_abst_physics(self, state, deriv_density)
        implicit none
        class(abst_physics), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: deriv_density

        real(real64) :: T_K, relative_humidity, drho_sat_dT

        call self%get_thermo_state_T(state, T_K)
        call state%relative_humidity%get(relative_humidity)

        call self%water%calc_saturation_drho_dT(T_K, drho_sat_dT)

        deriv_density = drho_sat_dT * relative_humidity
    end subroutine calc_drho_vapor_dT_abst_physics

    !> @brief 蒸気密度の圧力微分 (dRho_v/dP) を計算します。
    !> @details 飽和密度が温度のみに依存するモデルの場合、圧力微分は 0 となります。
    subroutine calc_drho_vapor_dP_abst_physics(self, state, deriv_density)
        implicit none
        class(abst_physics), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: deriv_density

        ! IAPWS-97の飽和密度は温度依存のみであるため、
        ! 圧力に対する直接的な変化はないとみなす (RH一定条件下)
        deriv_density = 0.0d0
    end subroutine calc_drho_vapor_dP_abst_physics

    !> @brief 飽和水蒸気密度を計算します。
    subroutine calc_rho_vapor_saturation_abst_physics(self, state, density)
        implicit none
        class(abst_physics), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: density
        real(real64) :: T_K

        call self%get_thermo_state_T(state, T_K)
        call self%water%calc_saturation_density(T_K, density)
    end subroutine calc_rho_vapor_saturation_abst_physics

    !> @brief 飽和水蒸気密度の温度微分を計算します。
    subroutine calc_drho_vapor_saturation_dT_abst_physics(self, state, deriv_density)
        implicit none
        class(abst_physics), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: deriv_density
        real(real64) :: T_K

        call self%get_thermo_state_T(state, T_K)
        call self%water%calc_saturation_drho_dT(T_K, deriv_density)
    end subroutine calc_drho_vapor_saturation_dT_abst_physics

    !> @brief 飽和水蒸気密度の圧力微分を計算します。
    subroutine calc_drho_vapor_saturation_dP_abst_physics(self, state, deriv_density)
        implicit none
        class(abst_physics), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: deriv_density
        real(real64) :: T_K

        call self%get_thermo_state_T(state, T_K)
        call self%water%calc_saturation_drho_dP(T_K, deriv_density)
    end subroutine calc_drho_vapor_saturation_dP_abst_physics

    !> @brief 蒸気の定圧比熱 (Cp) を飽和状態で計算します。
    subroutine calc_cp_vapor_abst_physics(self, state, cp)
        implicit none
        class(abst_physics), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: cp
        real(real64) :: T_K

        call self%get_thermo_state_T(state, T_K)
        call self%water%calc_saturation_cp(T_K, cp)
    end subroutine calc_cp_vapor_abst_physics

    !> @brief IAPWS物質モデルの初期化を行います。
    subroutine initialize_iapws_wrapper(self, water_model, ice_model)
        implicit none
        class(type_iapws_wrapper), intent(inout) :: self
        type(type_iapws97), intent(in), target :: water_model
        type(type_iapws06), intent(in), target :: ice_model

        self%water => water_model
        self%ice => ice_model
    end subroutine initialize_iapws_wrapper

    function is_initialized_abst_physics(self) result(initialized)
        implicit none
        class(abst_physics), intent(in) :: self
        logical :: initialized

        initialized = self%initialized

    end function is_initialized_abst_physics

end module physics_base
