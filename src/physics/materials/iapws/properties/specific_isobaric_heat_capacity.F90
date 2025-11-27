
submodule(physics_material_iapws) iapws_specific_isobaric_heat_capacity
    implicit none
contains

    !> Calculate the specific isobaric heat capacity (Cp) for liquid water (Region 1).
    !> Valid range: \( 273.15 \text{ K} \le T \le 623.15 \text{ K} \), \( P_s(T) \le P \le 100 \text{ MPa} \).
    !> Formula: \( C_p = R \left[ -\tau^2 \gamma_{\tau\tau} + \frac{(\gamma_{\tau} - \tau \gamma_{\pi\tau})^2}{\gamma_{\pi}} \right] \)
    module pure elemental function calc_cp_iapws97_region1(T_in, P_in) result(cp)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Pressure [Pa]
        real(real64), intent(in) :: P_in
        !> Specific heat capacity at constant pressure [J/(kg K)]
        real(real64) :: cp

        real(real64) :: pi, tau
        real(real64) :: gamma_tt
        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        pi = P_in / p_star1
        tau = T_star1 / T_in

        ! ==========================================================
        ! Get derivatives from parent module
        ! ==========================================================
        gamma_tt = calc_gamma_tt_region1(pi, tau)

        ! ==========================================================
        ! Convert to physical units [J/(kg K)]
        ! ==========================================================
        cp = -tau**2.0d0 * gamma_tt * specific_gas_constant_water

    end function calc_cp_iapws97_region1

    module pure elemental function calc_cp_iapws97_region2(T_in, P_in) result(cp)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Pressure [Pa]
        real(real64), intent(in) :: P_in
        !> Specific heat capacity at constant pressure [J/(kg K)]
        real(real64) :: cp

        real(real64) :: pi, tau
        real(real64) :: gamma_tt

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        pi = P_in / p_star2
        tau = T_star2 / T_in

        ! ==========================================================
        ! Get derivatives from parent module
        ! ==========================================================
        gamma_tt = calc_gamma0_tt_region2(pi, tau) + calc_gammar_tt_region2(pi, tau)

        cp = -tau**2.0d0 * gamma_tt * specific_gas_constant_water

    end function calc_cp_iapws97_region2

    module pure elemental function calc_cp_iapws97_region3(T_in, rho_in) result(cp)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Density [kg/m^3]
        real(real64), intent(in) :: rho_in
        !> Specific heat capacity at constant pressure [J/kg-K]
        real(real64) :: cp

        real(real64) :: delta, tau
        real(real64) :: phi_d, phi_dd, phi_dt, phi_tt
        real(real64) :: numerator, denominator, term1

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        delta = rho_in / rho_star3
        tau = T_star3 / T_in

        ! ==========================================================
        ! Get derivatives
        ! phi_d  : phi_delta
        ! phi_dd : phi_d_delta
        ! phi_dt : phi_d_tau
        ! phi_tt : phi_t_tau
        ! ==========================================================
        phi_d = calc_phi_d_region3(delta, tau)
        phi_dd = calc_phi_dd_region3(delta, tau)
        phi_dt = calc_phi_dt_region3(delta, tau)
        phi_tt = calc_phi_tt_region3(delta, tau)

        ! ==========================================================
        ! Calculate Cp [J/(kg K)]
        ! Formula: cp/R = -tau^2 * phi_tt + (delta*phi_d - delta*tau*phi_dt)^2 / (2*delta*phi_d + delta^2*phi_dd)
        ! ==========================================================

        term1 = -tau**2 * phi_tt

        ! 修正箇所: 分子の計算式
        numerator = (delta * phi_d - delta * tau * phi_dt)**2

        denominator = 2.0d0 * delta * phi_d + delta**2 * phi_dd

        cp = (term1 + numerator / denominator) * specific_gas_constant_water

    end function calc_cp_iapws97_region3

    !> Specific Isobaric Heat Capacity [J/(kg K)]
    !> Formula: cp = -R * tau^2 * gamma_tautau
    module pure elemental function calc_cp_iapws97_region5(T_in, P_in) result(cp)
        implicit none
        real(real64), intent(in) :: T_in
        real(real64), intent(in) :: P_in
        real(real64) :: cp

        real(real64) :: pi, tau
        real(real64) :: gamma_tt

        pi = P_in / p_star5
        tau = T_star5 / T_in

        gamma_tt = calc_gamma0_tt_region5(pi, tau) + calc_gammar_tt_region5(pi, tau)

        cp = specific_gas_constant_water * (-tau**2 * gamma_tt)
    end function calc_cp_iapws97_region5

    module pure elemental function calc_cp_iapws06_Ih(T_in, P_in) result(cp)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Pressure [Pa]
        real(real64), intent(in) :: P_in
        !> Specific heat capacity at constant pressure [J/(kg K)]
        real(real64) :: cp

        real(real64) :: pi, tau
        real(real64) :: gamma_tt

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        pi = P_in / p_starIh
        tau = T_in / T_starIh

        ! ==========================================================
        ! Get derivatives from parent module
        ! ==========================================================
        gamma_tt = calc_gamma_tt_iapws06_Ih(pi, tau)

        ! ==========================================================
        ! Convert to physical units [J/(kg K)]
        ! ==========================================================
        cp = -T_in * gamma_tt

    end function calc_cp_iapws06_Ih

end submodule iapws_specific_isobaric_heat_capacity
