submodule(physics_material_iapws97_region3) iapws97_region3_properties
    implicit none
contains
    module pure elemental function calc_p_iapws97_region3(T_in, rho_in) result(p)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Density [kg/m^3]
        real(real64), intent(in) :: rho_in
        !> Pressure [Pa]
        real(real64) :: p

        real(real64) :: delta, tau
        real(real64) :: phi_delta

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        delta = rho_in / rho_star3
        tau = T_star3 / T_in

        phi_delta = calc_phi_d_region3(delta, tau)

        ! ==========================================================
        ! Calculate Pressure [Pa]
        ! ==========================================================
        p = rho_in * R_w * T_in * delta * phi_delta

    end function calc_p_iapws97_region3

    module pure elemental function calc_u_iapws97_region3(T_in, rho_in) result(u)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Density [kg/m^3]
        real(real64), intent(in) :: rho_in
        !> Specific internal energy [J/kg]
        real(real64) :: u

        real(real64) :: delta, tau
        real(real64) :: phi_t

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        delta = rho_in / rho_star3
        tau = T_star3 / T_in

        phi_t = calc_phi_t_region3(delta, tau)

        ! ==========================================================
        ! Calculate Specific internal energy [J/kg]
        ! ==========================================================
        u = R_w * T_in * tau * phi_t

    end function calc_u_iapws97_region3

    module pure elemental function calc_s_iapws97_region3(T_in, rho_in) result(s)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Density [kg/m^3]
        real(real64), intent(in) :: rho_in
        !> Specific entropy [J/kg-K]
        real(real64) :: s

        real(real64) :: delta, tau
        real(real64) :: phi, phi_t

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        delta = rho_in / rho_star3
        tau = T_star3 / T_in

        phi = calc_phi_region3(delta, tau)
        phi_t = calc_phi_t_region3(delta, tau)

        ! ==========================================================
        ! Calculate Specific entropy [J/kg-K]
        ! ==========================================================
        s = R_w * (tau * phi_t - phi)
    end function calc_s_iapws97_region3

    module pure elemental function calc_h_iapws97_region3(T_in, rho_in) result(h)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Density [kg/m^3]
        real(real64), intent(in) :: rho_in
        !> Specific enthalpy [J/kg]
        real(real64) :: h

        real(real64) :: delta, tau
        real(real64) :: phi_t, phi_d

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        delta = rho_in / rho_star3
        tau = T_star3 / T_in

        phi_t = calc_phi_t_region3(delta, tau)
        phi_d = calc_phi_d_region3(delta, tau)

        ! ==========================================================
        ! Calculate specific enthalpy [J/kg]
        ! ==========================================================
        h = R_w * T_in * (tau * phi_t + delta * phi_d)

    end function calc_h_iapws97_region3

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

        cp = (term1 + numerator / denominator) * R_w

    end function calc_cp_iapws97_region3

    module pure elemental function calc_cv_iapws97_region3(T_in, rho_in) result(cv)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Density [kg/m^3]
        real(real64), intent(in) :: rho_in
        !> Specific heat capacity at constant volume [J/kg-K]
        real(real64) :: cv

        real(real64) :: delta, tau
        real(real64) :: phi_tt

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        delta = rho_in / rho_star3
        tau = T_star3 / T_in

        ! ==========================================================
        ! Get derivatives from parent module
        ! ==========================================================
        phi_tt = calc_phi_tt_region3(delta, tau)

        cv = -tau**2.0d0 * phi_tt * R_w

    end function calc_cv_iapws97_region3

    module pure elemental function calc_w_iapws97_region3(T_in, rho_in) result(w)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Density [kg/m^3]
        real(real64), intent(in) :: rho_in
        !> Specific speed of sound [m/s]
        real(real64) :: w

        real(real64) :: delta, tau
        real(real64) :: phi_d, phi_dd, phi_gt, phi_tt
        real(real64) :: w2_dimless

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        delta = rho_in / rho_star3
        tau = T_star3 / T_in

        ! ==========================================================
        ! Get derivatives from parent module
        ! ==========================================================
        phi_d = calc_phi_d_region3(delta, tau)
        phi_dd = calc_phi_dd_region3(delta, tau)
        ! 修正: 混合偏微分 phi_deltatau を取得 (元コードは phi_tautau でした)
        phi_gt = calc_phi_dt_region3(delta, tau)
        phi_tt = calc_phi_tt_region3(delta, tau)

        ! ==========================================================
        ! Calculate Speed of Sound [m/s]
        ! Formula: w^2 = R*T * [ 2*delta*phi_d + delta^2*phi_dd - ((delta*phi_d - delta*tau*phi_gt)^2) / (tau^2*phi_tt) ]
        ! Note: phi_g was removed as it is identical to phi_d
        ! ==========================================================
        w2_dimless = 2.0d0 * delta * phi_d + delta**2.0d0 * phi_dd - &
                     (delta * phi_d - delta * tau * phi_gt)**2.0d0 / (tau**2.0d0 * phi_tt)

        ! Convert to dimensions [m/s]
        ! Ensure the argument for sqrt is non-negative
        w = sqrt(max(R_w * T_in * w2_dimless, 0.0d0))

    end function calc_w_iapws97_region3

    ! ==========================================================
    ! Phase-equilibrium condition (Maxwell criterion)
    ! Region 3 saturation condition check
    ! ==========================================================

    !> Maxwell criterion Eq 1 (Liquid phase pressure consistency)
    !> Residual = p_s / (rho' * R * T) - delta' * phi_delta'
    !> Should be zero at equilibrium.
    module pure elemental function calc_maxwell_residual_1(T_in, rho_liq, p_sat) result(res)
        implicit none
        real(real64), intent(in) :: T_in ! Temperature [K]
        real(real64), intent(in) :: rho_liq ! Saturated Liquid Density [kg/m^3] (rho')
        real(real64), intent(in) :: p_sat ! Saturation Pressure [Pa] (p_s)
        real(real64) :: res

        real(real64) :: delta_liq, tau
        real(real64) :: phi_d_liq

        ! Dimensionless variables
        delta_liq = rho_liq / rho_star3
        tau = T_star3 / T_in

        ! Derivative
        phi_d_liq = calc_phi_d_region3(delta_liq, tau)

        ! Calculate Residual 1
        ! LHS = p_s / (R * T * rho')
        ! RHS = delta' * phi_delta'
        res = p_sat / (R_w * T_in * rho_liq) - delta_liq * phi_d_liq

    end function calc_maxwell_residual_1

    !> Maxwell criterion Eq 2 (Vapor phase pressure consistency)
    !> Residual = p_s / (rho'' * R * T) - delta'' * phi_delta''
    !> Should be zero at equilibrium.
    module pure elemental function calc_maxwell_residual_2(T_in, rho_vap, p_sat) result(res)
        implicit none
        real(real64), intent(in) :: T_in ! Temperature [K]
        real(real64), intent(in) :: rho_vap ! Saturated Vapor Density [kg/m^3] (rho'')
        real(real64), intent(in) :: p_sat ! Saturation Pressure [Pa] (p_s)
        real(real64) :: res

        real(real64) :: delta_vap, tau
        real(real64) :: phi_d_vap

        ! Dimensionless variables
        delta_vap = rho_vap / rho_star3
        tau = T_star3 / T_in

        ! Derivative
        phi_d_vap = calc_phi_d_region3(delta_vap, tau)

        ! Calculate Residual 2
        ! LHS = p_s / (R * T * rho'')
        ! RHS = delta'' * phi_delta''
        res = p_sat / (R_w * T_in * rho_vap) - delta_vap * phi_d_vap

    end function calc_maxwell_residual_2

    !> Maxwell criterion Eq 3 (Gibbs energy consistency / Equal Area Rule)
    !> Residual = (p_s / (R*T)) * (1/rho'' - 1/rho') - (phi(delta') - phi(delta''))
    !> Should be zero at equilibrium.
    module pure elemental function calc_maxwell_residual_3(T_in, rho_liq, rho_vap, p_sat) result(res)
        implicit none
        real(real64), intent(in) :: T_in ! Temperature [K]
        real(real64), intent(in) :: rho_liq ! Saturated Liquid Density [kg/m^3] (rho')
        real(real64), intent(in) :: rho_vap ! Saturated Vapor Density [kg/m^3] (rho'')
        real(real64), intent(in) :: p_sat ! Saturation Pressure [Pa] (p_s)
        real(real64) :: res

        real(real64) :: delta_liq, delta_vap, tau
        real(real64) :: phi_liq, phi_vap
        real(real64) :: lhs, rhs

        ! Dimensionless variables
        delta_liq = rho_liq / rho_star3
        delta_vap = rho_vap / rho_star3
        tau = T_star3 / T_in

        ! Helmholtz free energies
        phi_liq = calc_phi_region3(delta_liq, tau)
        phi_vap = calc_phi_region3(delta_vap, tau)

        ! Calculate Residual 3
        ! LHS = (p_s / (R * T)) * (1/rho'' - 1/rho')
        lhs = (p_sat / (R_w * T_in)) * (1.0d0 / rho_vap - 1.0d0 / rho_liq)

        ! RHS = phi(delta') - phi(delta'')
        rhs = phi_liq - phi_vap

        res = lhs - rhs

    end function calc_maxwell_residual_3

end submodule iapws97_region3_properties
