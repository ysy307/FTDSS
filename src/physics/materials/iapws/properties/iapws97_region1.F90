submodule(physics_material_iapws97_region1) iapws97_region1_properties
    implicit none
contains
    module pure elemental function calc_nu_iapws97_region1(T_in, P_in) result(nu)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Pressure [Pa]
        real(real64), intent(in) :: P_in
        !> Specific volume [m^3/kg]
        real(real64) :: nu

        real(real64) :: pi, tau, gamma_p

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        pi = P_in / p_star1
        tau = T_star1 / T_in

        gamma_p = calc_gamma_p_region1(pi, tau)

        ! ==========================================================
        ! Calculate specific volume [m^3/kg]
        ! ==========================================================
        nu = R_w * T_in * gamma_p / p_star1
    end function calc_nu_iapws97_region1

    module pure elemental function calc_rho_iapws97_region1(T_in, P_in) result(rho)
        implicit none
        real(real64), intent(in) :: T_in
        real(real64), intent(in) :: P_in
        real(real64) :: rho

        rho = 1.0d0 / calc_nu_iapws97_region1(T_in, P_in)

    end function calc_rho_iapws97_region1

    module pure elemental function calc_u_iapws97_region1(T_in, P_in) result(u)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Pressure [Pa]
        real(real64), intent(in) :: P_in
        !> Specific internal energy [J/kg]
        real(real64) :: u

        real(real64) :: pi, tau
        real(real64) :: gamma_t, gamma_p

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        pi = P_in / p_star1
        tau = T_star1 / T_in

        gamma_t = calc_gamma_t_region1(pi, tau)
        gamma_p = calc_gamma_p_region1(pi, tau)

        ! ==========================================================
        ! Calculate Specific internal energy [J/kg]
        ! ==========================================================
        u = R_w * T_in * (tau * gamma_t - pi * gamma_p)

    end function calc_u_iapws97_region1

    module pure elemental function calc_s_iapws97_region1(T_in, P_in) result(s)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Pressure [Pa]
        real(real64), intent(in) :: P_in
        !> Specific entropy [J/kg-K]
        real(real64) :: s

        real(real64) :: pi, tau
        real(real64) :: gamma, gamma_t

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        pi = P_in / p_star1
        tau = T_star1 / T_in

        gamma = calc_gamma_region1(pi, tau)
        gamma_t = calc_gamma_t_region1(pi, tau)

        ! ==========================================================
        ! Calculate Specific entropy [J/kg-K]
        ! ==========================================================
        s = R_w * (tau * gamma_t - gamma)

    end function calc_s_iapws97_region1

    !> Calculate the specific enthalpy of liquid water (Region 1).
    !> Valid range: \( 273.15 \text{ K} \le T \le 623.15 \text{ K} \), \( P_s(T) \le P \le 100 \text{ MPa} \).
    module pure elemental function calc_h_iapws97_region1(T_in, P_in) result(h)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Pressure [Pa]
        real(real64), intent(in) :: P_in
        !> Specific enthalpy [J/kg]
        real(real64) :: h

        real(real64) :: pi, tau
        real(real64) :: gamma_t

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        pi = P_in / p_star1
        tau = T_star1 / T_in

        gamma_t = calc_gamma_t_region1(pi, tau)

        ! ==========================================================
        ! Calculate specific enthalpy [J/kg]
        ! ==========================================================
        h = R_w * T_in * tau * gamma_t

    end function calc_h_iapws97_region1

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
        cp = -tau**2.0d0 * gamma_tt * R_w

    end function calc_cp_iapws97_region1

    module pure elemental function calc_cv_iapws97_region1(T_in, P_in) result(cv)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Pressure [Pa]
        real(real64), intent(in) :: P_in
        !> Specific heat capacity at constant volume [J/(kg K)]
        real(real64) :: cv

        real(real64) :: pi, tau
        real(real64) :: gamma_p
        real(real64) :: gamma_tt, gamma_pp, gamma_pt
        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        pi = P_in / p_star1
        tau = T_star1 / T_in

        ! ==========================================================
        ! Get derivatives from parent module
        ! ==========================================================
        gamma_p = calc_gamma_p_region1(pi, tau)
        gamma_tt = calc_gamma_tt_region1(pi, tau)
        gamma_pp = calc_gamma_pp_region1(pi, tau)
        gamma_pt = calc_gamma_pt_region1(pi, tau)

        cv = (-tau**2.0d0 * gamma_tt + (gamma_p - tau * gamma_pt)**2.0d0 / gamma_pp) * R_w

    end function calc_cv_iapws97_region1

    module pure elemental function calc_w_iapws97_region1(T_in, P_in) result(w)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Pressure [Pa]
        real(real64), intent(in) :: P_in
        !> Speed of sound [m/s]
        real(real64) :: w

        real(real64) :: pi, tau
        real(real64) :: gamma_p
        real(real64) :: gamma_tt, gamma_pp, gamma_pt
        real(real64) :: numerator, denominator, w2_dimless

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        pi = P_in / p_star1
        tau = T_star1 / T_in

        ! ==========================================================
        ! Get derivatives from parent module
        ! ==========================================================
        gamma_p = calc_gamma_p_region1(pi, tau)
        gamma_tt = calc_gamma_tt_region1(pi, tau)
        gamma_pp = calc_gamma_pp_region1(pi, tau)
        gamma_pt = calc_gamma_pt_region1(pi, tau)

        numerator = gamma_p**2

        ! Note: gamma_tt is negative (related to Cp), so the first term in denominator is negative.
        ! gamma_pp is also negative (related to dv/dp). The total denominator must be positive.
        denominator = ((gamma_p - tau * gamma_pt)**2) / (tau**2 * gamma_tt) - gamma_pp

        w2_dimless = numerator / denominator

        ! Convert to dimensions [m/s]
        ! R is typically in kJ/(kg K), so multiply by 1000 to get J/(kg K) = m^2/s^2
        w = sqrt(max(R_w * T_in * w2_dimless, 0.0d0))

    end function calc_w_iapws97_region1

end submodule iapws97_region1_properties

