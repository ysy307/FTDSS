submodule(physics_material_iapws97_region2) iapws97_region2_properties
    implicit none
contains
    module pure elemental function calc_nu_iapws97_region2(T_in, P_in) result(nu)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Pressure [Pa]
        real(real64), intent(in) :: P_in
        !> Specific volume [m^3/kg]
        real(real64) :: nu

        real(real64) :: pi, tau, gamma_pi

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        pi = P_in / p_star2
        tau = T_star2 / T_in

        ! ==========================================================
        ! Calculate Density
        ! ==========================================================
        gamma_pi = calc_gamma0_p_region2(pi, tau) + calc_gammar_p_region2(pi, tau)
        nu = R_w * T_in * gamma_pi / p_star2

    end function calc_nu_iapws97_region2

    module pure elemental function calc_rho_iapws97_region2(T_in, P_in) result(rho)
        implicit none
        real(real64), intent(in) :: T_in
        real(real64), intent(in) :: P_in
        real(real64) :: rho

        rho = 1.0d0 / calc_nu_iapws97_region2(T_in, P_in)

    end function calc_rho_iapws97_region2

    module pure elemental function calc_u_iapws97_region2(T_in, P_in) result(u)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Pressure [Pa]
        real(real64), intent(in) :: P_in
        !> Specific internal energy [J/kg]
        real(real64) :: u

        real(real64) :: pi, tau
        real(real64) :: gamma_tau, gamma_pi

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        pi = P_in / p_star2
        tau = T_star2 / T_in

        gamma_tau = calc_gamma0_t_region2(pi, tau) + calc_gammar_t_region2(pi, tau)
        gamma_pi = calc_gamma0_p_region2(pi, tau) + calc_gammar_p_region2(pi, tau)

        ! ==========================================================
        ! Calculate Specific internal energy [J/kg]
        ! ==========================================================
        u = R_w * T_in * (tau * gamma_tau - pi * gamma_pi)

    end function calc_u_iapws97_region2

    module pure elemental function calc_s_iapws97_region2(T_in, P_in) result(s)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Pressure [Pa]
        real(real64), intent(in) :: P_in
        !> Specific entropy [J/kg-K]
        real(real64) :: s

        real(real64) :: pi, tau
        real(real64) :: gamma0, gammar, gamma0_t, gammar_t

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        pi = P_in / p_star2
        tau = T_star2 / T_in

        gamma0 = calc_gamma0_region2(pi, tau)
        gammar = calc_gammar_region2(pi, tau)
        gamma0_t = calc_gamma0_t_region2(pi, tau)
        gammar_t = calc_gammar_t_region2(pi, tau)

        ! ==========================================================
        ! Calculate Specific entropy [J/kg-K]
        ! ==========================================================
        s = R_w * (tau * (gamma0_t + gammar_t) - (gamma0 + gammar))

    end function calc_s_iapws97_region2

    module pure elemental function calc_h_iapws97_region2(T_in, P_in) result(h)
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
        pi = P_in / p_star2
        tau = T_star2 / T_in

        gamma_t = calc_gamma0_t_region2(pi, tau) + calc_gammar_t_region2(pi, tau)

        ! ==========================================================
        ! Calculate specific enthalpy [J/kg]
        ! ==========================================================
        h = R_w * T_in * tau * gamma_t

    end function calc_h_iapws97_region2

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

        cp = -tau**2.0d0 * gamma_tt * R_w

    end function calc_cp_iapws97_region2

    module pure elemental function calc_cv_iapws97_region2(T_in, P_in) result(cv)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Pressure [Pa]
        real(real64), intent(in) :: P_in
        !> Specific heat capacity at constant volume [J/(kg K)]
        real(real64) :: cv

        real(real64) :: pi, tau
        real(real64) :: gammar_p
        real(real64) :: gamma0_tt, gammar_tt
        real(real64) :: gamma0_pp, gammar_pp
        real(real64) :: gamma0_pt, gammar_pt

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        pi = P_in / p_star2
        tau = T_star2 / T_in
        ! ==========================================================
        ! Get derivatives from parent module
        ! ==========================================================
        gammar_p = calc_gammar_p_region2(pi, tau)
        gamma0_tt = calc_gamma0_tt_region2(pi, tau)
        gammar_tt = calc_gammar_tt_region2(pi, tau)
        gamma0_pp = calc_gamma0_pp_region2(pi, tau)
        gammar_pp = calc_gammar_pp_region2(pi, tau)
        gamma0_pt = calc_gamma0_pt_region2(pi, tau)
        gammar_pt = calc_gammar_pt_region2(pi, tau)

        cv = (-tau**2.0d0 * (gamma0_tt + gammar_tt) &
              - (1.0d0 + pi * gammar_p - tau * pi * gammar_pt)**2.0d0 / (1 - pi**2.0d0 * gamma0_pp)) * R_w

    end function calc_cv_iapws97_region2

    module pure elemental function calc_w_iapws97_region2(T_in, P_in) result(w)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Pressure [Pa]
        real(real64), intent(in) :: P_in
        !> Speed of sound [m/s]
        real(real64) :: w

        real(real64) :: pi, tau
        real(real64) :: gammar_p, gammar_pp, gammar_pt, gammar_tt
        real(real64) :: gamma0_tt
        real(real64) :: numerator, denom_1, denom_2, w2_dimless

        ! ==========================================================
        ! Dimensionless variables for Region 2
        ! ==========================================================
        pi = P_in / p_star2
        tau = T_star2 / T_in

        ! ==========================================================
        ! Calculate derivatives needed for Region 2 Speed of Sound Formula
        ! Formula: IAPWS-97 Eq. 16
        ! Only residual part derivatives (gammar) and ideal part (gamma0_tt) are needed.
        ! ==========================================================

        ! Residual part derivatives
        gammar_p = calc_gammar_p_region2(pi, tau)
        gammar_pp = calc_gammar_pp_region2(pi, tau)
        gammar_pt = calc_gammar_pt_region2(pi, tau)
        gammar_tt = calc_gammar_tt_region2(pi, tau)

        ! Ideal gas part derivative (only gamma0_tautau is needed for the denominator)
        gamma0_tt = calc_gamma0_tt_region2(pi, tau)

        ! ==========================================================
        ! Calculate Speed of Sound using explicit Region 2 formula
        ! w^2/RT = Numerator / Denominator
        ! ==========================================================

        ! Numerator = 1 + 2*pi*gammar_pi + (pi*gammar_pi)^2
        numerator = 1.0d0 + 2.0d0 * pi * gammar_p + (pi * gammar_p)**2.0d0

        ! Denominator Term 1 = 1 - pi^2 * gammar_pipi
        denom_1 = 1.0d0 - pi**2.0d0 * gammar_pp

        ! Denominator Term 2 = (1 + pi*gammar_pi - tau*pi*gammar_pitau)^2 / (tau^2 * (gamma0_tautau + gammar_tautau))
        ! Note: (gamma0_tt + gammar_tt) corresponds to Cp and is negative.
        denom_2 = ((1.0d0 + pi * gammar_p - tau * pi * gammar_pt)**2) / (tau**2 * (gamma0_tt + gammar_tt))

        ! Total Denominator = Term1 + Term2
        ! (As per the image: (1 - pi^2*gammar_pp) + Term2)
        w2_dimless = numerator / (denom_1 + denom_2)

        ! Convert to dimensions [m/s]
        ! R is in [kJ/(kg K)], so *1000 to get [J/(kg K)] = [m^2/s^2]
        w = sqrt(max(R_w * T_in * w2_dimless, 0.0d0))

    end function calc_w_iapws97_region2

end submodule iapws97_region2_properties
