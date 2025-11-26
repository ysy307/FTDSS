submodule(physics_material_iapws) iapws_sound_speed
    implicit none
contains
    module pure elemental function get_w_iapws97_region1(T_in, P_in) result(w)
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
        gamma_p = get_gamma_pi_region1(pi, tau)
        gamma_tt = get_gamma_tautau_region1(pi, tau)
        gamma_pp = get_gamma_pipi_region1(pi, tau)
        gamma_pt = get_gamma_pitau_region1(pi, tau)

        numerator = gamma_p**2

        ! Note: gamma_tt is negative (related to Cp), so the first term in denominator is negative.
        ! gamma_pp is also negative (related to dv/dp). The total denominator must be positive.
        denominator = ((gamma_p - tau * gamma_pt)**2) / (tau**2 * gamma_tt) - gamma_pp

        w2_dimless = numerator / denominator

        ! Convert to dimensions [m/s]
        ! R is typically in kJ/(kg K), so multiply by 1000 to get J/(kg K) = m^2/s^2
        w = sqrt(max(specific_gas_constant_water * T_in * w2_dimless, 0.0d0))

    end function get_w_iapws97_region1

    module pure elemental function get_w_iapws97_region2(T_in, P_in) result(w)
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
        gammar_p = get_gammar_pi_region2(pi, tau)
        gammar_pp = get_gammar_pipi_region2(pi, tau)
        gammar_pt = get_gammar_pitau_region2(pi, tau)
        gammar_tt = get_gammar_tautau_region2(pi, tau)

        ! Ideal gas part derivative (only gamma0_tautau is needed for the denominator)
        gamma0_tt = get_gamma0_tautau_region2(pi, tau)

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
        w = sqrt(max(specific_gas_constant_water * T_in * w2_dimless, 0.0d0))

    end function get_w_iapws97_region2

    module pure elemental function get_w_iapws97_region3(T_in, rho_in) result(w)
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
        phi_d = get_phi_delta_region3(delta, tau)
        phi_dd = get_phi_deltadelta_region3(delta, tau)
        ! 修正: 混合偏微分 phi_deltatau を取得 (元コードは phi_tautau でした)
        phi_gt = get_phi_deltatau_region3(delta, tau)
        phi_tt = get_phi_tautau_region3(delta, tau)

        ! ==========================================================
        ! Calculate Speed of Sound [m/s]
        ! Formula: w^2 = R*T * [ 2*delta*phi_d + delta^2*phi_dd - ((delta*phi_d - delta*tau*phi_gt)^2) / (tau^2*phi_tt) ]
        ! Note: phi_g was removed as it is identical to phi_d
        ! ==========================================================
        w2_dimless = 2.0d0 * delta * phi_d + delta**2.0d0 * phi_dd - &
                     (delta * phi_d - delta * tau * phi_gt)**2.0d0 / (tau**2.0d0 * phi_tt)

        ! Convert to dimensions [m/s]
        ! Ensure the argument for sqrt is non-negative
        w = sqrt(max(specific_gas_constant_water * T_in * w2_dimless, 0.0d0))

    end function get_w_iapws97_region3
end submodule iapws_sound_speed
