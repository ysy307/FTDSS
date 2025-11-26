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
        pi = (P_in * 1.0d-6) / p_star1
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
        w = sqrt(specific_gas_constant_water * 1000.0d0 * T_in * w2_dimless)

    end function get_w_iapws97_region1

end submodule iapws_sound_speed
