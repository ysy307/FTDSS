submodule(physics_material_iapws) iapws_specific_isochoric_heat_capacity
    implicit none
contains

    module pure elemental function get_cv_iapws97_region1(T_in, P_in) result(cv)
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
        pi = (P_in * 1.0d-6) / p_star1
        tau = T_star1 / T_in

        ! ==========================================================
        ! Get derivatives from parent module
        ! ==========================================================
        gamma_p = get_gamma_pi_region1(pi, tau)
        gamma_tt = get_gamma_tautau_region1(pi, tau)
        gamma_pp = get_gamma_pipi_region1(pi, tau)
        gamma_pt = get_gamma_pitau_region1(pi, tau)

        cv = (-tau**2.0d0 * gamma_tt + (gamma_p - tau * gamma_pt)**2.0d0 / gamma_pp) * specific_gas_constant_water * 1000.0d0

    end function get_cv_iapws97_region1

end submodule iapws_specific_isochoric_heat_capacity
