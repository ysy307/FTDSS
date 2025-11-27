submodule(physics_material_iapws) iapws_isentropic_compressibility
    implicit none
contains

    module pure elemental function calc_kappa_s_iapws06_Ih(T_in, P_in) result(kappa_s)
        implicit none
        real(real64), intent(in) :: T_in
        real(real64), intent(in) :: P_in
        real(real64) :: kappa_s

        real(real64) :: pi, tau
        real(real64) :: gamma_p, gamma_tp, gamma_tt, gamma_pp

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        pi = P_in / p_starIh
        tau = T_in / T_starIh

        gamma_p = calc_gamma_p_iapws06_Ih(pi, tau)
        gamma_tp = calc_gamma_tp_iapws06_Ih(pi, tau)
        gamma_tt = calc_gamma_tt_iapws06_Ih(pi, tau)
        gamma_pp = calc_gamma_pp_iapws06_Ih(pi, tau)

        ! ==========================================================
        ! Calculate isentropic compressibility [1/Pa]
        ! ==========================================================
        kappa_s = (gamma_tp**2 - gamma_tt * gamma_pp) / (gamma_p * gamma_tt)

    end function calc_kappa_s_iapws06_Ih

end submodule iapws_isentropic_compressibility
