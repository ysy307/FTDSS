submodule(physics_material_iapws) iapws_isothermal_compressibility
    implicit none
contains

    module pure elemental function calc_kappa_T_iapws06_Ih(T_in, P_in) result(kappa_T)
        implicit none
        real(real64), intent(in) :: T_in
        real(real64), intent(in) :: P_in
        real(real64) :: kappa_T

        real(real64) :: pi, tau
        real(real64) :: gamma_p, gamma_pp

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        pi = P_in / p_starIh
        tau = T_in / T_starIh

        gamma_p = calc_gamma_p_iapws06_Ih(pi, tau)
        gamma_pp = calc_gamma_pp_iapws06_Ih(pi, tau)

        ! ==========================================================
        ! Calculate isothermal compressibility [1/Pa]
        ! ==========================================================
        kappa_T = -gamma_pp / gamma_p

    end function calc_kappa_T_iapws06_Ih

end submodule iapws_isothermal_compressibility
