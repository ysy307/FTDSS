submodule(physics_material_iapws) iapws_pressure_coefficient
    implicit none
contains

    module pure elemental function calc_beta_iapws06_Ih(T_in, P_in) result(beta)
        implicit none
        real(real64), intent(in) :: T_in
        real(real64), intent(in) :: P_in
        real(real64) :: beta

        real(real64) :: pi, tau

        real(real64) :: gamma_tp, gamma_pp

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        pi = P_in / p_starIh
        tau = T_in / T_starIh

        gamma_tp = calc_gamma_tp_iapws06_Ih(pi, tau)
        gamma_pp = calc_gamma_pp_iapws06_Ih(pi, tau)

        ! ==========================================================
        ! Calculate pressure coefficient [Pa/K]
        ! ==========================================================
        beta = -gamma_tp / gamma_pp

    end function calc_beta_iapws06_Ih

end submodule iapws_pressure_coefficient
