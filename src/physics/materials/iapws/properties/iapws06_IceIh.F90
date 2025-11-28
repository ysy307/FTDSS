submodule(physics_material_iapws06_IceIh) iapws06_IceIh_properties
    implicit none
contains
    module pure elemental function calc_nu_iapws06_Ih(T_in, P_in) result(nu)
        implicit none
        real(real64), intent(in) :: T_in
        real(real64), intent(in) :: P_in
        real(real64) :: nu

        real(real64) :: pi, tau

        ! Dimensionless variables
        pi = P_in / p_starIh
        tau = T_in / T_starIh

        ! Specific Volume [m^3/kg]
        nu = calc_gamma_p_iapws06_Ih(pi, tau)

    end function calc_nu_iapws06_Ih

    module pure elemental function calc_u_iapws06_Ih(T_in, P_in) result(u)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Pressure [Pa]
        real(real64), intent(in) :: P_in
        !> Specific internal energy [J/kg]
        real(real64) :: u

        real(real64) :: pi, tau
        real(real64) :: gamma
        real(real64) :: gamma_t, gamma_p

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        pi = P_in / p_starIh
        tau = T_in / T_starIh

        gamma = calc_gamma_iapws06_Ih(pi, tau)
        gamma_t = calc_gamma_t_iapws06_Ih(pi, tau)
        gamma_p = calc_gamma_p_iapws06_Ih(pi, tau)
        ! ==========================================================
        ! Calculate Specific internal energy [J/kg]
        ! ==========================================================
        u = gamma * T_in * gamma_t - P_in * gamma_p

    end function calc_u_iapws06_Ih

    module pure elemental function calc_s_iapws06_Ih(T_in, P_in) result(s)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Pressure [Pa]
        real(real64), intent(in) :: P_in
        !> Specific entropy [J/(kg K)]
        real(real64) :: s

        real(real64) :: pi, tau
        real(real64) :: gamma_t

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        pi = P_in / p_starIh
        tau = T_in / T_starIh

        ! ==========================================================
        ! Calculate Specific entropy [J/(kg K)]
        ! ==========================================================
        s = -calc_gamma_t_iapws06_Ih(pi, tau)

    end function calc_s_iapws06_Ih

    module pure elemental function calc_h_iapws06_Ih(T_in, P_in) result(h)
        implicit none
        real(real64), intent(in) :: T_in ! Temperature [K]
        real(real64), intent(in) :: P_in ! Pressure [Pa]
        real(real64) :: h

        real(real64) :: pi, tau
        real(real64) :: gamma, gamma_t

        ! Dimensionless variables
        pi = P_in / p_starIh
        tau = T_in / T_starIh

        gamma = calc_gamma_iapws06_Ih(pi, tau)
        gamma_t = calc_gamma_t_iapws06_Ih(pi, tau)

        ! Calculate specific enthalpy [J/kg]
        h = gamma - T_in * gamma_t

    end function calc_h_iapws06_Ih

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

    module pure elemental function calc_alpha_iapws06_Ih(T_in, P_in) result(alpha)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Pressure [Pa]
        real(real64), intent(in) :: P_in
        !> Cubic expansion coefficient [1/K]
        real(real64) :: alpha

        real(real64) :: pi, tau
        real(real64) :: gamma_p, gamma_tp

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        pi = P_in / p_starIh
        tau = T_in / T_starIh

        gamma_p = calc_gamma_p_iapws06_Ih(pi, tau)
        gamma_tp = calc_gamma_tp_iapws06_Ih(pi, tau)

        ! ==========================================================
        ! Calculate cubic expansion coefficient [1/K]
        ! ==========================================================
        alpha = gamma_tp / gamma_p

    end function calc_alpha_iapws06_Ih

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

end submodule iapws06_IceIh_properties
