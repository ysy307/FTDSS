submodule(physics_material_iapws) iapws_specific_entropy
    implicit none
contains
    module pure elemental function get_s_iapws97_region1(T_in, P_in) result(s)
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

        gamma = get_gamma_region1(pi, tau)
        gamma_t = get_gamma_tau_region1(pi, tau)

        ! ==========================================================
        ! Calculate Specific entropy [J/kg-K]
        ! ==========================================================
        s = specific_gas_constant_water * (tau * gamma_t - gamma)

    end function get_s_iapws97_region1

    module pure elemental function get_s_iapws97_region2(T_in, P_in) result(s)
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

        gamma0 = get_gamma0_region2(pi, tau)
        gammar = get_gammar_region2(pi, tau)
        gamma0_t = get_gamma0_tau_region2(pi, tau)
        gammar_t = get_gammar_tau_region2(pi, tau)

        ! ==========================================================
        ! Calculate Specific entropy [J/kg-K]
        ! ==========================================================
        s = specific_gas_constant_water * (tau * (gamma0_t + gammar_t) - (gamma0 + gammar))

    end function get_s_iapws97_region2

    module pure elemental function get_s_iapws97_region3(T_in, rho_in) result(s)
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

        phi = get_phi_region3(delta, tau)
        phi_t = get_phi_tau_region3(delta, tau)

        ! ==========================================================
        ! Calculate Specific entropy [J/kg-K]
        ! ==========================================================
        s = specific_gas_constant_water * (tau * phi_t - phi)
    end function get_s_iapws97_region3

end submodule iapws_specific_entropy
