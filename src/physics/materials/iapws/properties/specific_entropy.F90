submodule(physics_material_iapws) iapws_specific_entropy
    implicit none
contains
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
        s = specific_gas_constant_water * (tau * gamma_t - gamma)

    end function calc_s_iapws97_region1

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
        s = specific_gas_constant_water * (tau * (gamma0_t + gammar_t) - (gamma0 + gammar))

    end function calc_s_iapws97_region2

    module pure elemental function calc_s_iapws97_region3(T_in, rho_in) result(s)
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

        phi = calc_phi_region3(delta, tau)
        phi_t = calc_phi_t_region3(delta, tau)

        ! ==========================================================
        ! Calculate Specific entropy [J/kg-K]
        ! ==========================================================
        s = specific_gas_constant_water * (tau * phi_t - phi)
    end function calc_s_iapws97_region3

    !> Specific Entropy [J/(kg K)]
    !> Formula: s = R * (tau*gamma_tau - gamma)
    module pure elemental function calc_s_iapws97_region5(T_in, P_in) result(s)
        implicit none
        real(real64), intent(in) :: T_in
        real(real64), intent(in) :: P_in
        real(real64) :: s

        real(real64) :: pi, tau
        real(real64) :: gamma, gamma_t

        pi = P_in / p_star5
        tau = T_star5 / T_in

        gamma = calc_gamma0_region5(pi, tau) + calc_gammar_region5(pi, tau)
        gamma_t = calc_gamma0_t_region5(pi, tau) + calc_gammar_t_region5(pi, tau)

        s = specific_gas_constant_water * (tau * gamma_t - gamma)
    end function calc_s_iapws97_region5

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

end submodule iapws_specific_entropy
