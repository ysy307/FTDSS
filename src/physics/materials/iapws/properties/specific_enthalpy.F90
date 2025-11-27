submodule(physics_material_iapws) iapws_specific_enthalpy
    implicit none
contains
    !> Calculate the specific enthalpy of liquid water (Region 1).
    !> Valid range: \( 273.15 \text{ K} \le T \le 623.15 \text{ K} \), \( P_s(T) \le P \le 100 \text{ MPa} \).
    module pure elemental function calc_h_iapws97_region1(T_in, P_in) result(h)
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
        pi = P_in / p_star1
        tau = T_star1 / T_in

        gamma_t = calc_gamma_t_region1(pi, tau)

        ! ==========================================================
        ! Calculate specific enthalpy [J/kg]
        ! ==========================================================
        h = specific_gas_constant_water * T_in * tau * gamma_t

    end function calc_h_iapws97_region1

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
        h = specific_gas_constant_water * T_in * tau * gamma_t

    end function calc_h_iapws97_region2

    module pure elemental function calc_h_iapws97_region3(T_in, rho_in) result(h)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Density [kg/m^3]
        real(real64), intent(in) :: rho_in
        !> Specific enthalpy [J/kg]
        real(real64) :: h

        real(real64) :: delta, tau
        real(real64) :: phi_t, phi_d

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        delta = rho_in / rho_star3
        tau = T_star3 / T_in

        phi_t = calc_phi_t_region3(delta, tau)
        phi_d = calc_phi_d_region3(delta, tau)

        ! ==========================================================
        ! Calculate specific enthalpy [J/kg]
        ! ==========================================================
        h = specific_gas_constant_water * T_in * (tau * phi_t + delta * phi_d)

    end function calc_h_iapws97_region3

    !> Specific Enthalpy [J/kg]
    !> Formula: h = R*T * tau * gamma_tau
    module pure elemental function calc_h_iapws97_region5(T_in, P_in) result(h)
        implicit none
        real(real64), intent(in) :: T_in
        real(real64), intent(in) :: P_in
        real(real64) :: h

        real(real64) :: pi, tau
        real(real64) :: gamma_t

        pi = P_in / p_star5
        tau = T_star5 / T_in

        gamma_t = calc_gamma0_t_region5(pi, tau) + calc_gammar_t_region5(pi, tau)

        h = specific_gas_constant_water * T_in * tau * gamma_t
    end function calc_h_iapws97_region5

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

end submodule iapws_specific_enthalpy
