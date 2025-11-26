submodule(physics_material_iapws) iapws_specific_enthalpy
    implicit none
contains
    !> Calculate the specific enthalpy of liquid water (Region 1).
    !> Valid range: \( 273.15 \text{ K} \le T \le 623.15 \text{ K} \), \( P_s(T) \le P \le 100 \text{ MPa} \).
    module pure elemental function get_h_iapws97_region1(T_in, P_in) result(h)
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
        pi = P_in * 1.0d-6 / p_star1 ! Pa -> MPa
        tau = T_star1 / T_in

        gamma_t = get_gamma_tau_region1(pi, tau)

        ! ==========================================================
        ! Calculate specific enthalpy [J/kg]
        ! ==========================================================
        h = specific_gas_constant_water * T_in * tau * gamma_t * 1000.0d0

    end function get_h_iapws97_region1

    module pure elemental function get_enthalpy_iapws_region2(T_in, P_in) result(h)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Pressure [Pa]
        real(real64), intent(in) :: P_in
        !> Specific enthalpy [J/kg]
        real(real64) :: h

        real(real64) :: pi, tau
        real(real64) :: gamma_tau

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        pi = P_in * 1.0d-6 / p_star2 ! Pa -> MPa
        tau = T_star2 / T_in

        gamma_tau = get_gamma0_tau_region2(pi, tau) + get_gammar_tau_region2(pi, tau)

        ! ==========================================================
        ! Calculate specific enthalpy [J/kg]
        ! ==========================================================
        h = specific_gas_constant_water * T_in * tau * gamma_tau * 1000.0d0

    end function get_enthalpy_iapws_region2

end submodule iapws_specific_enthalpy
