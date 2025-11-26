submodule(physics_material_iapws) iapws_specific_volume
    implicit none
contains

    module pure elemental function get_nu_iapws97_region1(T_in, P_in) result(nu)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Pressure [Pa]
        real(real64), intent(in) :: P_in
        !> Specific volume [m^3/kg]
        real(real64) :: nu

        real(real64) :: pi, tau, gamma_pi

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        pi = P_in / p_star1
        tau = T_star1 / T_in

        gamma_pi = get_gamma_pi_region1(pi, tau)

        ! ==========================================================
        ! Calculate specific volume [m^3/kg]
        ! ==========================================================
        nu = specific_gas_constant_water * T_in * gamma_pi / p_star1
    end function get_nu_iapws97_region1

    module pure elemental function get_nu_iapws97_region2(T_in, P_in) result(nu)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Pressure [Pa]
        real(real64), intent(in) :: P_in
        !> Specific volume [m^3/kg]
        real(real64) :: nu

        real(real64) :: pi, tau, gamma_pi

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        pi = P_in / p_star2
        tau = T_star2 / T_in

        ! ==========================================================
        ! Calculate Density
        ! ==========================================================
        gamma_pi = get_gamma0_pi_region2(pi, tau) + get_gammar_pi_region2(pi, tau)
        nu = specific_gas_constant_water * T_in * gamma_pi / p_star2

    end function get_nu_iapws97_region2

end submodule iapws_specific_volume
