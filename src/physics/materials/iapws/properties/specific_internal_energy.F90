submodule(physics_material_iapws) iapws_specific_internal_energy
    implicit none
contains

    module pure elemental function get_u_iapws97_region1(T_in, P_in) result(u)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Pressure [Pa]
        real(real64), intent(in) :: P_in
        !> Specific internal energy [J/kg]
        real(real64) :: u

        real(real64) :: pi, tau
        real(real64) :: gamma_t, gamma_p

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        pi = P_in * 1.0d-6 / p_star1 ! Pa -> MPa
        tau = T_star1 / T_in

        gamma_t = get_gamma_tau_region1(pi, tau)
        gamma_p = get_gamma_pi_region1(pi, tau)

        ! ==========================================================
        ! Calculate Specific internal energy [J/kg]
        ! ==========================================================
        u = specific_gas_constant_water * T_in * (tau * gamma_t - pi * gamma_p) * 1000.0d0

    end function get_u_iapws97_region1

end submodule iapws_specific_internal_energy
