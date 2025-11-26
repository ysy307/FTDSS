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
        pi = P_in * 1.0d-6 / p_star1 ! Pa -> MPa
        tau = T_star1 / T_in

        gamma = get_gamma_region1(pi, tau)
        gamma_t = get_gamma_tau_region1(pi, tau)

        ! ==========================================================
        ! Calculate Specific entropy [J/kg-K]
        ! ==========================================================
        s = specific_gas_constant_water * (tau * gamma_t - gamma) * 1000.0d0

    end function get_s_iapws97_region1

end submodule iapws_specific_entropy
