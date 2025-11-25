submodule(physics_material_iapws) iapws_specific_isobaric_heat
    implicit none
contains
    !-----------------------------------------------------------------------------
    ! Region 1 (Liquid Water)
    ! Valid: 273.15 K <= T <= 623.15 K (Extrapolates well to < 273.15 K)
    !        p_s(T) <= P <= 100 MPa
    ! Input:
    !   T_in : Temperature [K]
    !   P_in : Pressure [Pa]
    ! Output:
    !   cp     : Specific isobaric heat capacity [kJ/kg/K]
    !-----------------------------------------------------------------------------
!> Specific heat capacity at constant pressure Cp [J/(kg K)]
    !!
    !! Formula: Cp = R * [ -tau^2 * g_tautau + (g_tau - tau * g_pitau)^2 / g_pi ]
    module pure elemental function get_cp_iapws_region1(T_in, P_in) result(cp)
        implicit none
        real(real64), intent(in) :: T_in !! Temperature [K]
        real(real64), intent(in) :: P_in !! Pressure [Pa]
        real(real64) :: cp !! Specific heat [J/(kg K)]

        real(real64) :: pi, tau
        real(real64) :: g_pi, g_tau, g_tautau, g_pitau
        real(real64) :: cp_dim ! dimensionless Cp/R

        ! Dimensionless variables
        pi = (P_in * 1.0d-6) / p_star1
        tau = T_star1 / T_in

        ! Get derivatives from parent module
        g_pi = get_gamma_pi_region1(pi, tau)
        g_tau = get_gamma_tau_region1(pi, tau)
        g_tautau = get_gamma_tautau_region1(pi, tau)
        g_pitau = get_gamma_pitau_region1(pi, tau)

        ! Calculate dimensionless Cp/R
        ! Note: Using parenthesis strictly to ensure correct order of operations
        cp_dim = (-tau**2 * g_tautau) + &
                 ((g_tau - tau * g_pitau)**2 / g_pi)

        ! Convert to physical units [J/(kg K)]
        ! specific_gas_constant_water is in [kJ/(kg K)], so multiply by 1000
        cp = cp_dim * specific_gas_constant_water * 1000.0d0

    end function get_cp_iapws_region1

    !> Derivative of Cp w.r.t Temperature (dCp/dT)_P [J/(kg K^2)]
    module pure elemental function get_dcp_dt_iapws_region1(T_in, P_in) result(dcp_dt)
        implicit none
        real(real64), intent(in) :: T_in, P_in
        real(real64) :: dcp_dt

        real(real64) :: pi, tau
        real(real64) :: g_pi, g_tau, g_tautau, g_pitau
        real(real64) :: g_pitautau, g_tautautau
        real(real64) :: term1, term2, num, den, d_num_dtau, d_den_dtau
        real(real64) :: dcp_dim_dtau

        pi = (P_in * 1.0d-6) / p_star1
        tau = T_star1 / T_in

        ! Need derivatives up to 3rd order
        g_pi = get_gamma_pi_region1(pi, tau)
        g_tau = get_gamma_tau_region1(pi, tau)
        g_tautau = get_gamma_tautau_region1(pi, tau)
        g_pitau = get_gamma_pitau_region1(pi, tau)
        g_pitautau = get_gamma_pi_tautau_region1(pi, tau)
        g_tautautau = get_gamma_tautau_tau_region1(pi, tau)

        ! Differentiate dimensionless Cp/R w.r.t tau
        ! Cp/R = term1 + term2
        ! term1 = -tau^2 * g_tautau
        ! term2 = (g_tau - tau * g_pitau)^2 / g_pi

        ! d(term1)/dtau
        term1 = -2.0d0 * tau * g_tautau - tau**2 * g_tautautau

        ! d(term2)/dtau
        num = (g_tau - tau * g_pitau)**2
        den = g_pi

        ! d(num)/dtau = 2 * (inner) * d(inner)/dtau
        ! inner = g_tau - tau * g_pitau
        ! d(inner)/dtau = g_tautau - (g_pitau + tau * g_pitautau)
        d_num_dtau = 2.0d0 * (g_tau - tau * g_pitau) * &
                     (g_tautau - g_pitau - tau * g_pitautau)

        ! d(den)/dtau = g_pitau
        d_den_dtau = g_pitau

        term2 = (den * d_num_dtau - num * d_den_dtau) / (den**2)

        dcp_dim_dtau = term1 + term2

        ! Convert to physical derivative: dCp/dT = R * d(Cp/R)/dtau * dtau/dT
        ! dtau/dT = -tau/T
        dcp_dt = (specific_gas_constant_water * 1000.0d0) * dcp_dim_dtau * (-tau / T_in)

    end function get_dcp_dt_iapws_region1

    !> Derivative of Cp w.r.t Pressure (dCp/dP)_T [J/(kg K Pa)]
    module pure elemental function get_dcp_dp_iapws_region1(T_in, P_in) result(dcp_dp)
        implicit none
        real(real64), intent(in) :: T_in, P_in
        real(real64) :: dcp_dp

        real(real64) :: pi, tau
        real(real64) :: g_pi, g_tau, g_tautau, g_pitau, g_pipi
        real(real64) :: g_pitautau, g_pipitau
        real(real64) :: term1, term2, num, den, d_num_dpi, d_den_dpi
        real(real64) :: dcp_dim_dpi

        pi = (P_in * 1.0d-6) / p_star1
        tau = T_star1 / T_in

        ! Need derivatives
        g_pi = get_gamma_pi_region1(pi, tau)
        g_pipi = get_gamma_pipi_region1(pi, tau)
        g_tau = get_gamma_tau_region1(pi, tau)
        g_tautau = get_gamma_tautau_region1(pi, tau)
        g_pitau = get_gamma_pitau_region1(pi, tau)
        g_pitautau = get_gamma_pi_tautau_region1(pi, tau)
        g_pipitau = get_gamma_pipi_tau_region1(pi, tau)

        ! Differentiate dimensionless Cp/R w.r.t pi

        ! d(term1)/dpi
        ! term1 = -tau^2 * g_tautau
        term1 = -tau**2 * g_pitautau

        ! d(term2)/dpi
        num = (g_tau - tau * g_pitau)**2
        den = g_pi

        ! d(num)/dpi = 2 * (inner) * d(inner)/dpi
        ! d(inner)/dpi = g_pitau - tau * g_pipitau
        d_num_dpi = 2.0d0 * (g_tau - tau * g_pitau) * &
                    (g_pitau - tau * g_pipitau)

        ! d(den)/dpi = g_pipi
        d_den_dpi = g_pipi

        term2 = (den * d_num_dpi - num * d_den_dpi) / (den**2)

        dcp_dim_dpi = term1 + term2

        ! Convert to physical derivative: dCp/dP = R * d(Cp/R)/dpi * dpi/dP
        ! dpi/dP = 1 / (P_star [Pa]) = 1 / (p_star1 * 1.0d6)
        dcp_dp = (specific_gas_constant_water * 1000.0d0) * dcp_dim_dpi * &
                 (1.0d0 / (p_star1 * 1.0d6))

    end function get_dcp_dp_iapws_region1
end submodule iapws_specific_isobaric_heat
