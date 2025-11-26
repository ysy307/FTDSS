submodule(physics_material_iapws) iapws_specific_pressure
    implicit none
contains
    module pure elemental function get_p_iapws97_region3(T_in, rho_in) result(p)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Density [kg/m^3]
        real(real64), intent(in) :: rho_in
        !> Pressure [Pa]
        real(real64) :: p

        real(real64) :: delta, tau
        real(real64) :: phi_delta

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        delta = rho_in / rho_star3
        tau = T_star3 / T_in

        phi_delta = get_phi_delta_region3(delta, tau)

        ! ==========================================================
        ! Calculate Pressure [Pa]
        ! ==========================================================
        p = rho_in * specific_gas_constant_water * T_in * delta * phi_delta

    end function get_p_iapws97_region3

end submodule iapws_specific_pressure
