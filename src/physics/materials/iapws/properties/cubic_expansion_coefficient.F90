submodule(physics_material_iapws) iapws_cubic_expansion_coefficient
    implicit none
contains

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

end submodule iapws_cubic_expansion_coefficient
