module physics_material_iapws06_IceIh
    use, intrinsic :: iso_fortran_env
    use :: physics_constants, only:P_0 => standard_atmospheric_pressure
    use :: physics_material_iapws_constants, only:p_starIh, T_starIh
    implicit none
    private

    public :: calc_nu_iapws06_Ih
    public :: calc_rho_iapws06_Ih
    public :: calc_u_iapws06_Ih
    public :: calc_s_iapws06_Ih
    public :: calc_h_iapws06_Ih
    public :: calc_cp_iapws06_Ih
    public :: calc_alpha_iapws06_Ih
    public :: calc_beta_iapws06_Ih
    public :: calc_kappa_s_iapws06_Ih
    public :: calc_kappa_T_iapws06_Ih

    interface

        !---------------------------------------------------------------------------
        ! Gibbs Energy: g(T,p) [Eq 1]
        !---------------------------------------------------------------------------
        module pure elemental function calc_gamma_iapws06_Ih(pi, tau) result(gamma)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gamma

        end function calc_gamma_iapws06_Ih

        module pure elemental function calc_gamma_t_iapws06_Ih(pi, tau) result(gamma_t)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gamma_t

        end function calc_gamma_t_iapws06_Ih

        module pure elemental function calc_gamma_p_iapws06_Ih(pi, tau) result(gamma_p)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gamma_p

        end function calc_gamma_p_iapws06_Ih

        module pure elemental function calc_gamma_tt_iapws06_Ih(pi, tau) result(gamma_tt)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gamma_tt

        end function calc_gamma_tt_iapws06_Ih

        module pure elemental function calc_gamma_tp_iapws06_Ih(pi, tau) result(gamma_tp)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gamma_tp

        end function calc_gamma_tp_iapws06_Ih

        module pure elemental function calc_gamma_pp_iapws06_Ih(pi, tau) result(gamma_pp)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gamma_pp

        end function calc_gamma_pp_iapws06_Ih

        module pure elemental function calc_nu_iapws06_Ih(T_in, P_in) result(nu)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: nu
        end function calc_nu_iapws06_Ih

        module pure elemental function calc_rho_iapws06_Ih(T_in, P_in) result(rho)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: rho
        end function calc_rho_iapws06_Ih

        module pure elemental function calc_u_iapws06_Ih(T_in, P_in) result(u)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: u
        end function calc_u_iapws06_Ih

        module pure elemental function calc_s_iapws06_Ih(T_in, P_in) result(s)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: s
        end function calc_s_iapws06_Ih

        module pure elemental function calc_h_iapws06_Ih(T_in, P_in) result(h)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: h

        end function calc_h_iapws06_Ih

        module pure elemental function calc_cp_iapws06_Ih(T_in, P_in) result(cp)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: cp

        end function calc_cp_iapws06_Ih

        module pure elemental function calc_alpha_iapws06_Ih(T_in, P_in) result(alpha)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: alpha

        end function calc_alpha_iapws06_Ih

        module pure elemental function calc_beta_iapws06_Ih(T_in, P_in) result(beta)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: beta

        end function calc_beta_iapws06_Ih

        module pure elemental function calc_kappa_T_iapws06_Ih(T_in, P_in) result(kappa_T)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: kappa_T

        end function calc_kappa_T_iapws06_Ih

        module pure elemental function calc_kappa_s_iapws06_Ih(T_in, P_in) result(kappa_T)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: kappa_T

        end function calc_kappa_s_iapws06_Ih

    end interface

end module physics_material_iapws06_IceIh
