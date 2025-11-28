module physics_material_iapws97_region2
    use, intrinsic :: iso_fortran_env
    use :: physics_constants, only:R_w => specific_gas_constant_water
    use :: physics_material_iapws_constants, only:T_star2, p_star2
    implicit none
    private

    public :: calc_nu_iapws97_region2
    public :: calc_rho_iapws97_region2
    public :: calc_u_iapws97_region2
    public :: calc_s_iapws97_region2
    public :: calc_h_iapws97_region2
    public :: calc_cp_iapws97_region2
    public :: calc_cv_iapws97_region2
    public :: calc_w_iapws97_region2

    interface
        module pure elemental function calc_gamma0_region2(pi, tau) result(gamma0)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gamma0

        end function calc_gamma0_region2

        module pure elemental function calc_gamma0_p_region2(pi, tau) result(gamma0_p)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gamma0_p

        end function calc_gamma0_p_region2

        module pure elemental function calc_gamma0_pp_region2(pi, tau) result(gamma0_pp)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gamma0_pp

        end function calc_gamma0_pp_region2

        module pure elemental function calc_gamma0_t_region2(pi, tau) result(gamma0_t)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gamma0_t

        end function calc_gamma0_t_region2

        module pure elemental function calc_gamma0_tt_region2(pi, tau) result(gamma0_tt)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma^r_{\pi}\)
            real(real64) :: gamma0_tt

        end function calc_gamma0_tt_region2

        module pure elemental function calc_gamma0_pt_region2(pi, tau) result(gamma0_pt)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gamma0_pt

        end function calc_gamma0_pt_region2

        module pure elemental function calc_gammar_region2(pi, tau) result(gammar)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gammar

        end function calc_gammar_region2

        module pure elemental function calc_gammar_p_region2(pi, tau) result(gammar_p)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gammar_p

        end function calc_gammar_p_region2

        module pure elemental function calc_gammar_pp_region2(pi, tau) result(gammar_pp)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gammar_pp

        end function calc_gammar_pp_region2

        module pure elemental function calc_gammar_t_region2(pi, tau) result(gammar_t)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gammar_t

        end function calc_gammar_t_region2

        module pure elemental function calc_gammar_tt_region2(pi, tau) result(gammar_tt)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gammar_tt

        end function calc_gammar_tt_region2

        module pure elemental function calc_gammar_pt_region2(pi, tau) result(gammar_pt)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gammar_pt

        end function calc_gammar_pt_region2

        module pure elemental function calc_nu_iapws97_region2(T_in, P_in) result(nu)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: nu

        end function calc_nu_iapws97_region2

        module pure elemental function calc_rho_iapws97_region2(T_in, P_in) result(rho)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: rho
        end function calc_rho_iapws97_region2

        module pure elemental function calc_u_iapws97_region2(T_in, P_in) result(u)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: u

        end function calc_u_iapws97_region2

        module pure elemental function calc_s_iapws97_region2(T_in, P_in) result(s)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: s

        end function calc_s_iapws97_region2

        module pure elemental function calc_h_iapws97_region2(T_in, P_in) result(h)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: h

        end function calc_h_iapws97_region2

        module pure elemental function calc_cp_iapws97_region2(T_in, P_in) result(cp)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: cp

        end function calc_cp_iapws97_region2

        module pure elemental function calc_cv_iapws97_region2(T_in, P_in) result(cv)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: cv

        end function calc_cv_iapws97_region2

        module pure elemental function calc_w_iapws97_region2(T_in, P_in) result(w)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: w

        end function calc_w_iapws97_region2

    end interface

end module physics_material_iapws97_region2
