module physics_material_iapws97_region1
    use, intrinsic :: iso_fortran_env
    use :: physics_constants, only:R_w => specific_gas_constant_water
    use :: physics_material_iapws_constants, only:T_star1, p_star1
    implicit none
    private

    public :: calc_nu_iapws97_region1
    public :: calc_rho_iapws97_region1
    public :: calc_u_iapws97_region1
    public :: calc_s_iapws97_region1
    public :: calc_h_iapws97_region1
    public :: calc_cp_iapws97_region1
    public :: calc_cv_iapws97_region1
    public :: calc_w_iapws97_region1

    interface
        !> Calculate the dimensionless Gibbs free energy \(\gamma\) for Region 1.
        !> Formula: \(\gamma = \sum n_i (7.1 - \pi)^{I_i} (\tau - 1.222)^{J_i}\)
        module pure elemental function calc_gamma_region1(pi, tau) result(gamma)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Resulting \(\gamma\) value
            real(real64) :: gamma

        end function calc_gamma_region1

        !> Calculate the first derivative of \(\gamma\) with respect to \(\pi\).
        !> Computes \(\gamma_{\pi} = \left(\frac{\partial \gamma}{\partial \pi}\right)_{\tau}\).
        module pure elemental function calc_gamma_p_region1(pi, tau) result(gamma_p)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma_{\pi}\)
            real(real64) :: gamma_p

        end function calc_gamma_p_region1

        !> Calculate the first derivative of \(\gamma\) with respect to \(\tau\).
        !> Computes \(\gamma_{\tau} = \left(\frac{\partial \gamma}{\partial \tau}\right)_{\pi}\).
        module pure elemental function calc_gamma_t_region1(pi, tau) result(gamma_t)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma_{\tau}\)
            real(real64) :: gamma_t

        end function calc_gamma_t_region1

        !> Calculate the second derivative of \(\gamma\) with respect to \(\pi\).
        !> Computes \(\gamma_{\pi\pi} = \left(\frac{\partial^2 \gamma}{\partial \pi^2}\right)_{\tau}\).
        module pure elemental function calc_gamma_pp_region1(pi, tau) result(gamma_pp)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma_{\pi\pi}\)
            real(real64) :: gamma_pp

        end function calc_gamma_pp_region1

        !> Calculate the second derivative of \(\gamma\) with respect to \(\tau\).
        !> Computes \(\gamma_{\tau\tau} = \left(\frac{\partial^2 \gamma}{\partial \tau^2}\right)_{\pi}\).
        module pure elemental function calc_gamma_tt_region1(pi, tau) result(gamma_tt)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma_{\tau\tau}\)
            real(real64) :: gamma_tt

        end function calc_gamma_tt_region1

        !> Calculate the mixed second derivative of \(\gamma\).
        !> Computes \(\gamma_{\pi\tau} = \frac{\partial^2 \gamma}{\partial \pi \partial \tau}\).
        module pure elemental function calc_gamma_pt_region1(pi, tau) result(gamma_pt)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma_{\pi\tau}\)
            real(real64) :: gamma_pt

        end function calc_gamma_pt_region1

        module pure elemental function calc_nu_iapws97_region1(T_in, P_in) result(nu)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: nu
        end function calc_nu_iapws97_region1

        module pure elemental function calc_rho_iapws97_region1(T_in, P_in) result(rho)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: rho
        end function calc_rho_iapws97_region1

        module pure elemental function calc_u_iapws97_region1(T_in, P_in) result(u)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: u

        end function calc_u_iapws97_region1

        module pure elemental function calc_s_iapws97_region1(T_in, P_in) result(s)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: s

        end function calc_s_iapws97_region1

        module pure elemental function calc_h_iapws97_region1(T_in, P_in) result(h)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: h
        end function calc_h_iapws97_region1

        module pure elemental function calc_cp_iapws97_region1(T_in, P_in) result(cp)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: cp
        end function calc_cp_iapws97_region1

        module pure elemental function calc_cv_iapws97_region1(T_in, P_in) result(cv)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: cv

        end function calc_cv_iapws97_region1

        module pure elemental function calc_w_iapws97_region1(T_in, P_in) result(w)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: w

        end function calc_w_iapws97_region1

    end interface

end module physics_material_iapws97_region1
