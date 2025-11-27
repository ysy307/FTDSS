module physics_material_iapws
    use, intrinsic :: iso_fortran_env
    use :: module_core
    implicit none
    private

    public :: calc_p_boundary_iapws97_region23
    public :: calc_t_boundary_iapws97_region23
    public :: calc_nu_iapws97_region1
    public :: calc_nu_iapws97_region2
    public :: calc_nu_iapws97_region5
    public :: calc_p_iapws97_region3
    public :: calc_u_iapws97_region1
    public :: calc_u_iapws97_region2
    public :: calc_u_iapws97_region3
    public :: calc_u_iapws97_region5
    public :: calc_s_iapws97_region1
    public :: calc_s_iapws97_region2
    public :: calc_s_iapws97_region3
    public :: calc_s_iapws97_region5
    public :: calc_h_iapws97_region1
    public :: calc_h_iapws97_region2
    public :: calc_h_iapws97_region3
    public :: calc_h_iapws97_region5
    public :: calc_cp_iapws97_region1
    public :: calc_cp_iapws97_region2
    public :: calc_cp_iapws97_region3
    public :: calc_cp_iapws97_region5
    public :: calc_cv_iapws97_region1
    public :: calc_cv_iapws97_region2
    public :: calc_cv_iapws97_region3
    public :: calc_cv_iapws97_region5
    public :: calc_w_iapws97_region1
    public :: calc_w_iapws97_region2
    public :: calc_w_iapws97_region3
    public :: calc_w_iapws97_region5
    public :: calc_psat_iapws97_region4
    public :: calc_tsat_iapws97_region4

    public :: calc_nu_iapws06_Ih
    public :: calc_u_iapws06_Ih
    public :: calc_s_iapws06_Ih
    public :: calc_h_iapws06_Ih
    public :: calc_cp_iapws06_Ih
    public :: calc_alpha_iapws06_Ih
    public :: calc_beta_iapws06_Ih
    public :: calc_kappa_s_iapws06_Ih
    public :: calc_kappa_T_iapws06_Ih

    public :: calc_p_boundary_iapws08_iceIh_melting
    public :: calc_p_boundary_iapws08_iceIh_sublimation
    public :: calc_p_boundary_iapws08_iceIII_melting
    public :: calc_p_boundary_iapws08_iceV_melting
    public :: calc_p_boundary_iapws08_iceVI_melting
    public :: calc_p_boundary_iapws08_iceVII_melting

    !------------------------------------------------------------------------------------------
    ! Reigon1: Saturated liquid water (IAPWS-IF97)
    !------------------------------------------------------------------------------------------
    real(real64), parameter :: T_star1 = 1386.0d0 ! 基準温度 [K]
    real(real64), parameter :: p_star1 = 16.53d6 ! 基準圧力 [Pa]
    !------------------------------------------------------------------------------------------
    ! Reigon2: Superheated Steam (IAPWS-IF97)
    !------------------------------------------------------------------------------------------
    real(real64), parameter :: T_star2 = 540.0d0 ! 基準温度 [K]
    real(real64), parameter :: p_star2 = 1.0d6 ! 基準圧力 [Pa]
    !------------------------------------------------------------------------------------------
    ! Reigon3: High Pressure Liquid Water and Steam (IAPWS-IF97)
    !------------------------------------------------------------------------------------------
    real(real64), parameter :: T_star3 = water_critical_point_temperature ! 基準温度 [K]
    real(real64), parameter :: p_star3 = water_critical_point_pressure ! 基準圧力 [Pa]
    real(real64), parameter :: rho_star3 = water_critical_point_density ! 基準密度 [kg/m^3]
    !------------------------------------------------------------------------------------------
    ! Reigon4: Saturation curve between liquid and vapor (IAPWS-IF97)
    !------------------------------------------------------------------------------------------
    real(real64), parameter :: T_star4 = 1.0d0 ! 基準温度 [K]
    real(real64), parameter :: p_star4 = 1.0d6 ! 基準圧力 [Pa]
    !------------------------------------------------------------------------------------------
    ! Reigon5: High Temperature Steam (IAPWS-IF97)
    !------------------------------------------------------------------------------------------
    real(real64), parameter :: T_star5 = 1000.0d0 ! 基準温度 [K]
    real(real64), parameter :: p_star5 = 1.0d6 ! 基準圧力 [Pa]
    !------------------------------------------------------------------------------------------
    ! Ice Ih properties (IAPWS-06)
    !------------------------------------------------------------------------------------------
    real(real64), parameter :: T_starIh = water_triple_point_temperature ! 基準温度 [K]
    real(real64), parameter :: p_starIh = water_triple_point_pressure ! 基準圧力 [Pa]

    interface
        module pure elemental function calc_p_boundary_iapws97_region23(temperature) result(pressure)
            implicit none
            real(real64), intent(in) :: temperature
            real(real64) :: pressure

        end function calc_p_boundary_iapws97_region23

        module pure elemental function calc_t_boundary_iapws97_region23(pressure) result(temperature)
            implicit none
            real(real64), intent(in) :: pressure
            real(real64) :: temperature

        end function calc_t_boundary_iapws97_region23
    end interface

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
        module pure elemental function calc_gamma_p_region1(pi, tau) result(gamma_pi)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma_{\pi}\)
            real(real64) :: gamma_pi

        end function calc_gamma_p_region1

        !> Calculate the first derivative of \(\gamma\) with respect to \(\tau\).
        !> Computes \(\gamma_{\tau} = \left(\frac{\partial \gamma}{\partial \tau}\right)_{\pi}\).
        module pure elemental function calc_gamma_t_region1(pi, tau) result(gamma_tau)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma_{\tau}\)
            real(real64) :: gamma_tau

        end function calc_gamma_t_region1

        !> Calculate the second derivative of \(\gamma\) with respect to \(\pi\).
        !> Computes \(\gamma_{\pi\pi} = \left(\frac{\partial^2 \gamma}{\partial \pi^2}\right)_{\tau}\).
        module pure elemental function calc_gamma_pp_region1(pi, tau) result(gamma_pipi)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma_{\pi\pi}\)
            real(real64) :: gamma_pipi

        end function calc_gamma_pp_region1

        !> Calculate the second derivative of \(\gamma\) with respect to \(\tau\).
        !> Computes \(\gamma_{\tau\tau} = \left(\frac{\partial^2 \gamma}{\partial \tau^2}\right)_{\pi}\).
        module pure elemental function calc_gamma_tt_region1(pi, tau) result(gamma_tautau)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma_{\tau\tau}\)
            real(real64) :: gamma_tautau

        end function calc_gamma_tt_region1

        !> Calculate the mixed second derivative of \(\gamma\).
        !> Computes \(\gamma_{\pi\tau} = \frac{\partial^2 \gamma}{\partial \pi \partial \tau}\).
        module pure elemental function calc_gamma_pt_region1(pi, tau) result(gamma_pitau)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma_{\pi\tau}\)
            real(real64) :: gamma_pitau

        end function calc_gamma_pt_region1

        module pure elemental function calc_nu_iapws97_region1(T_in, P_in) result(nu)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: nu
        end function calc_nu_iapws97_region1

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

    interface
        module pure elemental function calc_gamma0_region2(pi, tau) result(gamma0)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Resulting \(\gamma^o\) value
            real(real64) :: gamma0

            integer(int32) :: i
            real(real64) :: term

        end function calc_gamma0_region2

        module pure elemental function calc_gamma0_p_region2(pi, tau) result(gamma0_p)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma^o_{\pi}\)
            real(real64) :: gamma0_p

        end function calc_gamma0_p_region2

        module pure elemental function calc_gamma0_pp_region2(pi, tau) result(gamma0_pp)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma^o_{\pi}\)
            real(real64) :: gamma0_pp

        end function calc_gamma0_pp_region2

        module pure elemental function calc_gamma0_t_region2(pi, tau) result(gamma0_t)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma^r_{\pi}\)
            real(real64) :: gamma0_t

        end function calc_gamma0_t_region2

        module pure elemental function calc_gamma0_tt_region2(pi, tau) result(gamma0_tt)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma^r_{\pi}\)
            real(real64) :: gamma0_tt

        end function calc_gamma0_tt_region2

        module pure elemental function calc_gamma0_pt_region2(pi, tau) result(gamma0_pt)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma^r_{\pi}\)
            real(real64) :: gamma0_pt

        end function calc_gamma0_pt_region2

        module pure elemental function calc_gammar_region2(pi, tau) result(gammar)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Resulting \(\gamma^r\) value
            real(real64) :: gammar

        end function calc_gammar_region2

        module pure elemental function calc_gammar_p_region2(pi, tau) result(gammar_p)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma^r_{\pi}\)
            real(real64) :: gammar_p

        end function calc_gammar_p_region2

        module pure elemental function calc_gammar_pp_region2(pi, tau) result(gammar_pp)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma^r_{\pi\pi}\)
            real(real64) :: gammar_pp

        end function calc_gammar_pp_region2

        module pure elemental function calc_gammar_t_region2(pi, tau) result(gammar_t)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma^r_{\tau}\)
            real(real64) :: gammar_t

        end function calc_gammar_t_region2

        module pure elemental function calc_gammar_tt_region2(pi, tau) result(gammar_tt)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma^r_{\tau\tau}\)
            real(real64) :: gammar_tt

        end function calc_gammar_tt_region2

        module pure elemental function calc_gammar_pt_region2(pi, tau) result(gammar_pt)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma^r_{\pi\tau}\)
            real(real64) :: gammar_pt

        end function calc_gammar_pt_region2

        module pure elemental function calc_nu_iapws97_region2(T_in, P_in) result(nu)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            !> Specific volume [m^3/kg]
            real(real64) :: nu

        end function calc_nu_iapws97_region2

        module pure elemental function calc_u_iapws97_region2(T_in, P_in) result(u)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            !> Specific internal energy [J/kg]
            real(real64) :: u

        end function calc_u_iapws97_region2

        module pure elemental function calc_s_iapws97_region2(T_in, P_in) result(s)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            !> Specific entropy [J/kg-K]
            real(real64) :: s

        end function calc_s_iapws97_region2

        module pure elemental function calc_h_iapws97_region2(T_in, P_in) result(h)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            !> Specific enthalpy [J/kg]
            real(real64) :: h

        end function calc_h_iapws97_region2

        module pure elemental function calc_cp_iapws97_region2(T_in, P_in) result(cp)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            !> Specific heat capacity at constant pressure [J/(kg K)]
            real(real64) :: cp

        end function calc_cp_iapws97_region2

        module pure elemental function calc_cv_iapws97_region2(T_in, P_in) result(cv)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            !> Specific heat capacity at constant volume [J/(kg K)]
            real(real64) :: cv

        end function calc_cv_iapws97_region2

        module pure elemental function calc_w_iapws97_region2(T_in, P_in) result(w)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            !> Speed of sound [m/s]
            real(real64) :: w

        end function calc_w_iapws97_region2

    end interface

    interface
        !> Calculate the dimensionless Helmholtz free energy \(\phi\) for Region 3.
        !> Formula: \(\phi = n_1 \ln \delta + \sum n_i \delta^{I_i} \tau^{J_i}\)
        module pure elemental function calc_phi_region3(delta, tau) result(phi)
            implicit none
            !> Dimensionless density \(\delta = \rho / \rho^*\)
            real(real64), intent(in) :: delta
            !> Inverse dimensionless temperature \(\tau = T^* / T\)
            real(real64), intent(in) :: tau
            !> Resulting \(\phi\) value
            real(real64) :: phi
        end function calc_phi_region3

        !> First derivative w.r.t. \(\delta\): \(\phi_{\delta} = (\partial \phi / \partial \delta)_{\tau}\)
        module pure elemental function calc_phi_d_region3(delta, tau) result(phi_d)
            implicit none
            real(real64), intent(in) :: delta
            real(real64), intent(in) :: tau
            real(real64) :: phi_d
        end function calc_phi_d_region3

        !> Second derivative w.r.t. \(\delta\): \(\phi_{\delta\delta} = (\partial^2 \phi / \partial \delta^2)_{\tau}\)
        module pure elemental function calc_phi_dd_region3(delta, tau) result(phi_dd)
            implicit none
            real(real64), intent(in) :: delta
            real(real64), intent(in) :: tau
            real(real64) :: phi_dd
        end function calc_phi_dd_region3

        !> First derivative w.r.t. \(\tau\): \(\phi_{\tau} = (\partial \phi / \partial \tau)_{\delta}\)
        module pure elemental function calc_phi_t_region3(delta, tau) result(phi_t)
            implicit none
            real(real64), intent(in) :: delta
            real(real64), intent(in) :: tau
            real(real64) :: phi_t
        end function calc_phi_t_region3

        !> Second derivative w.r.t. \(\tau\): \(\phi_{\tau\tau} = (\partial^2 \phi / \partial \tau^2)_{\delta}\)
        module pure elemental function calc_phi_tt_region3(delta, tau) result(phi_tt)
            implicit none
            real(real64), intent(in) :: delta
            real(real64), intent(in) :: tau
            real(real64) :: phi_tt
        end function calc_phi_tt_region3

        !> Mixed derivative: \(\phi_{\delta\tau} = \partial^2 \phi / (\partial \delta \partial \tau)\)
        module pure elemental function calc_phi_dt_region3(delta, tau) result(phi_dt)
            implicit none
            real(real64), intent(in) :: delta
            real(real64), intent(in) :: tau
            real(real64) :: phi_dt
        end function calc_phi_dt_region3

        module pure elemental function calc_p_iapws97_region3(T_in, rho_in) result(p)
            implicit none
            real(real64), intent(in) :: T_in
            !> Density [kg/m^3]
            real(real64), intent(in) :: rho_in
            real(real64) :: p

        end function calc_p_iapws97_region3

        module pure elemental function calc_u_iapws97_region3(T_in, rho_in) result(u)
            implicit none
            real(real64), intent(in) :: T_in
            !> Density [kg/m^3]
            real(real64), intent(in) :: rho_in
            !> Specific internal energy [J/kg]
            real(real64) :: u

        end function calc_u_iapws97_region3

        module pure elemental function calc_s_iapws97_region3(T_in, rho_in) result(s)
            implicit none
            real(real64), intent(in) :: T_in
            !> Density [kg/m^3]
            real(real64), intent(in) :: rho_in
            !> Specific entropy [J/kg-K]
            real(real64) :: s

        end function calc_s_iapws97_region3

        module pure elemental function calc_h_iapws97_region3(T_in, rho_in) result(h)
            implicit none
            real(real64), intent(in) :: T_in
            !> Density [kg/m^3]
            real(real64), intent(in) :: rho_in
            !> Specific enthalpy [J/kg]
            real(real64) :: h

        end function calc_h_iapws97_region3

        module pure elemental function calc_cp_iapws97_region3(T_in, rho_in) result(cp)
            implicit none
            real(real64), intent(in) :: T_in
            !> Density [kg/m^3]
            real(real64), intent(in) :: rho_in
            !> Specific heat capacity at constant pressure [J/kg-K]
            real(real64) :: cp

        end function calc_cp_iapws97_region3

        module pure elemental function calc_cv_iapws97_region3(T_in, rho_in) result(cv)
            implicit none
            real(real64), intent(in) :: T_in
            !> Density [kg/m^3]
            real(real64), intent(in) :: rho_in
            !> Specific heat capacity at constant volume [J/kg-K]
            real(real64) :: cv

        end function calc_cv_iapws97_region3

        module pure elemental function calc_w_iapws97_region3(T_in, rho_in) result(w)
            implicit none
            real(real64), intent(in) :: T_in
            !> Density [kg/m^3]
            real(real64), intent(in) :: rho_in
            !> Specific speed of sound [m/s]
            real(real64) :: w

        end function calc_w_iapws97_region3

        module pure elemental function calc_maxwell_residual_1(T_in, rho_liq, p_sat) result(res)
            implicit none
            real(real64), intent(in) :: T_in ! Temperature [K]
            real(real64), intent(in) :: rho_liq ! Saturated Liquid Density [kg/m^3] (rho')
            real(real64), intent(in) :: p_sat ! Saturation Pressure [Pa] (p_s)
            real(real64) :: res

        end function calc_maxwell_residual_1

        module pure elemental function calc_maxwell_residual_2(T_in, rho_vap, p_sat) result(res)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: rho_vap
            real(real64), intent(in) :: p_sat
            real(real64) :: res

        end function calc_maxwell_residual_2

        module pure elemental function calc_maxwell_residual_3(T_in, rho_liq, rho_vap, p_sat) result(res)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: rho_liq
            real(real64), intent(in) :: rho_vap
            real(real64), intent(in) :: p_sat
            real(real64) :: res

        end function calc_maxwell_residual_3
    end interface

    interface
        module pure elemental function calc_psat_iapws97_region4(temperature) result(P_sat)
            implicit none
            real(real64), intent(in) :: temperature
            real(real64) :: P_sat

        end function calc_psat_iapws97_region4

        module pure elemental function calc_tsat_iapws97_region4(pressure) result(T_sat)
            implicit none
            real(real64), intent(in) :: pressure
            real(real64) :: T_sat

        end function calc_tsat_iapws97_region4
    end interface

    interface
        module pure elemental function calc_gamma0_region5(pi, tau) result(gamma0)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gamma0

        end function calc_gamma0_region5

        module pure elemental function calc_gamma0_p_region5(pi, tau) result(gamma0_p)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gamma0_p
        end function calc_gamma0_p_region5

        module pure elemental function calc_gamma0_pp_region5(pi, tau) result(gamma0_pp)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gamma0_pp

        end function calc_gamma0_pp_region5

        module pure elemental function calc_gamma0_t_region5(pi, tau) result(gamma0_t)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gamma0_t

        end function calc_gamma0_t_region5

        module pure elemental function calc_gamma0_tt_region5(pi, tau) result(gamma0_tt)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gamma0_tt

        end function calc_gamma0_tt_region5

        module pure elemental function calc_gamma0_pt_region5(pi, tau) result(gamma0_pt)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gamma0_pt

        end function calc_gamma0_pt_region5

        module pure elemental function calc_gammar_region5(pi, tau) result(gammar)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gammar

        end function calc_gammar_region5

        module pure elemental function calc_gammar_p_region5(pi, tau) result(gammar_p)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gammar_p

        end function calc_gammar_p_region5

        module pure elemental function calc_gammar_pp_region5(pi, tau) result(gammar_pp)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gammar_pp

        end function calc_gammar_pp_region5

        module pure elemental function calc_gammar_t_region5(pi, tau) result(gammar_t)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gammar_t

        end function calc_gammar_t_region5

        module pure elemental function calc_gammar_tt_region5(pi, tau) result(gammar_tt)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gammar_tt

        end function calc_gammar_tt_region5

        module pure elemental function calc_gammar_pt_region5(pi, tau) result(gammar_pt)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gammar_pt

        end function calc_gammar_pt_region5

        module pure elemental function calc_nu_iapws97_region5(T_in, P_in) result(u)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: u

        end function calc_nu_iapws97_region5

        module pure elemental function calc_u_iapws97_region5(T_in, P_in) result(u)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: u

        end function calc_u_iapws97_region5

        module pure elemental function calc_s_iapws97_region5(T_in, P_in) result(s)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: s

        end function calc_s_iapws97_region5

        module pure elemental function calc_h_iapws97_region5(T_in, P_in) result(h)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: h

        end function calc_h_iapws97_region5

        module pure elemental function calc_cp_iapws97_region5(T_in, P_in) result(cp)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: cp

        end function calc_cp_iapws97_region5

        module pure elemental function calc_cv_iapws97_region5(T_in, P_in) result(cv)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: cv

        end function calc_cv_iapws97_region5

        module pure elemental function calc_w_iapws97_region5(T_in, P_in) result(w)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: w

        end function calc_w_iapws97_region5
    end interface

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

    interface
        module pure elemental function calc_p_boundary_iapws08_iceIh_melting(T_in) result(p)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64) :: p

        end function calc_p_boundary_iapws08_iceIh_melting

        module pure elemental function calc_p_boundary_iapws08_iceIh_sublimation(T_in) result(p)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64) :: p

        end function calc_p_boundary_iapws08_iceIh_sublimation

        module pure elemental function calc_p_boundary_iapws08_iceIII_melting(T_in) result(p)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64) :: p

        end function calc_p_boundary_iapws08_iceIII_melting

        module pure elemental function calc_p_boundary_iapws08_iceV_melting(T_in) result(p)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64) :: p

        end function calc_p_boundary_iapws08_iceV_melting

        module pure elemental function calc_p_boundary_iapws08_iceVI_melting(T_in) result(p)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64) :: p

        end function calc_p_boundary_iapws08_iceVI_melting

        module pure elemental function calc_p_boundary_iapws08_iceVII_melting(T_in) result(p)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64) :: p
        end function calc_p_boundary_iapws08_iceVII_melting
    end interface
end module physics_material_iapws
