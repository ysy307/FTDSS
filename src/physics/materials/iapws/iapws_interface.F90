module physics_material_iapws
    use, intrinsic :: iso_fortran_env
    use :: module_core
    implicit none
    private
    public :: get_boundary_pressure_region23
    public :: get_boundary_temperature_region23
    public :: get_nu_iapws97_region1
    public :: get_nu_iapws97_region2
    public :: get_p_iapws97_region3
    public :: get_u_iapws97_region1
    public :: get_u_iapws97_region2
    public :: get_u_iapws97_region3
    public :: get_s_iapws97_region1
    public :: get_s_iapws97_region2
    public :: get_s_iapws97_region3
    public :: get_h_iapws97_region1
    public :: get_h_iapws97_region2
    public :: get_h_iapws97_region3
    public :: get_cp_iapws97_region1
    public :: get_cp_iapws97_region2
    public :: get_cp_iapws97_region3
    public :: get_cv_iapws97_region1
    public :: get_cv_iapws97_region2
    public :: get_cv_iapws97_region3
    public :: get_w_iapws97_region1
    public :: get_w_iapws97_region2
    public :: get_w_iapws97_region3
    public :: get_sat_pressure_region4
    public :: get_sat_temperature_region4

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
    real(real64), parameter :: p_star5 = 10.0d6 ! 基準圧力 [Pa]

    interface
        module pure elemental function get_boundary_pressure_region23(temperature) result(pressure)
            implicit none
            real(real64), intent(in) :: temperature
            real(real64) :: pressure

        end function get_boundary_pressure_region23

        module pure elemental function get_boundary_temperature_region23(pressure) result(temperature)
            implicit none
            real(real64), intent(in) :: pressure
            real(real64) :: temperature

        end function get_boundary_temperature_region23
    end interface

    interface
        !> Calculate the dimensionless Gibbs free energy \(\gamma\) for Region 1.
        !> Formula: \(\gamma = \sum n_i (7.1 - \pi)^{I_i} (\tau - 1.222)^{J_i}\)
        module pure elemental function get_gamma_region1(pi, tau) result(gamma)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Resulting \(\gamma\) value
            real(real64) :: gamma

        end function get_gamma_region1

        !> Calculate the first derivative of \(\gamma\) with respect to \(\pi\).
        !> Computes \(\gamma_{\pi} = \left(\frac{\partial \gamma}{\partial \pi}\right)_{\tau}\).
        module pure elemental function get_gamma_pi_region1(pi, tau) result(gamma_pi)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma_{\pi}\)
            real(real64) :: gamma_pi

        end function get_gamma_pi_region1

        !> Calculate the first derivative of \(\gamma\) with respect to \(\tau\).
        !> Computes \(\gamma_{\tau} = \left(\frac{\partial \gamma}{\partial \tau}\right)_{\pi}\).
        module pure elemental function get_gamma_tau_region1(pi, tau) result(gamma_tau)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma_{\tau}\)
            real(real64) :: gamma_tau

        end function get_gamma_tau_region1

        !> Calculate the second derivative of \(\gamma\) with respect to \(\pi\).
        !> Computes \(\gamma_{\pi\pi} = \left(\frac{\partial^2 \gamma}{\partial \pi^2}\right)_{\tau}\).
        module pure elemental function get_gamma_pipi_region1(pi, tau) result(gamma_pipi)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma_{\pi\pi}\)
            real(real64) :: gamma_pipi

        end function get_gamma_pipi_region1

        !> Calculate the second derivative of \(\gamma\) with respect to \(\tau\).
        !> Computes \(\gamma_{\tau\tau} = \left(\frac{\partial^2 \gamma}{\partial \tau^2}\right)_{\pi}\).
        module pure elemental function get_gamma_tautau_region1(pi, tau) result(gamma_tautau)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma_{\tau\tau}\)
            real(real64) :: gamma_tautau

        end function get_gamma_tautau_region1

        !> Calculate the mixed second derivative of \(\gamma\).
        !> Computes \(\gamma_{\pi\tau} = \frac{\partial^2 \gamma}{\partial \pi \partial \tau}\).
        module pure elemental function get_gamma_pitau_region1(pi, tau) result(gamma_pitau)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma_{\pi\tau}\)
            real(real64) :: gamma_pitau

        end function get_gamma_pitau_region1

        module pure elemental function get_nu_iapws97_region1(T_in, P_in) result(nu)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: nu
        end function get_nu_iapws97_region1

        module pure elemental function get_u_iapws97_region1(T_in, P_in) result(u)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: u

        end function get_u_iapws97_region1

        module pure elemental function get_s_iapws97_region1(T_in, P_in) result(s)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: s

        end function get_s_iapws97_region1

        module pure elemental function get_h_iapws97_region1(T_in, P_in) result(h)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: h
        end function get_h_iapws97_region1

        module pure elemental function get_cp_iapws97_region1(T_in, P_in) result(cp)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: cp
        end function get_cp_iapws97_region1

        module pure elemental function get_cv_iapws97_region1(T_in, P_in) result(cv)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: cv

        end function get_cv_iapws97_region1

        module pure elemental function get_w_iapws97_region1(T_in, P_in) result(w)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: w

        end function get_w_iapws97_region1

    end interface

    interface
        module pure elemental function get_gamma0_region2(pi, tau) result(gamma0)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Resulting \(\gamma^o\) value
            real(real64) :: gamma0

            integer(int32) :: i
            real(real64) :: term

        end function get_gamma0_region2

        module pure elemental function get_gamma0_pi_region2(pi, tau) result(gamma0_p)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma^o_{\pi}\)
            real(real64) :: gamma0_p

        end function get_gamma0_pi_region2

        module pure elemental function get_gamma0_pipi_region2(pi, tau) result(gamma0_pp)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma^o_{\pi}\)
            real(real64) :: gamma0_pp

        end function get_gamma0_pipi_region2

        module pure elemental function get_gamma0_tau_region2(pi, tau) result(gamma0_t)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma^r_{\pi}\)
            real(real64) :: gamma0_t

        end function get_gamma0_tau_region2

        module pure elemental function get_gamma0_tautau_region2(pi, tau) result(gamma0_tt)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma^r_{\pi}\)
            real(real64) :: gamma0_tt

        end function get_gamma0_tautau_region2

        module pure elemental function get_gamma0_pitau_region2(pi, tau) result(gamma0_pt)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma^r_{\pi}\)
            real(real64) :: gamma0_pt

        end function get_gamma0_pitau_region2

        module pure elemental function get_gammar_region2(pi, tau) result(gammar)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Resulting \(\gamma^r\) value
            real(real64) :: gammar

        end function get_gammar_region2

        module pure elemental function get_gammar_pi_region2(pi, tau) result(gammar_p)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma^r_{\pi}\)
            real(real64) :: gammar_p

        end function get_gammar_pi_region2

        module pure elemental function get_gammar_pipi_region2(pi, tau) result(gammar_pp)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma^r_{\pi\pi}\)
            real(real64) :: gammar_pp

        end function get_gammar_pipi_region2

        module pure elemental function get_gammar_tau_region2(pi, tau) result(gammar_t)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma^r_{\tau}\)
            real(real64) :: gammar_t

        end function get_gammar_tau_region2

        module pure elemental function get_gammar_tautau_region2(pi, tau) result(gammar_tt)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma^r_{\tau\tau}\)
            real(real64) :: gammar_tt

        end function get_gammar_tautau_region2

        module pure elemental function get_gammar_pitau_region2(pi, tau) result(gammar_pt)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma^r_{\pi\tau}\)
            real(real64) :: gammar_pt

        end function get_gammar_pitau_region2

        module pure elemental function get_nu_iapws97_region2(T_in, P_in) result(nu)
            implicit none
            !> Temperature [K]
            real(real64), intent(in) :: T_in
            !> Pressure [Pa]
            real(real64), intent(in) :: P_in
            !> Specific volume [m^3/kg]
            real(real64) :: nu

        end function get_nu_iapws97_region2

        module pure elemental function get_u_iapws97_region2(T_in, P_in) result(u)
            implicit none
            !> Temperature [K]
            real(real64), intent(in) :: T_in
            !> Pressure [Pa]
            real(real64), intent(in) :: P_in
            !> Specific internal energy [J/kg]
            real(real64) :: u

        end function get_u_iapws97_region2

        module pure elemental function get_s_iapws97_region2(T_in, P_in) result(s)
            implicit none
            !> Temperature [K]
            real(real64), intent(in) :: T_in
            !> Pressure [Pa]
            real(real64), intent(in) :: P_in
            !> Specific entropy [J/kg-K]
            real(real64) :: s

        end function get_s_iapws97_region2

        module pure elemental function get_h_iapws97_region2(T_in, P_in) result(h)
            implicit none
            !> Temperature [K]
            real(real64), intent(in) :: T_in
            !> Pressure [Pa]
            real(real64), intent(in) :: P_in
            !> Specific enthalpy [J/kg]
            real(real64) :: h

        end function get_h_iapws97_region2

        module pure elemental function get_cp_iapws97_region2(T_in, P_in) result(cp)
            implicit none
            !> Temperature [K]
            real(real64), intent(in) :: T_in
            !> Pressure [Pa]
            real(real64), intent(in) :: P_in
            !> Specific heat capacity at constant pressure [J/(kg K)]
            real(real64) :: cp

        end function get_cp_iapws97_region2

        module pure elemental function get_cv_iapws97_region2(T_in, P_in) result(cv)
            implicit none
            !> Temperature [K]
            real(real64), intent(in) :: T_in
            !> Pressure [Pa]
            real(real64), intent(in) :: P_in
            !> Specific heat capacity at constant volume [J/(kg K)]
            real(real64) :: cv

        end function get_cv_iapws97_region2

        module pure elemental function get_w_iapws97_region2(T_in, P_in) result(w)
            implicit none
            !> Temperature [K]
            real(real64), intent(in) :: T_in
            !> Pressure [Pa]
            real(real64), intent(in) :: P_in
            !> Speed of sound [m/s]
            real(real64) :: w

        end function get_w_iapws97_region2

    end interface

    interface
        !> Calculate the dimensionless Helmholtz free energy \(\phi\) for Region 3.
        !> Formula: \(\phi = n_1 \ln \delta + \sum n_i \delta^{I_i} \tau^{J_i}\)
        module pure elemental function get_phi_region3(delta, tau) result(phi)
            implicit none
            !> Dimensionless density \(\delta = \rho / \rho^*\)
            real(real64), intent(in) :: delta
            !> Inverse dimensionless temperature \(\tau = T^* / T\)
            real(real64), intent(in) :: tau
            !> Resulting \(\phi\) value
            real(real64) :: phi
        end function get_phi_region3

        !> First derivative w.r.t. \(\delta\): \(\phi_{\delta} = (\partial \phi / \partial \delta)_{\tau}\)
        module pure elemental function get_phi_delta_region3(delta, tau) result(phi_d)
            implicit none
            real(real64), intent(in) :: delta
            real(real64), intent(in) :: tau
            real(real64) :: phi_d
        end function get_phi_delta_region3

        !> Second derivative w.r.t. \(\delta\): \(\phi_{\delta\delta} = (\partial^2 \phi / \partial \delta^2)_{\tau}\)
        module pure elemental function get_phi_deltadelta_region3(delta, tau) result(phi_dd)
            implicit none
            real(real64), intent(in) :: delta
            real(real64), intent(in) :: tau
            real(real64) :: phi_dd
        end function get_phi_deltadelta_region3

        !> First derivative w.r.t. \(\tau\): \(\phi_{\tau} = (\partial \phi / \partial \tau)_{\delta}\)
        module pure elemental function get_phi_tau_region3(delta, tau) result(phi_t)
            implicit none
            real(real64), intent(in) :: delta
            real(real64), intent(in) :: tau
            real(real64) :: phi_t
        end function get_phi_tau_region3

        !> Second derivative w.r.t. \(\tau\): \(\phi_{\tau\tau} = (\partial^2 \phi / \partial \tau^2)_{\delta}\)
        module pure elemental function get_phi_tautau_region3(delta, tau) result(phi_tt)
            implicit none
            real(real64), intent(in) :: delta
            real(real64), intent(in) :: tau
            real(real64) :: phi_tt
        end function get_phi_tautau_region3

        !> Mixed derivative: \(\phi_{\delta\tau} = \partial^2 \phi / (\partial \delta \partial \tau)\)
        module pure elemental function get_phi_deltatau_region3(delta, tau) result(phi_dt)
            implicit none
            real(real64), intent(in) :: delta
            real(real64), intent(in) :: tau
            real(real64) :: phi_dt
        end function get_phi_deltatau_region3

        module pure elemental function get_p_iapws97_region3(T_in, rho_in) result(p)
            implicit none
            !> Temperature [K]
            real(real64), intent(in) :: T_in
            !> Density [kg/m^3]
            real(real64), intent(in) :: rho_in
            !> Pressure [Pa]
            real(real64) :: p

        end function get_p_iapws97_region3

        module pure elemental function get_u_iapws97_region3(T_in, rho_in) result(u)
            implicit none
            !> Temperature [K]
            real(real64), intent(in) :: T_in
            !> Density [kg/m^3]
            real(real64), intent(in) :: rho_in
            !> Specific internal energy [J/kg]
            real(real64) :: u

        end function get_u_iapws97_region3

        module pure elemental function get_s_iapws97_region3(T_in, rho_in) result(s)
            implicit none
            !> Temperature [K]
            real(real64), intent(in) :: T_in
            !> Density [kg/m^3]
            real(real64), intent(in) :: rho_in
            !> Specific entropy [J/kg-K]
            real(real64) :: s

        end function get_s_iapws97_region3

        module pure elemental function get_h_iapws97_region3(T_in, rho_in) result(h)
            implicit none
            !> Temperature [K]
            real(real64), intent(in) :: T_in
            !> Density [kg/m^3]
            real(real64), intent(in) :: rho_in
            !> Specific enthalpy [J/kg]
            real(real64) :: h

        end function get_h_iapws97_region3

        module pure elemental function get_cp_iapws97_region3(T_in, rho_in) result(cp)
            implicit none
            !> Temperature [K]
            real(real64), intent(in) :: T_in
            !> Density [kg/m^3]
            real(real64), intent(in) :: rho_in
            !> Specific heat capacity at constant pressure [J/kg-K]
            real(real64) :: cp

        end function get_cp_iapws97_region3

        module pure elemental function get_cv_iapws97_region3(T_in, rho_in) result(cv)
            implicit none
            !> Temperature [K]
            real(real64), intent(in) :: T_in
            !> Density [kg/m^3]
            real(real64), intent(in) :: rho_in
            !> Specific heat capacity at constant volume [J/kg-K]
            real(real64) :: cv

        end function get_cv_iapws97_region3

        module pure elemental function get_w_iapws97_region3(T_in, rho_in) result(w)
            implicit none
            !> Temperature [K]
            real(real64), intent(in) :: T_in
            !> Density [kg/m^3]
            real(real64), intent(in) :: rho_in
            !> Specific speed of sound [m/s]
            real(real64) :: w

        end function get_w_iapws97_region3
    end interface

    interface
        module pure elemental function get_sat_pressure_region4(temperature) result(P_sat)
            implicit none
            real(real64), intent(in) :: temperature
            real(real64) :: P_sat

        end function get_sat_pressure_region4

        module pure elemental function get_sat_temperature_region4(pressure) result(T_sat)
            implicit none
            real(real64), intent(in) :: pressure
            real(real64) :: T_sat

        end function get_sat_temperature_region4
    end interface

    interface
        module pure elemental function get_gamma0_region5(pi, tau) result(gamma0)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gamma0

        end function get_gamma0_region5

        module pure elemental function get_gamma0_pi_region5(pi, tau) result(gamma0_p)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gamma0_p
        end function get_gamma0_pi_region5

        module pure elemental function get_gamma0_pipi_region5(pi, tau) result(gamma0_pp)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gamma0_pp

        end function get_gamma0_pipi_region5

        module pure elemental function get_gamma0_tau_region5(pi, tau) result(gamma0_t)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gamma0_t

        end function get_gamma0_tau_region5

        module pure elemental function get_gamma0_tautau_region5(pi, tau) result(gamma0_tt)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gamma0_tt

        end function get_gamma0_tautau_region5

        module pure elemental function get_gamma0_pitau_region5(pi, tau) result(gamma0_pt)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gamma0_pt

        end function get_gamma0_pitau_region5

        module pure elemental function get_gammar_region5(pi, tau) result(gammar)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gammar

        end function get_gammar_region5

        module pure elemental function get_gammar_pi_region5(pi, tau) result(gammar_p)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gammar_p

        end function get_gammar_pi_region5

        module pure elemental function get_gammar_pipi_region5(pi, tau) result(gammar_pp)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gammar_pp

        end function get_gammar_pipi_region5

        module pure elemental function get_gammar_tau_region5(pi, tau) result(gammar_t)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gammar_t

        end function get_gammar_tau_region5

        module pure elemental function get_gammar_tautau_region5(pi, tau) result(gammar_tt)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gammar_tt

        end function get_gammar_tautau_region5

        module pure elemental function get_gammar_pitau_region5(pi, tau) result(gammar_pt)
            implicit none
            real(real64), intent(in) :: pi
            real(real64), intent(in) :: tau
            real(real64) :: gammar_pt

        end function get_gammar_pitau_region5
    end interface

contains

end module physics_material_iapws
