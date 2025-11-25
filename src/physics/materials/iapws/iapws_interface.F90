module physics_material_iapws
    use, intrinsic :: iso_fortran_env
    use :: module_core
    implicit none
    private

    !------------------------------------------------------------------------------------------
    ! Reigon1: Saturated liquid water (IAPWS-IF97)
    !------------------------------------------------------------------------------------------
    real(real64), parameter :: T_star1 = 1386.0d0 ! 基準温度 [K]
    real(real64), parameter :: p_star1 = 16.53d0 ! 基準圧力 [MPa]
    !------------------------------------------------------------------------------------------
    ! Reigon2: Superheated Steam (IAPWS-IF97)
    !------------------------------------------------------------------------------------------
    real(real64), parameter :: T_star2 = 540.0d0 ! 基準温度 [K]
    real(real64), parameter :: p_star2 = 1.0d0 ! 基準圧力 [MPa]

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

        !> Calculate the third derivative \(\gamma_{\pi\pi\tau}\).
        !> Computes \(\frac{\partial^3 \gamma}{\partial \pi^2 \partial \tau}\).
        module pure elemental function get_gamma_pipi_tau_region1(pi, tau) result(val)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma_{\pi\pi\tau}\)
            real(real64) :: val

        end function get_gamma_pipi_tau_region1

        !> Calculate the third derivative \(\gamma_{\pi\tau\tau}\).
        !> Computes \(\frac{\partial^3 \gamma}{\partial \pi \partial \tau^2}\).
        module pure elemental function get_gamma_pi_tautau_region1(pi, tau) result(val)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma_{\pi\tau\tau}\)
            real(real64) :: val

        end function get_gamma_pi_tautau_region1

        !> Calculate the third derivative \(\gamma_{\tau\tau\tau}\).
        !> Computes \(\frac{\partial^3 \gamma}{\partial \tau^3}\).
        module pure elemental function get_gamma_tautau_tau_region1(pi, tau) result(val)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma_{\tau\tau\tau}\)
            real(real64) :: val

        end function get_gamma_tautau_tau_region1

        module pure elemental function get_density_iapws_region1(T_in, P_in) result(rho)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: rho
        end function get_density_iapws_region1

        module pure elemental function get_drho_dt_iapws_region1(T_in, P_in) result(drho_dt)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: drho_dt
        end function get_drho_dt_iapws_region1

        module pure elemental function get_drho_dp_iapws_region1(T_in, P_in) result(drho_dp)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: drho_dp
        end function get_drho_dp_iapws_region1

        module pure elemental function get_cp_iapws_region1(T_in, P_in) result(cp)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: cp
        end function get_cp_iapws_region1

        module pure elemental function get_dcp_dt_iapws_region1(T_in, P_in) result(dcp_dt)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: dcp_dt
        end function get_dcp_dt_iapws_region1

        module pure elemental function get_dcp_dp_iapws_region1(T_in, P_in) result(dcp_dp)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: dcp_dp
        end function get_dcp_dp_iapws_region1

        module pure elemental function get_enthalpy_iapws_region1(T_in, P_in) result(h)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            real(real64) :: h
        end function get_enthalpy_iapws_region1
    end interface

    interface
        module pure elemental function get_gammao_region2(pi, tau) result(gammao)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Resulting \(\gamma^o\) value
            real(real64) :: gammao

            integer(int32) :: i
            real(real64) :: term

        end function get_gammao_region2

        module pure elemental function get_gammao_pi_region2(pi, tau) result(gammao_pi)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma^o_{\pi}\)
            real(real64) :: gammao_pi

        end function get_gammao_pi_region2

        module pure elemental function get_gammao_pipi_region2(pi, tau) result(gammao_pipi)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma^o_{\pi}\)
            real(real64) :: gammao_pipi

        end function get_gammao_pipi_region2

        module pure elemental function get_gammao_tau_region2(pi, tau) result(gammao_tau)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma^r_{\pi}\)
            real(real64) :: gammao_tau

        end function get_gammao_tau_region2

        module pure elemental function get_gammao_tautau_region2(pi, tau) result(gammao_tautau)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma^r_{\pi}\)
            real(real64) :: gammao_tautau

        end function get_gammao_tautau_region2

        module pure elemental function get_gammao_pitau_region2(pi, tau) result(gammao_pitau)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma^r_{\pi}\)
            real(real64) :: gammao_pitau

        end function get_gammao_pitau_region2

        module pure elemental function get_gammar_region2(pi, tau) result(gammar)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Resulting \(\gamma^r\) value
            real(real64) :: gammar

        end function get_gammar_region2

        module pure elemental function get_gammar_pi_region2(pi, tau) result(gammar_pi)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma^r_{\pi}\)
            real(real64) :: gammar_pi

        end function get_gammar_pi_region2

        module pure elemental function get_gammar_pipi_region2(pi, tau) result(gammar_pipi)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma^r_{\pi\pi}\)
            real(real64) :: gammar_pipi

        end function get_gammar_pipi_region2

        module pure elemental function get_gammar_tau_region2(pi, tau) result(gammar_tau)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma^r_{\tau}\)
            real(real64) :: gammar_tau

        end function get_gammar_tau_region2

        module pure elemental function get_gammar_tautau_region2(pi, tau) result(gammar_tautau)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma^r_{\tau\tau}\)
            real(real64) :: gammar_tautau

        end function get_gammar_tautau_region2

        module pure elemental function get_gammar_pitau_region2(pi, tau) result(gammar_pitau)
            implicit none
            !> Dimensionless pressure \(\pi\)
            real(real64), intent(in) :: pi
            !> Dimensionless temperature \(\tau\)
            real(real64), intent(in) :: tau
            !> Derivative \(\gamma^r_{\pi\tau}\)
            real(real64) :: gammar_pitau

        end function get_gammar_pitau_region2

        module pure elemental function get_density_iapws_region2(T_in, P_in) result(rho)
            implicit none
            !> Temperature [K]
            real(real64), intent(in) :: T_in
            !> Pressure [Pa]
            real(real64), intent(in) :: P_in
            !> Density [kg/m^3]
            real(real64) :: rho

        end function get_density_iapws_region2

        module pure elemental function get_cp_iapws_region2(T_in, P_in) result(cp)
            implicit none
            !> Temperature [K]
            real(real64), intent(in) :: T_in
            !> Pressure [Pa]
            real(real64), intent(in) :: P_in
            !> Specific heat capacity at constant pressure [J/(kg K)]
            real(real64) :: cp

        end function get_cp_iapws_region2

        module pure elemental function get_enthalpy_iapws_region2(T_in, P_in) result(h)
            implicit none
            !> Temperature [K]
            real(real64), intent(in) :: T_in
            !> Pressure [Pa]
            real(real64), intent(in) :: P_in
            !> Specific enthalpy [J/kg]
            real(real64) :: h

        end function get_enthalpy_iapws_region2

    end interface

contains

end module physics_material_iapws
