module physics_material_iapws97_region3
    use, intrinsic :: iso_fortran_env
    use :: physics_constants, only:R_w => specific_gas_constant_water
    use :: physics_material_iapws_constants
    implicit none
    private

    public :: calc_T3ab
    public :: calc_T3cd
    public :: calc_T3gh
    public :: calc_T3ij
    public :: calc_T3jk
    public :: calc_T3mn
    public :: calc_T3op
    public :: calc_T3qu
    public :: calc_T3rx
    public :: calc_T3ef
    public :: calc_T3uv
    public :: calc_T3wx

    public :: calc_rho_iapws97_region3
    public :: calc_nu_iapws97_region3
    public :: calc_p_iapws97_region3
    public :: calc_u_iapws97_region3
    public :: calc_s_iapws97_region3
    public :: calc_h_iapws97_region3
    public :: calc_cp_iapws97_region3
    public :: calc_cv_iapws97_region3
    public :: calc_w_iapws97_region3
    public :: calc_maxwell_residual_1
    public :: calc_maxwell_residual_2
    public :: calc_maxwell_residual_3

    type :: r3_coeff_type
        integer(int32), allocatable :: I(:)
        integer(int32), allocatable :: J(:)
        real(real64), allocatable :: n(:)
    end type r3_coeff_type

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

        module pure elemental function calc_T3ab(P) result(T)
            implicit none
            real(real64), intent(in) :: P
            real(real64) :: T

        end function calc_T3ab

        ! T3cd(p)
        module pure elemental function calc_T3cd(P) result(T)
            implicit none
            real(real64), intent(in) :: P
            real(real64) :: T

        end function calc_T3cd

        ! T3gh(p)
        module pure elemental function calc_T3gh(P) result(T)
            implicit none
            real(real64), intent(in) :: P
            real(real64) :: T

        end function calc_T3gh

        ! T3ij(p)
        module pure elemental function calc_T3ij(P) result(T)
            implicit none
            real(real64), intent(in) :: P
            real(real64) :: T

        end function calc_T3ij

        ! T3jk(p)
        module pure elemental function calc_T3jk(P) result(T)
            implicit none
            real(real64), intent(in) :: P
            real(real64) :: T

        end function calc_T3jk

        ! T3mn(p)
        module pure elemental function calc_T3mn(P) result(T)
            implicit none
            real(real64), intent(in) :: P
            real(real64) :: T

        end function calc_T3mn

        ! T3op(p)
        module pure elemental function calc_T3op(P) result(T)
            implicit none
            real(real64), intent(in) :: P
            real(real64) :: T

        end function calc_T3op

        module pure elemental function calc_T3qu(P) result(T)
            implicit none
            real(real64), intent(in) :: P
            real(real64) :: T

        end function calc_T3qu

        module pure elemental function calc_T3rx(P) result(T)
            implicit none
            real(real64), intent(in) :: P
            real(real64) :: T

        end function calc_T3rx

        module pure elemental function calc_T3ef(P) result(T)
            implicit none
            real(real64), intent(in) :: P
            real(real64) :: T

        end function calc_T3ef

        module pure elemental function calc_T3uv(P) result(T)
            implicit none
            real(real64), intent(in) :: P
            real(real64) :: T

        end function calc_T3uv

        module pure elemental function calc_T3wx(P) result(T)
            implicit none
            real(real64), intent(in) :: P
            real(real64) :: T

        end function calc_T3wx

        module pure elemental function calc_rho_iapws97_region3(T_in, P_in, id) result(rho)
            real(real64), intent(in) :: T_in, P_in
            integer(int32), intent(in) :: id
            real(real64) :: rho
            real(real64) :: v

        end function calc_rho_iapws97_region3

        module pure elemental function calc_nu_iapws97_region3(T_in, P_in, id) result(v)
            implicit none
            real(real64), intent(in) :: T_in, P_in
            integer(int32), intent(in) :: id
            real(real64) :: v

        end function calc_nu_iapws97_region3

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

end module physics_material_iapws97_region3
