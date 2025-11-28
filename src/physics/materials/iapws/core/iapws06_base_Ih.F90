submodule(physics_material_iapws06_IceIh) iapws06_base_Ih
    implicit none
    real(real64), parameter :: pi_0 = P_0 / p_starIh
    real(real64), parameter :: s0_Ih = -0.332733756492168d4
    integer(int32), parameter :: gamma0_terms = 5
    real(real64), parameter :: gamma0_Ih(gamma0_terms) = [ & !&
                               -0.632020233335886d6, & !&
                                0.655022213658955d0, & !&
                               -0.189369929326131d-7, & !&
                                0.339746123271053d-14, & !&
                               -0.556464869058991d-21] !&
    complex(real64), parameter :: t1_Ih = (0.368017112855051d-1, 0.510878114959572d-1) !&
    complex(real64), parameter :: r1_Ih = (0.447050716285388d2,  0.656876847463481d2) !&
    complex(real64), parameter :: t2_Ih = (0.337315741065416d0,  0.335449415919309d0) !&
    integer(int32), parameter :: r2_terms = 3
    complex(real64), parameter :: r2_Ih_r2(r2_terms) = [ & !&
                                  (-0.725974574329220d2, -0.781008427112870d2), & !&
                                  (-0.557107698030123d-4, 0.464578634580806d-4), & !&
                                  (0.234801409215913d-10, -0.285651142904972d-10)] !&

contains

    !---------------------------------------------------------------------------
    ! Gibbs Energy: g(T,p) [Eq 1]
    !---------------------------------------------------------------------------
    module pure elemental function calc_gamma_iapws06_Ih(pi, tau) result(gamma)
        implicit none
        real(real64), intent(in) :: pi
        real(real64), intent(in) :: tau
        real(real64) :: gamma

        complex(real64) :: r2
        complex(real64) :: term1, term2

        ! g0(p) - s0 * T
        gamma = calc_gamma0_iapws06_Ih(pi) - s0_Ih * T_starIh * tau
        r2 = calc_r2_iapws06_Ih(pi)

        ! k=1
        term1 = (t1_Ih - tau) * log(t1_Ih - tau) + (t1_Ih + tau) * log(t1_Ih + tau) &
                - 2.0d0 * t1_Ih * log(t1_Ih) - (tau**2.0d0) / t1_Ih

        ! k=2
        term2 = (t2_Ih - tau) * log(t2_Ih - tau) + (t2_Ih + tau) * log(t2_Ih + tau) &
                - 2.0d0 * t2_Ih * log(t2_Ih) - (tau**2.0d0) / t2_Ih

        gamma = gamma + T_starIh * dble(r1_Ih * term1 + r2 * term2)
    end function calc_gamma_iapws06_Ih

    !---------------------------------------------------------------------------
    ! Derivative w.r.t Temperature: g_T [Table 4, 2nd eq]
    ! Result unit: J / (kg K)
    !---------------------------------------------------------------------------
    module pure elemental function calc_gamma_t_iapws06_Ih(pi, tau) result(gamma_t)
        implicit none
        real(real64), intent(in) :: pi
        real(real64), intent(in) :: tau
        real(real64) :: gamma_t

        complex(real64) :: r2
        complex(real64) :: term1, term2

        r2 = calc_r2_iapws06_Ih(pi)

        ! phi_tau terms: -ln(t-tau) + ln(t+tau) - 2tau/t
        ! k=1
        term1 = -log(t1_Ih - tau) + log(t1_Ih + tau) - 2.0d0 * tau / t1_Ih

        ! k=2
        term2 = -log(t2_Ih - tau) + log(t2_Ih + tau) - 2.0d0 * tau / t2_Ih

        gamma_t = -s0_Ih + dble(r1_Ih * term1 + r2 * term2)
    end function calc_gamma_t_iapws06_Ih

    !---------------------------------------------------------------------------
    ! Derivative w.r.t Pressure: g_p [Table 4, 3rd eq]
    ! Result unit: m^3 / kg -> Specific Volume
    !---------------------------------------------------------------------------
    module pure elemental function calc_gamma_p_iapws06_Ih(pi, tau) result(gamma_p)
        implicit none
        real(real64), intent(in) :: pi
        real(real64), intent(in) :: tau
        real(real64) :: gamma_p

        complex(real64) :: r2_p
        complex(real64) :: term2

        r2_p = calc_r2_p_iapws06_Ih(pi)

        ! phi terms (k=2 only because r1_p is 0)
        term2 = (t2_Ih - tau) * log(t2_Ih - tau) + (t2_Ih + tau) * log(t2_Ih + tau) &
                - 2.0d0 * t2_Ih * log(t2_Ih) - (tau**2.0d0) / t2_Ih

        gamma_p = calc_gamma0_p_iapws06_Ih(pi) + T_starIh * dble(r2_p * term2)
    end function calc_gamma_p_iapws06_Ih

    !---------------------------------------------------------------------------
    ! Second Derivative w.r.t Temperature: g_TT [Table 4, 4th eq]
    ! Result unit: J / (kg K^2) -> related to Cp
    !---------------------------------------------------------------------------
    module pure elemental function calc_gamma_tt_iapws06_Ih(pi, tau) result(gamma_tt)
        implicit none
        real(real64), intent(in) :: pi
        real(real64), intent(in) :: tau
        real(real64) :: gamma_tt

        complex(real64) :: r2
        complex(real64) :: term1, term2

        r2 = calc_r2_iapws06_Ih(pi)

        ! phi_t_tau terms: 1/(t-tau) + 1/(t+tau) - 2/t
        ! k=1
        term1 = 1.0d0 / (t1_Ih - tau) + 1.0d0 / (t1_Ih + tau) - 2.0d0 / t1_Ih

        ! k=2
        term2 = 1.0d0 / (t2_Ih - tau) + 1.0d0 / (t2_Ih + tau) - 2.0d0 / t2_Ih

        gamma_tt = (1.0d0 / T_starIh) * dble(r1_Ih * term1 + r2 * term2)
    end function calc_gamma_tt_iapws06_Ih

    !---------------------------------------------------------------------------
    ! Mixed Derivative: g_Tp [Table 4, 5th eq]
    ! Result unit: m^3 / (kg K) -> -Alpha * v
    !---------------------------------------------------------------------------
    module pure elemental function calc_gamma_tp_iapws06_Ih(pi, tau) result(gamma_tp)
        implicit none
        real(real64), intent(in) :: pi
        real(real64), intent(in) :: tau
        real(real64) :: gamma_tp

        complex(real64) :: r2_p
        complex(real64) :: term2

        r2_p = calc_r2_p_iapws06_Ih(pi)

        ! phi_tau term (k=2 only)
        term2 = -log(t2_Ih - tau) + log(t2_Ih + tau) - 2.0d0 * tau / t2_Ih

        gamma_tp = dble(r2_p * term2)
    end function calc_gamma_tp_iapws06_Ih

    !---------------------------------------------------------------------------
    ! Second Derivative w.r.t Pressure: g_pp [Table 4, 6th eq]
    ! Result unit: m^3 / (kg Pa) -> related to Compressibility
    !---------------------------------------------------------------------------
    module pure elemental function calc_gamma_pp_iapws06_Ih(pi, tau) result(gamma_pp)
        implicit none
        real(real64), intent(in) :: pi
        real(real64), intent(in) :: tau
        real(real64) :: gamma_pp

        complex(real64) :: r2_pp
        complex(real64) :: term2

        r2_pp = calc_r2_pp_iapws06_Ih(pi)

        ! phi term (k=2 only)
        term2 = (t2_Ih - tau) * log(t2_Ih - tau) + (t2_Ih + tau) * log(t2_Ih + tau) &
                - 2.0d0 * t2_Ih * log(t2_Ih) - (tau**2.0d0) / t2_Ih

        gamma_pp = calc_gamma0_pp_iapws06_Ih(pi) + T_starIh * dble(r2_pp * term2)
    end function calc_gamma_pp_iapws06_Ih

    !---------------------------------------------------------------------------
    ! Helper Functions: g0(p) and r2(p) derivatives using Horner's method
    !---------------------------------------------------------------------------

    ! g0(p)
    pure elemental function calc_gamma0_iapws06_Ih(pi) result(gamma0)
        implicit none
        real(real64), intent(in) :: pi
        real(real64) :: gamma0
        real(real64) :: delta_pi

        integer(int32) :: i

        delta_pi = pi - pi_0
        gamma0 = gamma0_Ih(gamma0_terms)
        do i = gamma0_terms - 1, 1, -1
            gamma0 = gamma0_Ih(i) + delta_pi * gamma0
        end do
    end function calc_gamma0_iapws06_Ih

    ! g0_p(p) = Sum k * g0k * (pi-pi0)^(k-1) / pt
    pure elemental function calc_gamma0_p_iapws06_Ih(pi) result(val)
        implicit none
        real(real64), intent(in) :: pi
        real(real64) :: val, delta_pi
        integer(int32) :: i
        delta_pi = pi - pi_0
        ! i=5 (k=4) to i=2 (k=1). i=1 (k=0) is constant, derivative is 0.
        val = gamma0_Ih(5) * 4.0d0
        do i = 4, 2, -1
            val = gamma0_Ih(i) * dble(i - 1) + delta_pi * val
        end do
        val = val / p_starIh
    end function calc_gamma0_p_iapws06_Ih

    ! \( g0_pp(p) = \Sum_{k=1}^{4} g0_k * k(k-1) * (pi - pi_0)^(k - 2) / p_t^2 \)
    ! Unit : m^3/kg/Pa
    pure elemental function calc_gamma0_pp_iapws06_Ih(pi) result(val)
        implicit none
        real(real64), intent(in) :: pi
        real(real64) :: val, delta_pi
        integer(int32) :: i
        delta_pi = pi - pi_0
        ! i=5 (k=4) to i=3 (k=2).
        val = gamma0_Ih(5) * 12.0d0 ! 4*3
        do i = 4, 3, -1
            val = gamma0_Ih(i) * dble((i - 1) * (i - 2)) + delta_pi * val
        end do
        val = val / (p_starIh**2.0d0)
    end function calc_gamma0_pp_iapws06_Ih

    ! r2(p)
    pure elemental function calc_r2_iapws06_Ih(pi) result(val)
        implicit none
        real(real64), intent(in) :: pi
        complex(real64) :: val

        real(real64) :: delta_pi
        integer(int32) :: i

        delta_pi = pi - pi_0
        val = r2_Ih_r2(r2_terms)
        do i = r2_terms - 1, 1, -1
            val = r2_Ih_r2(i) + delta_pi * val
        end do
    end function calc_r2_iapws06_Ih

    ! r2_p(p) = Sum k * r2k * (pi-pi0)^(k-1) / pt
    pure elemental function calc_r2_p_iapws06_Ih(pi) result(val)
        implicit none
        real(real64), intent(in) :: pi
        complex(real64) :: val
        real(real64) :: delta_pi
        integer(int32) :: i
        delta_pi = pi - pi_0
        ! i=3 (k=2) to i=2 (k=1)
        val = r2_Ih_r2(3) * 2.0d0
        do i = 2, 2, -1
            val = r2_Ih_r2(i) * dble(i - 1) + delta_pi * val
        end do
        val = val / p_starIh
    end function calc_r2_p_iapws06_Ih

    ! r2_pp(p) = r22 * 2 / pt^2 (Since max k=2)
    pure elemental function calc_r2_pp_iapws06_Ih(pi) result(val)
        implicit none
        real(real64), intent(in) :: pi
        complex(real64) :: val

        val = r2_Ih_r2(3) * 2.0d0 / (p_starIh**2.0d0)
    end function calc_r2_pp_iapws06_Ih

end submodule iapws06_base_Ih
