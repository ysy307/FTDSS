submodule(physics_material_iapws97_region4) iapws97_base_region4
    implicit none
    !------------------------------------------------------------------------------------------
    ! Region4: Saturation curve between liquid and vapor (IAPWS-IF97)
    !------------------------------------------------------------------------------------------
    integer(int32), parameter :: N4_terms = 10
    real(real64), parameter :: n_r4(N4_terms) = [ & !&
                                0.11670521452767d4, & !&
                               -0.72421316703206d6, & !&
                               -0.17073846940092d2, & !&
                                0.12020824702470d5, & !&
                               -0.32325550322333d7, & !&
                                0.14915108613530d2, & !&
                               -0.48232657361591d4, & !&
                                0.40511340542057d6, & !&
                               -0.23855557567849d0, & !&
                                0.65017534844798d3] !&

contains

    pure elemental function calc_beta_iapws97_region4(pressure) result(beta)
        implicit none
        real(real64), intent(in) :: pressure
        real(real64) :: beta

        ! Eq 29a: Transformed pressure beta
        beta = (pressure / P_star4)**0.25d0

    end function calc_beta_iapws97_region4

    pure elemental function calc_vartheta_iapws97_region4(temperature) result(vartheta)
        implicit none
        real(real64), intent(in) :: temperature
        real(real64) :: vartheta

        ! Eq 29b: Transformed temperature vartheta
        vartheta = temperature / T_star4 + n_r4(9) / (temperature / T_star4 - n_r4(10))

    end function calc_vartheta_iapws97_region4

    !> Saturation Pressure (Equation 30) [cite: 921]
    module pure elemental function calc_psat_iapws97_region4(temperature) result(P_sat)
        implicit none
        real(real64), intent(in) :: temperature
        real(real64) :: P_sat

        real(real64) :: vartheta, A, B, C

        vartheta = calc_vartheta_iapws97_region4(temperature)

        ! Eq 30 auxiliary terms
        A = vartheta**2 + n_r4(1) * vartheta + n_r4(2)
        B = n_r4(3) * vartheta**2 + n_r4(4) * vartheta + n_r4(5)
        C = n_r4(6) * vartheta**2 + n_r4(7) * vartheta + n_r4(8)

        ! Eq 30: ps/p* = [ 2C / (-B + sqrt...) ]^4
        P_sat = P_star4 * (((2.0d0 * C) / (-B + sqrt(max((B**2 - 4.0d0 * A * C), 0.0d0))))**4)

    end function calc_psat_iapws97_region4

    !> Saturation Temperature (Equation 31 - Backward Equation)
    !! Calculated explicitly using the same coefficients as Eq 30.
    module pure elemental function calc_tsat_iapws97_region4(pressure) result(T_sat)
        implicit none
        real(real64), intent(in) :: pressure
        real(real64) :: T_sat

        real(real64) :: beta, E, F, G, D

        ! beta = (p / p*)^0.25
        beta = calc_beta_iapws97_region4(pressure)

        ! Eq 31 auxiliary terms
        E = beta**2 + n_r4(3) * beta + n_r4(6)
        F = n_r4(1) * beta**2 + n_r4(4) * beta + n_r4(7)
        G = n_r4(2) * beta**2 + n_r4(5) * beta + n_r4(8)

        ! D = 2G / (-F - sqrt(F^2 - 4EG))x
        D = (2.0d0 * G) / (-F - sqrt(max((F**2 - 4.0d0 * E * G), 0.0d0)))

        ! Ts/T* = (n10 + D - sqrt((n10 + D)^2 - 4(n9 + n10*D))) / 2
        T_sat = T_star4 * (n_r4(10) + D - sqrt(max((n_r4(10) + D)**2 - 4.0d0 * (n_r4(9) + n_r4(10) * D), 0.0d0))) / 2.0d0

    end function calc_tsat_iapws97_region4

end submodule iapws97_base_region4
