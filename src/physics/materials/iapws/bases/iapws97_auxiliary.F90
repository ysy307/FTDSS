submodule(physics_material_iapws) iapws97_auxiliary
    implicit none
    real(real64), parameter :: T_starb23 = 1.0d0 ! K
    real(real64), parameter :: p_starb23 = 1.0d6 ! Pa
    real(real64), parameter :: nb23(5) = [ 0.34805185628969d3, & !&
                                         -0.11671859879975d1, & !&
                                          0.10192970039326d-2, & !&
                                          0.57254459862746d3, & !&
                                          0.13918839778870d2]
contains

    !> 領域2と3の境界における圧力を計算する（式5）
    !!
    !! IAPWS-IF97 式(5)に基づき、与えられた温度に対する境界圧力を返す。
    !! B23-equation: pi = n1 + n2*theta + n3*theta^2
    !!
    !! temperature : 温度 [K]
    module pure elemental function calc_p_boundary_iapws97_region23(temperature) result(pressure)
        implicit none
        real(real64), intent(in) :: temperature
        real(real64) :: pressure

        real(real64) :: theta, pi_val

        theta = temperature / T_starb23

        pi_val = nb23(1) + theta * (nb23(2) + theta * nb23(3))

        pressure = pi_val * p_starb23

    end function calc_p_boundary_iapws97_region23

    !> 領域2と3の境界における温度を計算する（式6）
    !!
    !! IAPWS-IF97 式(6)に基づき、与えられた圧力に対する境界温度を返す。
    !! Equation: theta = n4 + sqrt((pi - n5) / n3)
    !!
    !! pressure : 圧力 [Pa]
    module pure elemental function calc_t_boundary_iapws97_region23(pressure) result(temperature)
        implicit none
        real(real64), intent(in) :: pressure
        real(real64) :: temperature

        real(real64) :: pi_val, theta

        pi_val = pressure / p_starb23

        theta = nb23(4) + sqrt(max(0.0d0, (pi_val - nb23(5)) / nb23(3)))

        temperature = theta * T_starb23

    end function calc_t_boundary_iapws97_region23
end submodule iapws97_auxiliary
