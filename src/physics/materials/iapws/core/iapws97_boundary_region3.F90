submodule(physics_material_iapws97_region3) iapws97_boundary_region3
    implicit none
contains
    !---------------------------------------------------------
    ! Helper: Eq (1) form: theta = sum n_i * pi^I_i
    !---------------------------------------------------------
    pure function eval_poly(P_Pa, n, I) result(T_K)
        real(real64), intent(in) :: P_Pa
        real(real64), intent(in) :: n(:)
        integer(int32), intent(in) :: I(:)
        real(real64) :: T_K
        real(real64) :: pi
        integer :: k

        pi = P_Pa / P_star_r3_bound
        T_K = 0.0d0
        do k = 1, size(n)
            T_K = T_K + n(k) * (pi**I(k))
        end do
        T_K = T_K * T_star_r3_bound
    end function eval_poly

    !---------------------------------------------------------
    ! Helper: Eq (2) form: theta = sum n_i * (ln pi)^I_i
    !---------------------------------------------------------
    pure function eval_logpoly(P_Pa, n, I) result(T_K)
        real(real64), intent(in) :: P_Pa
        real(real64), intent(in) :: n(:)
        integer(int32), intent(in) :: I(:)
        real(real64) :: T_K
        real(real64) :: ln_pi
        integer :: k

        ln_pi = log(P_Pa / P_star_r3_bound)
        T_K = 0.0d0
        do k = 1, size(n)
            T_K = T_K + n(k) * (ln_pi**I(k))
        end do
        T_K = T_K * T_star_r3_bound
    end function eval_logpoly

    !=========================================================
    ! Boundary Equations (SR5-05 Table 1)
    !=========================================================

    ! T3ab(p)
    module pure elemental function calc_T3ab(P) result(T)
        implicit none
        real(real64), intent(in) :: P
        real(real64) :: T
        real(real64), parameter :: n(5) = [ &
                                   0.154793642129415e4_real64, &
                                   -0.187661219490113e3_real64, &
                                   -0.191887498864292e4_real64, &
                                   0.918419702359447e3_real64, &
                                   -0.213144632222113e2_real64]
        integer(int32), parameter :: I(5) = [0, 1, 2, 3, 4]
        T = eval_logpoly(P, n, I)
    end function calc_T3ab

    ! T3cd(p)
    module pure elemental function calc_T3cd(P) result(T)
        implicit none
        real(real64), intent(in) :: P
        real(real64) :: T
        real(real64), parameter :: n(4) = [ &
                                   0.585276966696349e3_real64, &
                                   0.278233532206915e1_real64, &
                                   -0.127283549295878e-1_real64, &
                                   0.159090746562729e-3_real64]
        integer(int32), parameter :: I(4) = [0, 1, 2, 3]
        T = eval_poly(P, n, I)
    end function calc_T3cd

    ! T3gh(p)
    module pure elemental function calc_T3gh(P) result(T)
        implicit none
        real(real64), intent(in) :: P
        real(real64) :: T
        real(real64), parameter :: n(4) = [ &
                                   -0.249284240900418e5_real64, &
                                   0.751608051114157e1_real64, &
                                   0.428143584791546e4_real64, &
                                   -0.787105249910383e-1_real64]
        integer(int32), parameter :: I(4) = [0, 1, 2, 3]
        T = eval_poly(P, n, I)
    end function calc_T3gh

    ! T3ij(p)
    module pure elemental function calc_T3ij(P) result(T)
        implicit none
        real(real64), intent(in) :: P
        real(real64) :: T
        real(real64), parameter :: n(5) = [ &
                                   0.584814781649163e5_real64, &
                                   -0.616179320924617e2_real64, &
                                   0.260763050899562e3_real64, &
                                   -0.587071076864459e-2_real64, &
                                   0.515308185433082e-4_real64]
        integer(int32), parameter :: I(5) = [0, 1, 2, 4, 5]
        T = eval_poly(P, n, I)
    end function calc_T3ij

    ! T3jk(p)
    module pure elemental function calc_T3jk(P) result(T)
        implicit none
        real(real64), intent(in) :: P
        real(real64) :: T
        real(real64), parameter :: n(5) = [ &
                                   0.617229772068439e3_real64, &
                                   -0.770600270141675e1_real64, &
                                   0.697072596851896e-1_real64, &
                                   -0.157391839848015e-1_real64, &
                                   0.137897492684194e-3_real64]
        integer(int32), parameter :: I(5) = [0, 1, 2, 3, 4]
        T = eval_poly(P, n, I)
    end function calc_T3jk

    ! T3mn(p)
    module pure elemental function calc_T3mn(P) result(T)
        implicit none
        real(real64), intent(in) :: P
        real(real64) :: T
        real(real64), parameter :: n(4) = [ &
                                   0.535339483742384e3_real64, &
                                   0.761978122720128e1_real64, &
                                   -0.158365725441648e-2_real64, &
                                   0.192871054508108e-2_real64]
        integer(int32), parameter :: I(4) = [0, 1, 2, 3]
        T = eval_poly(P, n, I)
    end function calc_T3mn

    ! T3op(p)
    module pure elemental function calc_T3op(P) result(T)
        implicit none
        real(real64), intent(in) :: P
        real(real64) :: T
        real(real64), parameter :: n(5) = [ &
                                   0.969461372400213e3_real64, &
                                   -0.332500170441278e3_real64, &
                                   0.642859598466067e2_real64, &
                                   0.773845935768222e3_real64, &
                                   -0.152313732937084e4_real64]
        integer(int32), parameter :: I(5) = [0, 1, 2, -1, -2]
        T = eval_logpoly(P, n, I)
    end function calc_T3op

    ! T3qu(p)
    module pure elemental function calc_T3qu(P) result(T)
        implicit none
        real(real64), intent(in) :: P
        real(real64) :: T
        real(real64), parameter :: n(4) = [ &
                                   0.565603648239126e3_real64, &
                                   0.529062258221222e1_real64, &
                                   -0.102020639611016e-2_real64, &
                                   0.122240301070145e-2_real64]
        integer(int32), parameter :: I(4) = [0, 1, 2, 3]
        T = eval_poly(P, n, I)
    end function calc_T3qu

    ! T3rx(p)
    module pure elemental function calc_T3rx(P) result(T)
        implicit none
        real(real64), intent(in) :: P
        real(real64) :: T
        real(real64), parameter :: n(4) = [ &
                                   0.584561202520006e3_real64, &
                                   -0.102961025163669e1_real64, &
                                   0.243293362700452e-2_real64, &
                                   -0.294905044740799e-2_real64]
        integer(int32), parameter :: I(4) = [0, 1, 2, 3]
        T = eval_poly(P, n, I)
    end function calc_T3rx

    ! T3ef(p): Eq (3) - Linear
    module pure elemental function calc_T3ef(P) result(T)
        implicit none
        real(real64), intent(in) :: P
        real(real64) :: T
        real(real64) :: pi
        pi = P / P_star_r3_bound
        ! theta = 3.727888004 * (pi - 22.064) + 647.096
        T = 3.727888004d0 * (pi - 22.064d0) + 647.096d0
        T = T * T_star_r3_bound
    end function calc_T3ef

    !=========================================================
    ! Auxiliary Equations Boundaries (SR5-05 Table 9)
    !=========================================================

    ! T3uv(p)
    module pure elemental function calc_T3uv(P) result(T)
        implicit none
        real(real64), intent(in) :: P
        real(real64) :: T
        real(real64), parameter :: n(4) = [ &
                                   0.528199646263062e3_real64, &
                                   0.890579602135307e1_real64, &
                                   -0.222814134903755e0_real64, &
                                   0.286791682263697e-2_real64]
        integer(int32), parameter :: I(4) = [0, 1, 2, 3]
        T = eval_poly(P, n, I)
    end function calc_T3uv

    ! T3wx(p)
    module pure elemental function calc_T3wx(P) result(T)
        implicit none
        real(real64), intent(in) :: P
        real(real64) :: T
        real(real64), parameter :: n(5) = [ &
                                   0.728052609145380e1_real64, &
                                   0.973505869861952e2_real64, &
                                   0.147370491183191e2_real64, &
                                   0.329196213998375e3_real64, &
                                   0.873371668682417e3_real64]
        integer(int32), parameter :: I(5) = [0, 1, 2, -1, -2]
        T = eval_logpoly(P, n, I)
    end function calc_T3wx
end submodule iapws97_boundary_region3
