submodule(physics_material_iapws08) iapws08_base
    implicit none
    integer(int32), parameter :: N_Ih_melt_terms = 3
    real(real64), parameter :: a_Ih_melt(N_Ih_melt_terms) = [0.119539337d7, 0.808183159d5, 0.333826860d4]
    real(real64), parameter :: b_Ih_melt(N_Ih_melt_terms) = [0.300000d1, 0.257500d2, 0.103750d3]
    integer(int32), parameter :: N_Ih_sub_terms = 3
    real(real64), parameter :: a_Ih_sub(N_Ih_sub_terms) = [-0.212144006d2, 0.273203819d2, -0.610598130d1]
    real(real64), parameter :: b_Ih_sub(N_Ih_sub_terms) = [0.333333333d-2, 0.120666667d1, 0.170333333d1]

contains
    !---------------------------------------------------------------------------
    ! Eq (1): Melting pressure of Ice Ih (Liquid-Solid boundary)
    ! Range: 273.16 K to 251.165 K
    !---------------------------------------------------------------------------
    module pure elemental function calc_p_boundary_iapws08_iceIh_melting(T_in) result(p)
        implicit none
        real(real64), intent(in) :: T_in
        real(real64) :: p

        real(real64) :: theta, pi

        theta = T_in / T_starIh

        pi = 1.0 + &
             a_Ih_melt(1) * (1.0 - theta**b_Ih_melt(1)) + &
             a_Ih_melt(2) * (1.0 - theta**b_Ih_melt(2)) + &
             a_Ih_melt(3) * (1.0 - theta**b_Ih_melt(3))

        p = pi * P_starIh ! Result in Pa
    end function calc_p_boundary_iapws08_iceIh_melting

    !---------------------------------------------------------------------------
    ! Eq (6): Sublimation pressure of Ice Ih (Gas-Solid boundary)
    ! Range: 50 K to 273.16 K
    !---------------------------------------------------------------------------
    module pure elemental function calc_p_boundary_iapws08_iceIh_sublimation(T_in) result(p)
        implicit none
        real(real64), intent(in) :: T_in
        real(real64) :: p

        real(real64) :: theta, ln_pi

        theta = T_in / T_starIh

        ln_pi = (1.0 / theta) * ( &
                a_Ih_sub(1) * (theta**b_Ih_sub(1)) + &
                a_Ih_sub(2) * (theta**b_Ih_sub(2)) + &
                a_Ih_sub(3) * (theta**b_Ih_sub(3)))

        p = P_starIh * exp(ln_pi) ! Result in Pa
    end function calc_p_boundary_iapws08_iceIh_sublimation

    !---------------------------------------------------------------------------
    ! Eq (2): Melting pressure of Ice III
    ! Range: 251.165 K to 256.164 K
    !---------------------------------------------------------------------------
    module pure elemental function calc_p_boundary_iapws08_iceIII_melting(T_in) result(p)
        implicit none
        real(real64), intent(in) :: T_in
        real(real64) :: p

        real(real64) :: theta, pi

        theta = T_in / T_starIII

        pi = 1.0d0 - 0.299948d0 * (1.0d0 - theta**60.0d0)
        p = pi * P_starIII
    end function calc_p_boundary_iapws08_iceIII_melting

    !---------------------------------------------------------------------------
    ! Eq (3): Melting pressure of Ice V
    ! Range: 256.164 K to 273.31 K
    !---------------------------------------------------------------------------
    module pure elemental function calc_p_boundary_iapws08_iceV_melting(T_in) result(p)
        implicit none
        real(real64), intent(in) :: T_in
        real(real64) :: p

        real(real64) :: theta, pi

        theta = T_in / T_starV

        pi = 1.0d0 - 1.18721d0 * (1.0d0 - theta**8.0d0)
        p = pi * P_starV

    end function calc_p_boundary_iapws08_iceV_melting

    !---------------------------------------------------------------------------
    ! Eq (4): Melting pressure of Ice VI
    ! Range: 273.31 K to 355 K
    !---------------------------------------------------------------------------
    module pure elemental function calc_p_boundary_iapws08_iceVI_melting(T_in) result(p)
        implicit none
        real(real64), intent(in) :: T_in
        real(real64) :: p
        real(real64) :: theta, pi

        theta = T_in / T_starVI

        pi = 1.0 - 1.07476d0 * (1.0d0 - theta**4.6d0)
        p = pi * P_starVI
    end function calc_p_boundary_iapws08_iceVI_melting

    !---------------------------------------------------------------------------
    ! Eq (5): Melting pressure of Ice VII
    ! Range: 355 K to 715 K
    !---------------------------------------------------------------------------
    module pure elemental function calc_p_boundary_iapws08_iceVII_melting(T_in) result(p)
        implicit none
        real(real64), intent(in) :: T_in
        real(real64) :: p
        real(real64) :: theta, ln_pi

        theta = T_in / T_starVII

        ln_pi = 0.173683d1 * (1.0 - theta**(-1.0d0)) &
                - 0.544606d-1 * (1.0 - theta**5.0d0) &
                + 0.806106d-7 * (1.0 - theta**22.0d0)

        p = P_starVII * exp(ln_pi)
    end function calc_p_boundary_iapws08_iceVII_melting
end submodule iapws08_base
