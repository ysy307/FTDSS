module physics_material_iapws08
    use, intrinsic :: iso_fortran_env
    use :: physics_material_iapws_constants, only:p_starIh, T_starIh, &
        p_starIII, T_starIII, p_starV, T_starV, p_starVI, T_starVI, p_starVII, T_starVII
    implicit none
    private

    public :: calc_p_boundary_iapws08_iceIh_melting
    public :: calc_p_boundary_iapws08_iceIh_sublimation
    public :: calc_p_boundary_iapws08_iceIII_melting
    public :: calc_p_boundary_iapws08_iceV_melting
    public :: calc_p_boundary_iapws08_iceVI_melting
    public :: calc_p_boundary_iapws08_iceVII_melting

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
end module physics_material_iapws08
