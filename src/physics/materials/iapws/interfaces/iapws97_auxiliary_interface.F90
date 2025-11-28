module physics_material_iapws97_auxiliary
    use, intrinsic :: iso_fortran_env
    use :: physics_material_iapws_constants, only:T_starb23, p_starb23
    implicit none
    private
    public :: calc_p_boundary_iapws97_region23
    public :: calc_t_boundary_iapws97_region23

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

end module physics_material_iapws97_auxiliary
