module physics_material_iapws97_region4
    use, intrinsic :: iso_fortran_env
    use :: physics_constants, only:R_w => specific_gas_constant_water
    use :: physics_material_iapws_constants, only:T_star4, p_star4
    implicit none
    private
    public :: calc_psat_iapws97_region4
    public :: calc_tsat_iapws97_region4

    interface
        module pure elemental function calc_psat_iapws97_region4(temperature) result(P_sat)
            implicit none
            real(real64), intent(in) :: temperature
            real(real64) :: P_sat

        end function calc_psat_iapws97_region4

        module pure elemental function calc_tsat_iapws97_region4(pressure) result(T_sat)
            implicit none
            real(real64), intent(in) :: pressure
            real(real64) :: T_sat

        end function calc_tsat_iapws97_region4
    end interface
end module physics_material_iapws97_region4
