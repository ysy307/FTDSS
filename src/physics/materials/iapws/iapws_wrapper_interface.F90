module physics_material_iapws_wrapper
    use, intrinsic :: iso_fortran_env, only: real64, int32
    use :: stdlib_optval, only:optval
    use :: physics_constants
    use :: physics_material_iapws
    use :: physics_material_iapws_constants
    implicit none

    interface
        module pure elemental function determine_iapws_region(T_in, P_in, is_supercooled) result(region_id)
            implicit none
            real(real64), intent(in) :: T_in ! Temperature [K]
            real(real64), intent(in) :: P_in ! Pressure [Pa]
            logical, intent(in), optional :: is_supercooled
            integer(int32) :: region_id

        end function determine_iapws_region
    end interface

contains

end module physics_material_iapws_wrapper
