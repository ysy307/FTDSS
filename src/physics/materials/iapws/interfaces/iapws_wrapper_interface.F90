module physics_material_iapws_wrapper
    use, intrinsic :: iso_fortran_env, only: real64, int32
    use :: stdlib_optval, only:optval
    use :: physics_constants
    use :: physics_material_iapws_constants
    use :: physics_material_iapws06_IceIh
    use :: physics_material_iapws08
    use :: physics_material_iapws97_region1
    use :: physics_material_iapws97_region2
    use :: physics_material_iapws97_region3
    use :: physics_material_iapws97_region4
    use :: physics_material_iapws97_region5
    use :: physics_material_iapws97_auxiliary
    implicit none
    private

    public :: type_iapws_property
    public :: calc_iapws_properties

    type :: type_iapws_property
        !> IAPWS物性値構造体
        integer(int32) :: region_id = -1
        !> Specific Volume, \( \nu \) [m^3/kg]
        real(real64) :: nu = 0.0d0
        !> Density, \( \rho \) [kg/m^3]
        real(real64) :: rho = 0.0d0
        !> Specific Internal Energy, \( u \) [J/kg]
        real(real64) :: u = 0.0d0
        !> Specific Entropy, \( s \) [J/(kg K)]
        real(real64) :: s = 0.0d0
        !> Specific Enthalpy, \( h \) [J/kg]
        real(real64) :: h = 0.0d0
        !> Specific Heat Capacity at constant pressure, \( c_p \) [J/(kg K)]
        real(real64) :: cp = 0.0d0
        !> Specific Heat Capacity at constant volume, \( c_v \) [J/(kg K)]
        real(real64) :: cv = 0.0d0
        !> Speed of Sound, \( w \) [m/s]
        real(real64) :: w = 0.0d0
        !> Pressure, \( p \) [Pa]
        real(real64) :: p = 0.0d0
        !> Temperature, \( T \) [K]
        real(real64) :: T = 0.0d0
        !> Cubic expansion coefficient [1/K]
        real(real64) :: alpha = 0.0d0
        !> Pressure coefficient [Pa/K]
        real(real64) :: beta = 0.0d0
        !> Isothermal compressibility [1/Pa]
        real(real64) :: kappa_s = 0.0d0
        !> Isentropic compressibility [1/Pa]
        real(real64) :: kappa_T = 0.0d0
    end type type_iapws_property

    interface
        module pure elemental function determine_iapws_region(T_in, P_in, is_supercooled) result(region_id)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            logical, intent(in), optional :: is_supercooled
            integer(int32) :: region_id

        end function determine_iapws_region
    end interface

    interface
        module pure elemental subroutine calc_iapws_properties(T_in, P_in, properties, is_supercooled)
            implicit none
            real(real64), intent(in) :: T_in
            real(real64), intent(in) :: P_in
            type(type_iapws_property), intent(inout) :: properties
            logical, intent(in), optional :: is_supercooled

        end subroutine calc_iapws_properties
    end interface

contains

end module physics_material_iapws_wrapper
