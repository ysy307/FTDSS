module core_types_gauss
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    private

    public :: type_state

    type :: type_state
        real(real64) :: temperature
        real(real64) :: pressure
        real(real64) :: water_content
        real(real64) :: ice_content
        real(real64) :: porosity
        real(real64) :: dQi_dT
        real(real64) :: density_water
        real(real64) :: density_ice
        real(real64) :: vhc_water
        real(real64) :: ice
        real(real64) :: dot_ice
    end type type_state

end module core_types_gauss
