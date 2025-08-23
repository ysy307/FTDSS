module core_types_gauss
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    private

    public :: type_state

    type :: type_state
        real(real64) :: temperature !! T
        real(real64) :: pressure !! P
        real(real64) :: water_content !! \theta_w
        real(real64) :: ice_content !! \theta_i
        real(real64) :: dot_ice !! \dot{\theta_i}
        real(real64) :: porosity !! \phi
        real(real64) :: latent_heat !! L_f
        real(real64) :: dQw_dT !! dQw/dT
        real(real64) :: density_water !! \rho_w
        real(real64) :: density_ice !! \rho_i
        real(real64) :: vhc_water !! C_w
    end type type_state

end module core_types_gauss
