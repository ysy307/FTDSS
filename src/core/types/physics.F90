module core_types_physics
    use, intrinsic :: iso_fortran_env, only: int32, real64
    implicit none
    private

    public :: type_state
    public :: type_physics_phase

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

    type :: type_physics_phase
        integer(int32) :: num_phases = 0
        real(real64) :: solid = 0.0d0
        real(real64) :: water = 0.0d0
        real(real64) :: ice = 0.0d0
        real(real64) :: vapor = 0.0d0
        real(real64) :: air = 0.0d0
    end type type_physics_phase

end module core_types_physics
