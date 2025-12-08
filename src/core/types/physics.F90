module core_types_physics
    use, intrinsic :: iso_fortran_env, only: int32, real64
    implicit none
    private

    public :: type_state
    public :: type_physics_phase

    type :: type_state
        !> Temperature [C]
        real(real64) :: temperature
        !> Pressure [m]
        real(real64) :: pressure
        !> Water content, \(\theta_w\) [-]
        real(real64) :: water_content
        !> Ice content, \(\theta_i\) [-]
        real(real64) :: ice_content
        !> Rate of change of ice content, \(\dot{\theta_i}\) [-]
        real(real64) :: dot_ice
        !> Porosity, \(\phi\) [-]
        real(real64) :: porosity
        !> Latent heat, \(L_f\) [J/kg]
        real(real64) :: latent_heat
        real(real64) :: dQw_dT
        real(real64) :: density_water
        real(real64) :: density_ice
        real(real64) :: vhc_water
        !> Relative humidity, \(H_\mathrm{r}\) [-]
        real(real64) :: relative_humidity
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
