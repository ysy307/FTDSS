module core_types_physics
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: core_types_coordinate, only:type_coordinate_dp
    implicit none
    private

    public :: type_state
    public :: type_physics_info

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
        !> Latent heat of fusion, \(L_\mathrm{f}\) [J/kg]
        real(real64) :: latent_heat_fusion
        !> Latent heat of vaporization, \(h_\mathrm{v}\) [J/kg]
        real(real64) :: latent_heat_vaporization
        !> Rate of change of water content with respect to temperature, \(\frac{d\theta_w}{dT}\) [-/K]
        real(real64) :: dQw_dT
        !> Rate of change of vapor content with respect to temperature, \(\frac{d\theta_v}{dT}\) [-/K]
        real(real64) :: dQv_dT
        !> Density of water [kg/m^3]
        real(real64) :: density_water
        real(real64) :: density_ice
        real(real64) :: vhc_water
        !> Relative humidity, \(H_\mathrm{r}\) [-]
        real(real64) :: relative_humidity
        !> Water flux vector, \(\mathbf{q}_w\) [m/s]
        type(type_coordinate_dp) :: water_flux

    end type type_state

    type :: type_physics_info
        integer(int32) :: num_phases = 0
        real(real64) :: solid = 0.0d0
        real(real64) :: water = 0.0d0
        real(real64) :: ice = 0.0d0
        real(real64) :: vapor = 0.0d0
        real(real64) :: air = 0.0d0
        real(real64), allocatable :: dispersity(:)
        real(real64), allocatable :: params(:)
    end type type_physics_info

end module core_types_physics
