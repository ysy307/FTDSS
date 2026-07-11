!> Physical reference constants shared across input normalization and physics models.
module core_physical_constants
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    private

    !> Reference water density [kg/m^3].
    real(real64), parameter, public :: reference_water_density = 1000.0d0

    !> Standard gravitational acceleration [m/s^2].
    real(real64), parameter, public :: gravity_acceleration = 9.80665d0

end module core_physical_constants
