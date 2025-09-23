module core_constants
    use, intrinsic :: iso_fortran_env, only: int32
    implicit none
    private

    ! --- Physics Types ---
    integer(int32), parameter, public :: PHYSICS_TYPE_THERMAL    = 1 !&
    integer(int32), parameter, public :: PHYSICS_TYPE_HYDRAULIC  = 2 !&
    integer(int32), parameter, public :: PHYSICS_TYPE_MECHANICAL = 3 !&

    integer(int32), parameter, public :: NUM_PHYSICS_TYPES = 3

    ! --- DOF Types ---
    integer(int32), parameter, public :: DOF_TYPE_T = 1 ! Temperature
    integer(int32), parameter, public :: DOF_TYPE_P = 2 ! Pressure
    integer(int32), parameter, public :: DOF_TYPE_DX = 3 ! Disp X
    integer(int32), parameter, public :: DOF_TYPE_DY = 4 ! Disp Y
    integer(int32), parameter, public :: DOF_TYPE_DZ = 5 ! Disp Z

    ! Definition of boundary condition type IDs for thermal analysis
    integer(int32), parameter, public :: THERMAL_BC_DIRICHLET  = 101 !&
    integer(int32), parameter, public :: THERMAL_BC_NEUMANN    = 102 !&
    integer(int32), parameter, public :: THERMAL_BC_FLUX       = 103 !&
    integer(int32), parameter, public :: THERMAL_BC_ROBIN      = 104 !&
    integer(int32), parameter, public :: THERMAL_BC_CONVECTIVE = 105 !&
    integer(int32), parameter, public :: THERMAL_BC_RADIATION  = 106 !&
    integer(int32), parameter, public :: THERMAL_BC_ADIABATIC  = 107 !&
    integer(int32), parameter, public :: THERMAL_BC_FREE       = 108 !&

    ! -----------------------------
    ! Thermal BC application sequence
    ! -----------------------------
    integer(int32), parameter, public :: THERMAL_BC_SEQUENCE(8) = [ &
                                         THERMAL_BC_ROBIN, &
                                         THERMAL_BC_CONVECTIVE, &
                                         THERMAL_BC_RADIATION, &
                                         THERMAL_BC_FLUX, &
                                         THERMAL_BC_ADIABATIC, &
                                         THERMAL_BC_FREE, &
                                         THERMAL_BC_NEUMANN, &
                                         THERMAL_BC_DIRICHLET]

    ! Definition of boundary condition type IDs for hydraulic analysis
    integer(int32), parameter, public :: HYDRAULIC_BC_DIRICHLET   = 201 !&
    integer(int32), parameter, public :: HYDRAULIC_BC_NEUMANN     = 202 !&
    integer(int32), parameter, public :: HYDRAULIC_BC_FLUX        = 203 !&
    integer(int32), parameter, public :: HYDRAULIC_BC_IMPERMEABLE = 204 !&
    integer(int32), parameter, public :: HYDRAULIC_BC_SEEPAGE     = 205 !&

    ! -----------------------------
    ! Hydraulic BC application sequence
    ! -----------------------------
    integer(int32), parameter, public :: HYDRAULIC_BC_SEQUENCE(5) = [ &
                                         HYDRAULIC_BC_FLUX, &
                                         HYDRAULIC_BC_IMPERMEABLE, &
                                         HYDRAULIC_BC_SEEPAGE, &
                                         HYDRAULIC_BC_NEUMANN, &
                                         HYDRAULIC_BC_DIRICHLET]

end module core_constants
