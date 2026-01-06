module core_constants_physical
    use, intrinsic :: iso_fortran_env
    implicit none
    private

    !> Physics unit in systems
    integer(int32), parameter, public :: PHYSICS_UNIT_M = 1
    integer(int32), parameter, public :: PHYSICS_UNIT_CM = 2
    integer(int32), parameter, public :: PHYSICS_UNIT_PA = 3

    !> SWCC models
    integer(int32), parameter, public :: SWCC_BC = 1
    integer(int32), parameter, public :: SWCC_VG = 2
    integer(int32), parameter, public :: SWCC_KO = 3
    integer(int32), parameter, public :: SWCC_MVG = 4
    integer(int32), parameter, public :: SWCC_DURNER = 5
    integer(int32), parameter, public :: SWCC_DVGCH = 6
    !> Water retention function of Brooks-Corey model
    integer(int32), parameter, public :: WRF_BC = SWCC_BC
    !> Water retention function of van-Genuchten model
    integer(int32), parameter, public :: WRF_VG = SWCC_VG
    !> Water retention function of Kosugi model
    integer(int32), parameter, public :: WRF_KO = SWCC_KO
    !> Water retention function of modified van-Genuchten model
    integer(int32), parameter, public :: WRF_MVG = SWCC_MVG
    !> Water retention function of durner model
    integer(int32), parameter, public :: WRF_DURNER = SWCC_DURNER
    !> Water retention function of dvgch model
    integer(int32), parameter, public :: WRF_DVGCH = SWCC_DVGCH

    integer(int32), parameter, public :: HCF_BASE = 1
    integer(int32), parameter, public :: HCF_IMPEDANCE = 2
    integer(int32), parameter, public :: HCF_VISCOSITY = 3
    integer(int32), parameter, public :: HCF_BASE_IMPEDANCE = 4
    integer(int32), parameter, public :: HCF_BASE_VISCOSITY = 5
    integer(int32), parameter, public :: HCF_IMPEDANCE_VISCOSITY = 6
    integer(int32), parameter, public :: HCF_BASE_IMPEDANCE_VISCOSITY = 7

    integer(int32), parameter, public :: HCF_BC = SWCC_BC
    integer(int32), parameter, public :: HCF_VG = SWCC_VG
    integer(int32), parameter, public :: HCF_KO = SWCC_KO
    integer(int32), parameter, public :: HCF_MVG = SWCC_MVG
    integer(int32), parameter, public :: HCF_DURNER = SWCC_DURNER
    integer(int32), parameter, public :: HCF_DVGCH = SWCC_DVGCH

    integer(int32), parameter, public :: HCF_VISCOSITY_EXPONENTIAL = 1
    integer(int32), parameter, public :: HCF_VISCOSITY_SUPERCOOLED = 2

    integer(int32), parameter, public :: GCC_NON_SEGREGATION = 1
    integer(int32), parameter, public :: GCC_SEGREGATION = 2

end module core_constants_physical
