module core_constants_physical
    use, intrinsic :: iso_fortran_env
    implicit none
    private

    !> Water retention function of Brooks-Corey model
    integer(int32), parameter, public :: WRF_BC = 1
    !> Water retention function of van-Genuchten model
    integer(int32), parameter, public :: WRF_VG = 2
    !> Water retention function of Kosugi model
    integer(int32), parameter, public :: WRF_KO = 3
    !> Water retention function of modified van-Genuchten model
    integer(int32), parameter, public :: WRF_MVG = 4
    !> Water retention function of durner model
    integer(int32), parameter, public :: WRF_DURNER = 5
    !> Water retention function of dvgch model
    integer(int32), parameter, public :: WRF_DVGCH = 6

    integer(int32), parameter, public :: HCF_BASE = 1
    integer(int32), parameter, public :: HCF_IMPEDANCE = 2
    integer(int32), parameter, public :: HCF_VISCOSITY = 3
    integer(int32), parameter, public :: HCF_BASE_IMPEDANCE = 4
    integer(int32), parameter, public :: HCF_BASE_VISCOSITY = 5
    integer(int32), parameter, public :: HCF_IMPEDANCE_VISCOSITY = 6
    integer(int32), parameter, public :: HCF_BASE_IMPEDANCE_VISCOSITY = 7

    integer(int32), parameter, public :: HCF_BC = 1
    integer(int32), parameter, public :: HCF_VG = 2
    integer(int32), parameter, public :: HCF_KO = 3
    integer(int32), parameter, public :: HCF_MVG = 4
    integer(int32), parameter, public :: HCF_DURNER = 5
    integer(int32), parameter, public :: HCF_DVGCH = 6

    integer(int32), parameter, public :: HCF_VISCOSITY_EXPONENTIAL = 1
    integer(int32), parameter, public :: HCF_VISCOSITY_SUPERCOOLED = 2

end module core_constants_physical
