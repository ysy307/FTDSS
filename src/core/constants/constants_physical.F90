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

end module core_constants_physical
