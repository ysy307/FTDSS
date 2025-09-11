module module_type_matrix
    use, intrinsic :: iso_fortran_env, only: int32
    use :: core_types_matrix, only:abst_matrix, type_coo, type_crs, type_dense
    implicit none
    private

    public :: abst_matrix
    public :: type_dense
    public :: type_crs
    public :: type_coo

    integer(int32), public, parameter :: matrix_dense = 0
    integer(int32), public, parameter :: matrix_crs = 1
    integer(int32), public, parameter :: matrix_coo = 2

end module module_type_matrix
