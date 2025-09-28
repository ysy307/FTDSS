module module_type_matrix
    use, intrinsic :: iso_fortran_env
    use :: core_types_matrix, only:abst_matrix, type_coo, type_crs, type_dense
    implicit none
    private

    public :: abst_matrix
    public :: type_dense
    public :: type_crs
    public :: type_coo

end module module_type_matrix
