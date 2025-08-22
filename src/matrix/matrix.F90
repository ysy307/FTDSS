module module_matrix
    use :: matrix_base,  only:abst_matrix !&
    use :: matrix_coo,   only:type_coo, type_coo_gemv, type_coo_add !&
    use :: matrix_crs,   only:type_crs, type_crs_gemv, type_crs_add !&
    use :: matrix_dense, only:type_dense, type_dense_gemv, type_dense_add !&
    implicit none
    private

    public :: abst_matrix
    public :: type_coo
    public :: type_crs
    public :: type_dense

    public :: gemv
    interface gemv
        module procedure :: type_coo_gemv
        module procedure :: type_crs_gemv
        module procedure :: type_dense_gemv
    end interface

    public :: add
    interface add
        module procedure :: type_coo_add
        module procedure :: type_crs_add
        module procedure :: type_dense_add
    end interface

end module module_matrix
