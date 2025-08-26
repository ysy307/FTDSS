module module_type_matrix
    use :: core_types_matrix, only:abst_matrix
    use :: core_types_matrix_coo, only:type_coo
    use :: core_types_matrix_crs, only:type_crs
    use :: core_types_matrix_dense, only:type_dense
    implicit none
    private

    public :: abst_matrix
    public :: type_coo
    public :: type_crs
    public :: type_dense

    ! public :: gemv
    ! interface gemv
    !     module procedure :: type_coo_gemv
    !     module procedure :: type_crs_gemv
    !     module procedure :: type_dense_gemv
    ! end interface

    ! public :: add
    ! interface add
    !     module procedure :: type_coo_add
    !     module procedure :: type_crs_add
    !     module procedure :: type_dense_add
    ! end interface

end module module_type_matrix
