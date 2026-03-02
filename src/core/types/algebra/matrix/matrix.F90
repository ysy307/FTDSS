module core_types_matrix
    use :: types_algebra_matrix
    use :: types_algebra_matrix_factory
    implicit none
    private

    public :: abst_matrix
    public :: type_matrix_dense
    public :: type_matrix_csr
    public :: type_matrix_coo
    public :: type_matrix_bsr
    public :: type_matrix_dia

    public :: type_matrix_info
    public :: create_matrix

end module core_types_matrix
