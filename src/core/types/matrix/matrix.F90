module module_type_matrix
    use :: core_types_matrix
    use :: core_types_matrix_factory
    implicit none
    private

    public :: abst_matrix
    public :: type_dense
    public :: type_csr
    public :: type_coo
    public :: type_bsr

    public :: type_matrix_info
    public :: create_matrix

end module module_type_matrix
