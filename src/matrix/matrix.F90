module module_matrix
    use :: matrix_base, only:abst_matrix
    use :: matrix_coo, only:type_coo
    use :: matrix_crs, only:type_crs, operator(*), operator(+)
    use :: matrix_dense, only:type_dense
    implicit none
    private

    public :: abst_matrix
    public :: type_coo
    public :: type_crs
    public :: operator(*)
    public :: operator(+)
    public :: type_dense

end module module_matrix
