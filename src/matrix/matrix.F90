module module_matrix
    use :: matrix_coo, only:type_coo
    use :: matrix_crs, only:type_crs, operator(*), operator(+)
    implicit none
    private

    public :: type_coo
    public :: type_crs
    public :: operator(*)
    public :: operator(+)

end module module_matrix
