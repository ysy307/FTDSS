module core_types_matrix_factory
    use, intrinsic :: iso_fortran_env, only: int32
    use :: core_constants
    use :: core_types_matrix
    implicit none
    private

    public :: create_matrix

contains
    function create_matrix(matrix_type, num_nodes, row, col, block_size) result(matrix)
        implicit none
        !> The type of matrix to create: "dense", "crs", or "coo".
        integer(int32), intent(in) :: matrix_type
        !> The number of nodes.
        integer(int32), intent(in) :: num_nodes
        !> Optional node-level CSR `ptr` array to define sparsity.
        integer(int32), intent(in), optional :: row(:)
        !> Optional node-level CSR `ind` array to define sparsity.
        integer(int32), intent(in), optional :: col(:)
        !> Optional block size for block sparse row matrices.
        integer(int32), intent(in), optional :: block_size
        !> The matrix object to initialize.
        class(abst_matrix), allocatable :: matrix

        select case (matrix_type)
        case (MATRIX_DENSE)
            allocate (type_matrix_dense :: matrix)
            call matrix%initialize(num_nodes)
        case (MATRIX_CSR)
            allocate (type_matrix_csr :: matrix)
            call matrix%initialize(num_nodes, row, col)
        case (MATRIX_COO)
            allocate (type_matrix_coo :: matrix)
            call matrix%initialize(num_nodes, row, col)
        case (MATRIX_BSR)
            allocate (type_matrix_bsr :: matrix)
            call matrix%initialize(num_nodes, row, col, block_size, block_size)
        case default
            error stop "Error: Unsupported matrix type in create_matrix."
        end select

    end function create_matrix
end module core_types_matrix_factory
