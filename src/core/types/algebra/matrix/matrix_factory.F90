module types_algebra_matrix_factory
    use, intrinsic :: iso_fortran_env, only: int32
    use :: core_constants
    use :: types_algebra_matrix
    implicit none
    private

    public :: create_matrix

contains
    function create_matrix(matrix_type, num_nodes, row, col, block_size) result(matrix)
        implicit none
        !> The type of matrix to create: "dense", "crs", or "coo".
        type(type_constant_id), intent(in) :: matrix_type
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

        if (.not. MATRIX_TYPES%is_valid(matrix_type)) then
            error stop "Error: Invalid matrix type in create_matrix."
        end if

        if (matrix_type == MATRIX_TYPES%DENSE) then
            allocate (type_matrix_dense :: matrix)
            call matrix%initialize(num_nodes)
        else if (matrix_type == MATRIX_TYPES%CSR) then
            allocate (type_matrix_csr :: matrix)
            call matrix%initialize(num_nodes, row, col)
        else if (matrix_type == MATRIX_TYPES%COO) then
            allocate (type_matrix_coo :: matrix)
            call matrix%initialize(num_nodes, row, col)
        else if (matrix_type == MATRIX_TYPES%BSR) then
            allocate (type_matrix_bsr :: matrix)
            call matrix%initialize(num_nodes, row, col, block_size, block_size)
        else if (matrix_type == MATRIX_TYPES%DIA) then
            allocate (type_matrix_dia :: matrix)
            call matrix%initialize(num_nodes, row, col)
        else
            error stop "Error: Unsupported matrix type in create_matrix."
        end if
        
    end function create_matrix
end module types_algebra_matrix_factory
