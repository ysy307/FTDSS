module linalg_matrix_factory
    use, intrinsic :: iso_fortran_env
    use :: module_core

    implicit none
    private

    public :: create_matrix

    public :: holder_matrices
    public :: allocate_matrix

    type :: holder_matrices
        class(abst_matrix), allocatable :: m
    end type holder_matrices

contains
    subroutine allocate_matrix(matrix, length)
        implicit none
        type(holder_matrices), allocatable :: matrix(:)
        integer(int32), intent(in) :: length

        if (allocated(matrix)) then
            deallocate (matrix)
        end if

        allocate (matrix(length))

        ! Initialize each matrix in the array
        ! Do something with range if needed
    end subroutine allocate_matrix

    function create_matrix(matrix_type, num_nodes, num_dofs, row, col) result(matrix)
        implicit none
        !> The type of matrix to create: "dense", "crs", or "coo".
        integer(int32), intent(in) :: matrix_type
        !> The number of nodes.
        integer(int32), intent(in) :: num_nodes
        !> The number of DOFs per node.
        integer(int32), intent(in) :: num_dofs
        !> Optional node-level CSR `ptr` array to define sparsity.
        integer(int32), intent(in), optional :: row(:)
        !> Optional node-level CSR `ind` array to define sparsity.
        integer(int32), intent(in), optional :: col(:)
        !> The matrix object to initialize.
        class(abst_matrix), allocatable :: matrix

        select case (matrix_type)
        case (MATRIX_DENSE)
            allocate (type_dense :: matrix)
            call matrix%initialize(num_nodes, num_dofs)
        case (MATRIX_CRS)
            allocate (type_crs :: matrix)
            call matrix%initialize(num_nodes, num_dofs, row, col)
        case (MATRIX_COO)
            allocate (type_coo :: matrix)
            call matrix%initialize(num_nodes, num_dofs, row, col)
        case default
            allocate (type_dense :: matrix)
            call matrix%initialize(num_nodes, num_dofs)
            write (*, *) "Warning: Unknown matrix type. Defaulting to dense matrix."
        end select

    end function create_matrix

end module linalg_matrix_factory
