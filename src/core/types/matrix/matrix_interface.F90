!>
!> Defines an abstract matrix type and concrete implementations for dense,
!> COO (Coordinate), and CRS (Compressed Row Storage) sparse matrices.
!>
module core_types_matrix
    use, intrinsic :: iso_fortran_env
    use :: core_allocate, only:allocate_array
    use :: core_deallocate, only:deallocate_array
    use :: core_findings, only:binary_find
    implicit none
    private

    public :: abst_matrix
    public :: type_coo
    public :: type_crs
    public :: type_dense

    ! ==========================================================
    ! Abstract Base Type and Interface
    ! ==========================================================

    !>
    !> An abstract base type for matrix objects, defining a common API for
    !> different storage formats. It supports operations based on a two-level
    !> addressing scheme (node index and DOF index within the node).
    !>
    type, abstract :: abst_matrix
        private
        !> The number of nodes associated with the matrix dimensions.
        integer(int32) :: num_nodes = 0
        !> The number of degrees of freedom (DOFs) per node.
        integer(int32) :: num_dofs = 0
    contains
        procedure(abst_initialize), public, pass(self), deferred :: initialize
        procedure(abst_destroy), public, pass(self), deferred :: destroy

        procedure(abst_set_value), private, pass(self), deferred :: set_value
        procedure(abst_set_row), private, pass(self), deferred :: set_row
        procedure(abst_set_all), private, pass(self), deferred :: set_all
        !> Generic interface for setting matrix values.
        generic, public :: set => set_value, set_row, set_all

        procedure(abst_zero), public, pass(self), deferred :: zero
        procedure(abst_add_value), private, pass(self), deferred :: add_value
        procedure(abst_add_matrix), private, pass(self), deferred :: add_matrix
        !> Generic interface for adding values to the matrix.
        generic, public :: add => add_value, add_matrix

        procedure(abst_gemv), public, pass(self), deferred :: gemv

        procedure(abst_display), public, pass(self), deferred :: display
    end type abst_matrix

    abstract interface
        !>
        !> Initializes the matrix structure based on the number of nodes and DOFs.
        !> For sparse matrices, a sparsity pattern can be provided.
        !>
        subroutine abst_initialize(self, num_nodes, num_dofs, row, col)
            import :: abst_matrix, int32
            implicit none
            !> The matrix object to initialize.
            class(abst_matrix), intent(inout) :: self
            !> The number of nodes.
            integer(int32), intent(in) :: num_nodes
            !> The number of DOFs per node.
            integer(int32), intent(in) :: num_dofs
            !> Optional node-level CSR `ptr` array to define sparsity.
            integer(int32), intent(in), optional :: row(:)
            !> Optional node-level CSR `ind` array to define sparsity.
            integer(int32), intent(in), optional :: col(:)
        end subroutine abst_initialize

        !>
        !> Sets the value of a single entry in the matrix.
        !>
        subroutine abst_set_value(self, row_dof, col_dof, row, col, value)
            import :: abst_matrix, int32, real64
            implicit none
            !> The matrix object.
            class(abst_matrix), intent(inout) :: self
            !> The 1-based DOF index within the row/column node.
            integer(int32), intent(in) :: row_dof, col_dof
            !> The 1-based node index for the row/column.
            integer(int32), intent(in) :: row, col
            !> The value to set.
            real(real64), intent(in) :: value
        end subroutine abst_set_value

        !>
        !> Sets all entries in a specified row to a single value.
        !>
        subroutine abst_set_row(self, row_dof, row, value)
            import :: abst_matrix, real64, int32
            implicit none
            !> The matrix object.
            class(abst_matrix), intent(inout) :: self
            !> The 1-based DOF index within the row node.
            integer(int32), intent(in) :: row_dof
            !> The 1-based node index for the row.
            integer(int32), intent(in) :: row
            !> The scalar value to assign.
            real(real64), intent(in) :: value
        end subroutine abst_set_row

        !>
        !> Sets all stored entries in the matrix to a single value.
        !>
        subroutine abst_set_all(self, value)
            import :: abst_matrix, real64
            implicit none
            !> The matrix object.
            class(abst_matrix), intent(inout) :: self
            !> The scalar value to assign.
            real(real64), intent(in) :: value
        end subroutine abst_set_all

        !>
        !> Sets all stored entries in the matrix to zero.
        !>
        subroutine abst_zero(self)
            import :: abst_matrix
            implicit none
            !> The matrix object.
            class(abst_matrix), intent(inout) :: self
        end subroutine abst_zero

        !>
        !> Adds a value to a single entry in the matrix.
        !>
        subroutine abst_add_value(self, row_dof, col_dof, row, col, value)
            import :: abst_matrix, int32, real64
            implicit none
            !> The matrix object.
            class(abst_matrix), intent(inout) :: self
            !> The 1-based DOF index within the row/column node.
            integer(int32), intent(in) :: row_dof, col_dof
            !> The 1-based node index for the row/column.
            integer(int32), intent(in) :: row, col
            !> The value to add.
            real(real64), intent(in) :: value
        end subroutine abst_add_value

        !>
        !> Performs the matrix operation C = alpha*A + B, where A is `self`.
        !>
        subroutine abst_add_matrix(self, alpha, B, C)
            import :: abst_matrix, real64
            implicit none
            !> The matrix object (A).
            class(abst_matrix), intent(in) :: self
            !> The scalar multiplier alpha.
            real(real64), intent(in) :: alpha
            !> The matrix B.
            class(abst_matrix), intent(in) :: B
            !> The matrix C to store the result.
            class(abst_matrix), intent(inout) :: C
        end subroutine abst_add_matrix

        !>
        !> Performs a general matrix-vector multiplication: y = alpha*A*x + beta*y.
        !>
        subroutine abst_gemv(self, alpha, x, beta, y)
            import :: abst_matrix, real64
            implicit none
            !> The matrix object (A).
            class(abst_matrix), intent(in) :: self
            !> The scalar multiplier alpha.
            real(real64), intent(in) :: alpha
            !> The input vector x.
            real(real64), intent(in) :: x(:)
            !> The scalar multiplier beta.
            real(real64), intent(in) :: beta
            !> The input/output vector y.
            real(real64), intent(inout) :: y(:)
        end subroutine abst_gemv

        !>
        !> Prints the matrix contents to standard output for debugging.
        !>
        subroutine abst_display(self)
            import :: abst_matrix
            implicit none
            !> The matrix object.
            class(abst_matrix), intent(in) :: self
        end subroutine abst_display

        !>
        !> Deallocates all memory associated with the matrix.
        !>
        subroutine abst_destroy(self)
            import :: abst_matrix
            implicit none
            !> The matrix object.
            class(abst_matrix), intent(inout) :: self
        end subroutine abst_destroy
    end interface

    ! ==========================================================
    ! Dense Matrix Implementation
    ! ==========================================================

    !>
    !> Represents a dense matrix stored as a 2D array.
    !>
    type, extends(abst_matrix) :: type_dense
        !> The number of rows in the matrix.
        integer(int32) :: num_row = 0
        !> The number of columns in the matrix.
        integer(int32) :: num_col = 0
        !> The 2D array holding the matrix values.
        real(real64), allocatable :: val(:, :)
    contains
        procedure, pass(self) :: initialize => initialize_dense
        procedure, pass(self) :: destroy => destroy_dense
        procedure, pass(self) :: get_num_row => get_num_row_dense
        procedure, pass(self) :: get_num_col => get_num_col_dense
        procedure, pass(self) :: get_val => get_val_dense
        procedure, pass(self) :: set_value => set_value_dense
        procedure, pass(self) :: set_row => set_row_dense
        procedure, pass(self) :: set_all => set_all_dense
        procedure, pass(self) :: zero => zero_dense
        procedure, pass(self) :: add_value => add_value_dense
        procedure, pass(self) :: add_matrix => add_matrix_dense
        procedure, pass(self) :: gemv => gemv_dense
        procedure, pass(self) :: display => display_dense
    end type type_dense

    interface
        !> Initializes a dense matrix.
        module subroutine initialize_dense(self, num_nodes, num_dofs, row, col)
            implicit none
            class(type_dense), intent(inout) :: self
            integer(int32), intent(in) :: num_nodes
            integer(int32), intent(in) :: num_dofs
            integer(int32), intent(in), optional :: row(:)
            integer(int32), intent(in), optional :: col(:)
        end subroutine initialize_dense

        !> Deallocates the dense matrix.
        module subroutine destroy_dense(self)
            implicit none
            class(type_dense), intent(inout) :: self
        end subroutine destroy_dense

        !> Returns the number of rows in the dense matrix.
        module pure function get_num_row_dense(self) result(num_row)
            implicit none
            class(type_dense), intent(in) :: self
            integer(int32) :: num_row
        end function get_num_row_dense

        !> Returns the number of columns in the dense matrix.
        module pure function get_num_col_dense(self) result(num_col)
            implicit none
            class(type_dense), intent(in) :: self
            integer(int32) :: num_col
        end function get_num_col_dense

        !> Returns a pointer to the dense matrix's value array.
        module function get_val_dense(self) result(val)
            implicit none
            class(type_dense), intent(in), target :: self
            real(real64), dimension(:, :), pointer :: val
        end function get_val_dense

        !> Sets a single value in the dense matrix.
        module subroutine set_value_dense(self, row_dof, col_dof, row, col, value)
            implicit none
            class(type_dense), intent(inout) :: self
            integer(int32), intent(in) :: row_dof, col_dof, row, col
            real(real64), intent(in) :: value
        end subroutine set_value_dense

        !> Sets all values in a specified row of the dense matrix.
        module subroutine set_row_dense(self, row_dof, row, value)
            implicit none
            class(type_dense), intent(inout) :: self
            integer(int32), intent(in) :: row_dof, row
            real(real64), intent(in) :: value
        end subroutine set_row_dense

        !> Sets all values in the dense matrix to a single scalar value.
        module subroutine set_all_dense(self, value)
            class(type_dense), intent(inout) :: self
            real(real64), intent(in) :: value
        end subroutine set_all_dense

        !> Sets all values in the dense matrix to zero.
        module subroutine zero_dense(self)
            implicit none
            class(type_dense), intent(inout) :: self
        end subroutine zero_dense

        !> Adds a value to a single entry in the dense matrix.
        module subroutine add_value_dense(self, row_dof, col_dof, row, col, value)
            implicit none
            class(type_dense), intent(inout) :: self
            integer(int32), intent(in) :: row_dof, col_dof, row, col
            real(real64), intent(in) :: value
        end subroutine add_value_dense

        !> Performs the matrix operation C = alpha*A + B for dense matrices.
        module subroutine add_matrix_dense(self, alpha, B, C)
            implicit none
            class(type_dense), intent(in) :: self
            real(real64), intent(in) :: alpha
            class(abst_matrix), intent(in) :: B
            class(abst_matrix), intent(inout) :: C
        end subroutine add_matrix_dense

        !> Performs the matrix-vector multiplication y = alpha*A*x + beta*y for a dense matrix.
        module subroutine gemv_dense(self, alpha, x, beta, y)
            implicit none
            class(type_dense), intent(in) :: self
            real(real64), intent(in) :: alpha
            real(real64), intent(in) :: x(:)
            real(real64), intent(in) :: beta
            real(real64), intent(inout) :: y(:)
        end subroutine gemv_dense

        !> Displays the contents of the dense matrix.
        module subroutine display_dense(self)
            implicit none
            class(type_dense), intent(in) :: self
        end subroutine display_dense
    end interface

    ! ==========================================================
    ! CRS Matrix Implementation
    ! ==========================================================

    !>
    !> Represents a sparse matrix in Compressed Row Storage (CRS) format.
    !>
    type, extends(abst_matrix) :: type_crs
        !> Number of non-zero elements.
        integer(int32) :: nnz = 0
        !> Number of rows.
        integer(int32) :: num_row = 0
        !> Number of entries in the pointer array (num_row + 1).
        integer(int32) :: num_ptr = 0
        !> Pointers to the start of each row in the `ind` and `val` arrays.
        integer(int32), allocatable :: ptr(:)
        !> Column indices of the non-zero elements.
        integer(int32), allocatable :: ind(:)
        !> Values of the non-zero elements.
        real(real64), allocatable :: val(:)
    contains
        procedure, pass(self) :: initialize => initialize_type_crs
        procedure, pass(self) :: destroy => destroy_crs
        procedure, pass(self) :: get_nnz => get_nnz_crs
        procedure, pass(self) :: get_num_ptr => get_num_ptr_crs
        procedure, pass(self) :: get_num_row => get_num_row_crs
        procedure, pass(self) :: get_ptr => get_ptr_crs
        procedure, pass(self) :: get_ind => get_ind_crs
        procedure, pass(self) :: get_val => get_val_crs
        procedure, pass(self) :: set_value => set_crs
        procedure, pass(self) :: set_row => set_row_crs
        procedure, pass(self) :: set_all => set_all_crs
        procedure, pass(self) :: zero => zero_crs
        procedure, private, pass(self) :: find => find_crs
        procedure, pass(self) :: add_value => add_crs
        procedure, pass(self) :: add_matrix => add_matrix_crs
        procedure, pass(self) :: gemv => gemv_crs
        procedure, pass(self) :: display => display_crs
    end type type_crs

    interface
        !> Initializes a CRS matrix from a node-level sparsity pattern.
        module subroutine initialize_type_crs(self, num_nodes, num_dofs, row, col)
            implicit none
            class(type_crs), intent(inout) :: self
            integer(int32), intent(in) :: num_nodes
            integer(int32), intent(in) :: num_dofs
            integer(int32), intent(in), optional :: row(:)
            integer(int32), intent(in), optional :: col(:)
        end subroutine initialize_type_crs

        !> Deallocates the CRS matrix.
        module subroutine destroy_crs(self)
            implicit none
            class(type_crs), intent(inout) :: self
        end subroutine destroy_crs

        !> Returns the number of non-zero entries.
        module pure function get_nnz_crs(self) result(nnz)
            implicit none
            class(type_crs), intent(in) :: self
            integer(int32) :: nnz
        end function get_nnz_crs

        !> Returns the size of the pointer array.
        module pure function get_num_ptr_crs(self) result(num_ptr)
            implicit none
            class(type_crs), intent(in) :: self
            integer(int32) :: num_ptr
        end function get_num_ptr_crs

        !> Returns the number of rows.
        module pure function get_num_row_crs(self) result(num_row)
            implicit none
            class(type_crs), intent(in) :: self
            integer(int32) :: num_row
        end function get_num_row_crs

        !> Returns a pointer to the `ptr` array.
        module function get_ptr_crs(self) result(ptr)
            implicit none
            class(type_crs), intent(in), target :: self
            integer(int32), dimension(:), pointer :: ptr
        end function get_ptr_crs

        !> Returns a pointer to the `ind` array.
        module function get_ind_crs(self) result(ind)
            implicit none
            class(type_crs), intent(in), target :: self
            integer(int32), dimension(:), pointer :: ind
        end function get_ind_crs

        !> Returns a pointer to the `val` array.
        module function get_val_crs(self) result(val)
            implicit none
            class(type_crs), intent(in), target :: self
            real(real64), dimension(:), pointer :: val
        end function get_val_crs

        !> Finds the storage index of a matrix entry.
        module pure function find_crs(self, row_dof, col_dof, row, col) result(index)
            implicit none
            class(type_crs), intent(in) :: self
            integer(int32), intent(in) :: row_dof, col_dof, row, col
            integer(int32) :: index
        end function find_crs

        !> Sets a single value in the matrix.
        module subroutine set_crs(self, row_dof, col_dof, row, col, value)
            implicit none
            class(type_crs), intent(inout) :: self
            integer(int32), intent(in) :: row_dof, col_dof, row, col
            real(real64), intent(in) :: value
        end subroutine set_crs

        !> Sets all values in a row.
        module subroutine set_row_crs(self, row_dof, row, value)
            implicit none
            class(type_crs), intent(inout) :: self
            integer(int32), intent(in) :: row_dof, row
            real(real64), intent(in) :: value
        end subroutine set_row_crs

        !> Sets all stored values in the matrix.
        module subroutine set_all_crs(self, value)
            implicit none
            class(type_crs), intent(inout) :: self
            real(real64), intent(in) :: value
        end subroutine set_all_crs

        !> Sets all stored matrix values to zero.
        module subroutine zero_crs(self)
            implicit none
            class(type_crs), intent(inout) :: self
        end subroutine zero_crs

        !> Adds a value to a single matrix entry.
        module subroutine add_crs(self, row_dof, col_dof, row, col, value)
            implicit none
            class(type_crs), intent(inout) :: self
            integer(int32), intent(in) :: row_dof, col_dof, row, col
            real(real64), intent(in) :: value
        end subroutine add_crs

        !> Performs C = alpha*A + B for CRS matrices.
        module subroutine add_matrix_crs(self, alpha, B, C)
            implicit none
            class(type_crs), intent(in) :: self
            real(real64), intent(in) :: alpha
            class(abst_matrix), intent(in) :: B
            class(abst_matrix), intent(inout) :: C
        end subroutine add_matrix_crs

        !> Performs y = alpha*A*x + beta*y for a CRS matrix.
        module subroutine gemv_crs(self, alpha, x, beta, y)
            implicit none
            class(type_crs), intent(in) :: self
            real(real64), intent(in) :: alpha
            real(real64), intent(in) :: x(:)
            real(real64), intent(in) :: beta
            real(real64), intent(inout) :: y(:)
        end subroutine gemv_crs

        !> Displays the matrix contents.
        module subroutine display_crs(self)
            implicit none
            class(type_crs), intent(in) :: self
        end subroutine display_crs
    end interface

    ! ==========================================================
    ! COO Matrix Implementation
    ! ==========================================================

    !>
    !> Represents a sparse matrix in Coordinate (COO) list format.
    !>
    type, extends(abst_matrix) :: type_coo
        !> Number of non-zero elements.
        integer(int32) :: nnz = 0
        !> Number of rows.
        integer(int32) :: num_row = 0
        !> Number of columns.
        integer(int32) :: num_col = 0
        !> Row indices of the non-zero elements.
        integer(int32), allocatable :: row(:)
        !> Column indices of the non-zero elements.
        integer(int32), allocatable :: col(:)
        !> Values of the non-zero elements.
        real(real64), allocatable :: val(:)
    contains
        procedure, pass(self) :: initialize => initialize_type_coo
        procedure, pass(self) :: destroy => destroy_coo
        procedure, pass(self) :: get_nnz => get_nnz_coo
        procedure, pass(self) :: get_num_row => get_num_row_coo
        procedure, pass(self) :: get_num_col => get_num_col_coo
        procedure, pass(self) :: get_row => get_row_coo
        procedure, pass(self) :: get_col => get_col_coo
        procedure, pass(self) :: get_val => get_val_coo
        procedure, pass(self) :: set_value => set_coo
        procedure, pass(self) :: set_all => set_all_coo
        procedure, pass(self) :: set_row => set_row_coo
        procedure, private, pass(self) :: find => find_coo
        procedure, pass(self) :: zero => zero_coo
        procedure, pass(self) :: add_value => add_coo
        procedure, pass(self) :: add_matrix => add_matrix_coo
        procedure, pass(self) :: gemv => gemv_coo
        procedure, pass(self) :: display => display_coo
    end type type_coo

    interface
        !> Initializes a COO matrix from a node-level sparsity pattern.
        module subroutine initialize_type_coo(self, num_nodes, num_dofs, row, col)
            implicit none
            class(type_coo), intent(inout) :: self
            integer(int32), intent(in) :: num_nodes
            integer(int32), intent(in) :: num_dofs
            integer(int32), intent(in), optional :: row(:)
            integer(int32), intent(in), optional :: col(:)
        end subroutine initialize_type_coo

        !> Returns the number of non-zero entries.
        module pure function get_nnz_coo(self) result(nnz)
            implicit none
            class(type_coo), intent(in) :: self
            integer(int32) :: nnz
        end function get_nnz_coo

        !> Returns the number of rows.
        module pure function get_num_row_coo(self) result(num_row)
            implicit none
            class(type_coo), intent(in) :: self
            integer(int32) :: num_row
        end function get_num_row_coo

        !> Returns the number of columns.
        module pure function get_num_col_coo(self) result(num_col)
            implicit none
            class(type_coo), intent(in) :: self
            integer(int32) :: num_col
        end function get_num_col_coo

        !> Returns a pointer to the `row` index array.
        module function get_row_coo(self) result(row)
            implicit none
            class(type_coo), intent(in), target :: self
            integer(int32), dimension(:), pointer :: row
        end function get_row_coo

        !> Returns a pointer to the `col` index array.
        module function get_col_coo(self) result(col)
            implicit none
            class(type_coo), intent(in), target :: self
            integer(int32), dimension(:), pointer :: col
        end function get_col_coo

        !> Returns a pointer to the `val` array.
        module function get_val_coo(self) result(val)
            implicit none
            class(type_coo), intent(in), target :: self
            real(real64), dimension(:), pointer :: val
        end function get_val_coo

        !> Finds the storage index of a matrix entry.
        module pure function find_coo(self, row_dof, col_dof, row, col) result(index)
            implicit none
            class(type_coo), intent(in) :: self
            integer(int32), intent(in) :: row_dof, col_dof, row, col
            integer(int32) :: index
        end function find_coo

        !> Sets a single value in the matrix.
        module subroutine set_coo(self, row_dof, col_dof, row, col, value)
            implicit none
            class(type_coo), intent(inout) :: self
            integer(int32), intent(in) :: row_dof, col_dof, row, col
            real(real64), intent(in) :: value
        end subroutine set_coo

        !> Sets all values in a row.
        module subroutine set_row_coo(self, row_dof, row, value)
            implicit none
            class(type_coo), intent(inout) :: self
            integer(int32), intent(in) :: row_dof, row
            real(real64), intent(in) :: value
        end subroutine set_row_coo

        !> Sets all stored values in the matrix.
        module subroutine set_all_coo(self, value)
            implicit none
            class(type_coo), intent(inout) :: self
            real(real64), intent(in) :: value
        end subroutine set_all_coo

        !> Sets all stored matrix values to zero.
        module subroutine zero_coo(self)
            implicit none
            class(type_coo), intent(inout) :: self
        end subroutine zero_coo

        !> Adds a value to a single matrix entry.
        module subroutine add_coo(self, row_dof, col_dof, row, col, value)
            implicit none
            class(type_coo), intent(inout) :: self
            integer(int32), intent(in) :: row_dof, col_dof, row, col
            real(real64), intent(in) :: value
        end subroutine add_coo

        !> Performs C = alpha*A + B for COO matrices.
        module subroutine add_matrix_coo(self, alpha, B, C)
            implicit none
            class(type_coo), intent(in) :: self
            real(real64), intent(in) :: alpha
            class(abst_matrix), intent(in) :: B
            class(abst_matrix), intent(inout) :: C
        end subroutine add_matrix_coo

        !> Performs y = alpha*A*x + beta*y for a COO matrix.
        module subroutine gemv_coo(self, alpha, x, beta, y)
            implicit none
            class(type_coo), intent(in) :: self
            real(real64), intent(in) :: alpha
            real(real64), intent(in) :: x(:)
            real(real64), intent(in) :: beta
            real(real64), intent(inout) :: y(:)
        end subroutine gemv_coo

        !> Displays the matrix contents.
        module subroutine display_coo(self)
            implicit none
            class(type_coo), intent(in) :: self
        end subroutine display_coo

        !> Deallocates the COO matrix.
        module subroutine destroy_coo(self)
            implicit none
            class(type_coo), intent(inout) :: self
        end subroutine destroy_coo
    end interface

end module core_types_matrix
