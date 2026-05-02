!>
!> Defines an abstract matrix type and concrete implementations for dense,
!> COO (Coordinate), and CRS (Compressed Row Storage) sparse matrices.
!>
module core_types_algebra_matrix
    use, intrinsic :: iso_fortran_env
!$  use :: omp_lib
    use :: stdlib_optval, only:optval
    use :: core_constants
    use :: core_allocate, only:allocate_array
    use :: core_deallocate, only:deallocate_array
    use :: core_findings, only:binary_find
    use :: core_check_range, only:value_in_range
    use :: core_types_algebra_vector, only:type_vector_dp
    implicit none
    private

    public :: abst_matrix
    public :: type_matrix_dense
    public :: type_matrix_coo
    public :: type_matrix_csr
    public :: type_matrix_bsr
    public :: type_matrix_dia

    public :: type_matrix_info

    type :: type_matrix_info
        sequence
        integer(int32) :: num_nodes = 0
        integer(int32) :: num_rows = 0
        integer(int32) :: num_cols = 0
        integer(int32) :: num_ptrs = 0
        integer(int32) :: num_blocks = 0
        integer(int32) :: num_block_rows = 0
        integer(int32) :: num_block_cols = 0
        integer(int32) :: nnz = 0
    end type type_matrix_info

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
        !> Status flag for the matrix or matrix operations.
        type(type_constant_id) :: status = MATRIX_STATUS%SUCCESS
        !> flag of the matrix initialized or not.
        logical :: is_initialized_matrix = .false.
    contains
        procedure(abst_initialize),      public,  pass(self), deferred :: initialize !&
        procedure(abst_destroy),         public,  pass(self), deferred :: destroy !&
        procedure(abst_get_info),        public,  pass(self), deferred :: get_info !&
        procedure(abst_get_diagonal),    public,  pass(self), deferred :: get_diagonal !&
        procedure, public, pass(self) :: get_status
        procedure, public, pass(self) :: is_initialized

        procedure(abst_set_value),       private, pass(self), deferred :: set_value !&
        procedure(abst_set_value_block), private, pass(self), deferred :: set_value_block !&
        procedure(abst_set_row),         private, pass(self), deferred :: set_row !&
        procedure(abst_set_all),         private, pass(self), deferred :: set_all !&
        procedure(abst_set_by_index), private, pass(self), deferred :: set_by_index
        generic, public :: set => set_value, & !&
                                  set_value_block, & !&
                                  set_row, & !&
                                  set_all, & !&
                                  set_by_index !&

        procedure(abst_scale),           public,  pass(self), deferred :: scale !&
        procedure(abst_zero_all),        public,  pass(self), deferred :: zero_all !&
        procedure(abst_zero_row),        public,  pass(self), deferred :: zero_row !&
        generic, public :: zero => zero_all, & !&
                                   zero_row !&
        procedure(abst_find),            private, pass(self), deferred :: find !&
        procedure, public, pass(self) :: check => check_matrix_status
        procedure(abst_display),         public,  pass(self), deferred :: display !&
    end type abst_matrix

    abstract interface
        !>
        !> Initializes the matrix structure based on the number of nodes and DOFs.
        !> For sparse matrices, a sparsity pattern can be provided.
        subroutine abst_initialize(self, num_nodes, row, col, row_blocks, col_blocks)
            import :: abst_matrix, type_constant_id, int32
            implicit none
            !> The matrix object to initialize.
            class(abst_matrix), intent(inout) :: self
            !> The number of nodes.
            integer(int32), intent(in) :: num_nodes
            !> Optional node-level CSR `ptr` array to define sparsity.
            integer(int32), intent(in), optional :: row(:)
            !> Optional node-level CSR `ind` array to define sparsity.
            integer(int32), intent(in), optional :: col(:)
            !> Optional DOF-level CSR `ptr` array to define sparsity.
            integer(int32), intent(in), optional :: row_blocks
            !> Optional DOF-level CSR `ind` array to define sparsity.
            integer(int32), intent(in), optional :: col_blocks
        end subroutine abst_initialize

        subroutine abst_get_info(self, info)
            import :: abst_matrix, type_constant_id, type_matrix_info
            implicit none
            !> The matrix object.
            class(abst_matrix), intent(in) :: self
            !> The matrix information structure to populate.
            type(type_matrix_info), intent(inout) :: info
        end subroutine abst_get_info

        subroutine abst_get_diagonal(self, diagonal)
            import :: abst_matrix, type_constant_id, type_vector_dp
            implicit none
            !> The matrix object.
            class(abst_matrix), intent(in) :: self
            !> The diagonal entries of the matrix.
            type(type_vector_dp), intent(inout) :: diagonal
        end subroutine abst_get_diagonal

        !>
        !> Sets the value of a single entry in the matrix.
        subroutine abst_set_value(self, op, row, col, value)
            import :: abst_matrix, type_constant_id, int32, real64
            implicit none
            !> The matrix object.
            class(abst_matrix), intent(inout) :: self
            !> The operation to perform
            type(type_constant_id), intent(in) :: op
            !> The 1-based node index for the row.
            integer(int32), intent(in) :: row
            !> The 1-based node index for the column.
            integer(int32), intent(in) :: col
            !> The value to set.
            real(real64), intent(in) :: value
        end subroutine abst_set_value

        !>
        !> Sets the value in block of a single entry in the matrix.
        subroutine abst_set_value_block(self, op, row, col, row_block, col_block, value)
            import :: abst_matrix, type_constant_id, int32, real64
            implicit none
            !> The matrix object.
            class(abst_matrix), intent(inout) :: self
            !> The operation to perform
            type(type_constant_id), intent(in) :: op
            !> The 1-based node index for the row.
            integer(int32), intent(in) :: row
            !> The 1-based node index for the column.
            integer(int32), intent(in) :: col
            !> The 1-based DOF index for the row.
            integer(int32), intent(in) :: row_block
            !> The 1-based DOF index for the column.
            integer(int32), intent(in) :: col_block
            !> The value to set.
            real(real64), intent(in) :: value
        end subroutine abst_set_value_block

        !>
        !> Sets all entries in a specified row to a single value.
        subroutine abst_set_row(self, op, row, value, row_block)
            import :: abst_matrix, type_constant_id, real64, int32
            implicit none
            !> The matrix object.
            class(abst_matrix), intent(inout) :: self
            !> The operation to perform
            type(type_constant_id), intent(in) :: op
            !> The 1-based node index for the row.
            integer(int32), intent(in) :: row
            !> The scalar value to assign.
            real(real64), intent(in) :: value
            !> The block row index.
            integer(int32), intent(in), optional :: row_block
        end subroutine abst_set_row

        !>
        !> Sets all stored entries in the matrix to a single value.
        subroutine abst_set_all(self, op, value)
            import :: abst_matrix, type_constant_id, real64, int32
            implicit none
            !> The matrix object.
            class(abst_matrix), intent(inout) :: self
            !> The operation to perform
            type(type_constant_id), intent(in) :: op
            !> The scalar value to assign.
            real(real64), intent(in) :: value
        end subroutine abst_set_all

        subroutine abst_set_by_index(self, op, index, row_block, col_block, value)
            import :: abst_matrix, type_constant_id, int32, real64
            implicit none
            class(abst_matrix), intent(inout) :: self
            type(type_constant_id), intent(in) :: op
            integer(int32), intent(in) :: index
            integer(int32), intent(in) :: row_block, col_block
            real(real64), intent(in) :: value
        end subroutine abst_set_by_index

        !>
        !> Scales the matrix by a given scalar value.
        subroutine abst_scale(self, op, alpha)
            import :: abst_matrix, type_constant_id, int32, type_vector_dp
            implicit none
            !> The matrix object.
            class(abst_matrix), intent(inout) :: self
            !> The operation to perform
            type(type_constant_id), intent(in) :: op
            !> The scaling factor.
            type(type_vector_dp), intent(in) :: alpha
        end subroutine abst_scale

        !>
        !> Sets all stored entries in the matrix to zero.
        subroutine abst_zero_all(self)
            import :: abst_matrix
            implicit none
            !> The matrix object.
            class(abst_matrix), intent(inout) :: self
        end subroutine abst_zero_all

        !>
        !> Sets all entries in a specified row to zero.
        subroutine abst_zero_row(self, row, row_block)
            import :: abst_matrix, type_constant_id, int32
            implicit none
            !> The matrix object.
            class(abst_matrix), intent(inout) :: self
            !> The 1-based node index for the row.
            integer(int32), intent(in) :: row
            !> The block row index.
            integer(int32), intent(in), optional :: row_block
        end subroutine abst_zero_row

        !> Finds the storage index of a matrix entry.
        pure function abst_find(self, row, col) result(index)
            import :: abst_matrix, type_constant_id, int32
            implicit none
            class(abst_matrix), intent(in) :: self
            integer(int32), intent(in) :: row, col
            integer(int32) :: index
        end function abst_find

        !>
        !> Prints the matrix contents to standard output for debugging.
        subroutine abst_display(self, unit_in)
            import :: abst_matrix, type_constant_id, int32
            implicit none
            !> The matrix object.
            class(abst_matrix), intent(in) :: self
            !> The output unit.
            integer(int32), intent(in), optional :: unit_in
        end subroutine abst_display

        !>
        !> Deallocates all memory associated with the matrix.
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
    type, extends(abst_matrix) :: type_matrix_dense
        !> The number of rows in the matrix.
        integer(int32) :: num_rows = 0
        !> The number of columns in the matrix.
        integer(int32) :: num_cols = 0
        !> The 2D array holding the matrix values.
        real(real64), allocatable :: val(:, :)
    contains
        procedure, pass(self) :: initialize => initialize_dense
        procedure, pass(self) :: destroy => destroy_dense
        procedure, pass(self) :: get_info => get_info_dense
        procedure, pass(self) :: get_diagonal => get_diagonal_dense
        procedure, pass(self) :: get_val => get_val_dense
        procedure, pass(self) :: set_value => set_value_dense
        procedure, pass(self) :: set_value_block => set_value_block_dense
        procedure, pass(self) :: set_row => set_row_dense
        procedure, pass(self) :: set_all => set_all_dense
        procedure, pass(self) :: set_by_index => set_by_index_dense
        procedure, pass(self) :: scale => scale_dense
        procedure, pass(self) :: find => find_dense
        procedure, pass(self) :: zero_all => zero_all_dense
        procedure, pass(self) :: zero_row => zero_row_dense
        procedure, pass(self) :: display => display_dense
    end type type_matrix_dense

    interface
        !> Initializes a dense matrix.
        module subroutine initialize_dense(self, num_nodes, row, col, row_blocks, col_blocks)
            implicit none
            class(type_matrix_dense), intent(inout) :: self
            integer(int32), intent(in) :: num_nodes
            integer(int32), intent(in), optional :: row(:)
            integer(int32), intent(in), optional :: col(:)
            integer(int32), intent(in), optional :: row_blocks
            integer(int32), intent(in), optional :: col_blocks
        end subroutine initialize_dense

        !> Deallocates the dense matrix.
        module subroutine destroy_dense(self)
            implicit none
            class(type_matrix_dense), intent(inout) :: self
        end subroutine destroy_dense

        module subroutine get_info_dense(self, info)
            implicit none
            !> The matrix object.
            class(type_matrix_dense), intent(in) :: self
            !> The matrix information structure to populate.
            type(type_matrix_info), intent(inout) :: info
        end subroutine get_info_dense

        module subroutine get_diagonal_dense(self, diagonal)
            implicit none
            class(type_matrix_dense), intent(in) :: self
            !> The diagonal entries of the matrix.
            type(type_vector_dp), intent(inout) :: diagonal
        end subroutine get_diagonal_dense

        !> Returns a pointer to the dense matrix value array.
        module function get_val_dense(self) result(val)
            implicit none
            class(type_matrix_dense), intent(in), target :: self
            real(real64), dimension(:, :), pointer :: val
        end function get_val_dense

        !> Sets a single value in the dense matrix.
        module subroutine set_value_dense(self, op, row, col, value)
            implicit none
            class(type_matrix_dense), intent(inout) :: self
            type(type_constant_id), intent(in) :: op
            integer(int32), intent(in) :: row, col
            real(real64), intent(in) :: value
        end subroutine set_value_dense

        !> Sets a single value in the dense matrix.
        module subroutine set_value_block_dense(self, op, row, col, row_block, col_block, value)
            implicit none
            class(type_matrix_dense), intent(inout) :: self
            type(type_constant_id), intent(in) :: op
            integer(int32), intent(in) :: row, col
            integer(int32), intent(in) :: row_block, col_block
            real(real64), intent(in) :: value
        end subroutine set_value_block_dense

        !> Sets all values in a specified row of the dense matrix.
        module subroutine set_row_dense(self, op, row, value, row_block)
            implicit none
            class(type_matrix_dense), intent(inout) :: self
            type(type_constant_id), intent(in) :: op
            integer(int32), intent(in) :: row
            real(real64), intent(in) :: value
            integer(int32), intent(in), optional :: row_block
        end subroutine set_row_dense

        !> Sets all values in the dense matrix to a single scalar value.
        module subroutine set_all_dense(self, op, value)
            class(type_matrix_dense), intent(inout) :: self
            type(type_constant_id), intent(in) :: op
            real(real64), intent(in) :: value
        end subroutine set_all_dense

        module subroutine set_by_index_dense(self, op, index, row_block, col_block, value)
            class(type_matrix_dense), intent(inout) :: self
            type(type_constant_id), intent(in) :: op
            integer(int32), intent(in) :: index
            integer(int32), intent(in) :: row_block, col_block
            real(real64), intent(in) :: value
        end subroutine set_by_index_dense

        !> Scales all values in the dense matrix by a scalar factor.
        module subroutine scale_dense(self, op, alpha)
            implicit none
            class(type_matrix_dense), intent(inout) :: self
            type(type_constant_id), intent(in) :: op
            type(type_vector_dp), intent(in) :: alpha
        end subroutine scale_dense

        !> Sets all values in the dense matrix to zero.
        module subroutine zero_all_dense(self)
            implicit none
            class(type_matrix_dense), intent(inout) :: self
        end subroutine zero_all_dense

        !> Sets all values in a specified row of the dense matrix to zero.
        module subroutine zero_row_dense(self, row, row_block)
            implicit none
            class(type_matrix_dense), intent(inout) :: self
            integer(int32), intent(in) :: row
            integer(int32), intent(in), optional :: row_block
        end subroutine zero_row_dense

        !> Finds the storage index of a matrix entry.
        module pure function find_dense(self, row, col) result(index)
            implicit none
            class(type_matrix_dense), intent(in) :: self
            integer(int32), intent(in) :: row, col
            integer(int32) :: index
        end function find_dense

        !> Displays the contents of the dense matrix.
        module subroutine display_dense(self, unit_in)
            implicit none
            class(type_matrix_dense), intent(in) :: self
            integer(int32), intent(in), optional :: unit_in
        end subroutine display_dense
    end interface

    ! ==========================================================
    ! CRS Matrix Implementation
    ! ==========================================================

    !>
    !> Represents a sparse matrix in Compressed Row Storage (CRS) format.
    !>
    type, extends(abst_matrix) :: type_matrix_csr
        !> Number of non-zero elements.
        integer(int32) :: nnz = 0
        !> Number of rows.
        integer(int32) :: num_rows = 0
        !> Number of entries in the pointer array (num_rows + 1).
        integer(int32) :: num_ptrs = 0
        !> Pointers to the start of each row in the `ind` and `val` arrays.
        integer(int32), allocatable :: ptr(:)
        !> Column indices of the non-zero elements.
        integer(int32), allocatable :: ind(:)
        !> Values of the non-zero elements.
        real(real64), allocatable :: val(:)
    contains
        procedure, pass(self) :: initialize => initialize_type_matrix_csr
        procedure, pass(self) :: destroy => destroy_csr
        procedure, pass(self) :: get_info => get_info_csr
        procedure, pass(self) :: get_diagonal => get_diagonal_csr
        procedure, pass(self) :: get_ptr => get_ptr_csr
        procedure, pass(self) :: get_ind => get_ind_csr
        procedure, pass(self) :: get_val => get_val_csr
        procedure, pass(self) :: set_value => set_value_csr
        procedure, pass(self) :: set_value_block => set_value_block_csr
        procedure, pass(self) :: set_row => set_row_csr
        procedure, pass(self) :: set_all => set_all_csr
        procedure, pass(self) :: set_by_index => set_by_index_csr
        procedure, pass(self) :: scale => scale_csr
        procedure, pass(self) :: zero_all => zero_all_csr
        procedure, pass(self) :: zero_row => zero_row_csr
        procedure, pass(self), private :: find => find_csr
        procedure, pass(self) :: display => display_csr
    end type type_matrix_csr

    interface
        !> Initializes a CRS matrix from a node-level sparsity pattern.
        module subroutine initialize_type_matrix_csr(self, num_nodes, row, col, row_blocks, col_blocks)
            implicit none
            class(type_matrix_csr), intent(inout) :: self
            integer(int32), intent(in) :: num_nodes
            integer(int32), intent(in), optional :: row(:)
            integer(int32), intent(in), optional :: col(:)
            integer(int32), intent(in), optional :: row_blocks
            integer(int32), intent(in), optional :: col_blocks
        end subroutine initialize_type_matrix_csr

        !> Deallocates the CRS matrix.
        module subroutine destroy_csr(self)
            implicit none
            class(type_matrix_csr), intent(inout) :: self
        end subroutine destroy_csr

        module subroutine get_info_csr(self, info)
            implicit none
            !> The matrix object.
            class(type_matrix_csr), intent(in) :: self
            !> The matrix information structure to populate.
            type(type_matrix_info), intent(inout) :: info
        end subroutine get_info_csr

        module subroutine get_diagonal_csr(self, diagonal)
            implicit none
            class(type_matrix_csr), intent(in) :: self
            type(type_vector_dp), intent(inout) :: diagonal
        end subroutine get_diagonal_csr

        !> Returns a pointer to the `ptr` array.
        module function get_ptr_csr(self) result(ptr)
            implicit none
            class(type_matrix_csr), intent(in), target :: self
            integer(int32), dimension(:), pointer :: ptr
        end function get_ptr_csr

        !> Returns a pointer to the `ind` array.
        module function get_ind_csr(self) result(ind)
            implicit none
            class(type_matrix_csr), intent(in), target :: self
            integer(int32), dimension(:), pointer :: ind
        end function get_ind_csr

        !> Returns a pointer to the `val` array.
        module function get_val_csr(self) result(val)
            implicit none
            class(type_matrix_csr), intent(in), target :: self
            real(real64), dimension(:), pointer :: val
        end function get_val_csr

        !> Finds the storage index of a matrix entry.
        module pure function find_csr(self, row, col) result(index)
            implicit none
            class(type_matrix_csr), intent(in) :: self
            integer(int32), intent(in) :: row, col
            integer(int32) :: index
        end function find_csr

        !> Sets a single value in the matrix.
        module subroutine set_value_csr(self, op, row, col, value)
            implicit none
            class(type_matrix_csr), intent(inout) :: self
            type(type_constant_id), intent(in) :: op
            integer(int32), intent(in) :: row, col
            real(real64), intent(in) :: value
        end subroutine set_value_csr

        !> Sets a single value in the matrix.
        module subroutine set_value_block_csr(self, op, row, col, row_block, col_block, value)
            implicit none
            class(type_matrix_csr), intent(inout) :: self
            type(type_constant_id), intent(in) :: op
            integer(int32), intent(in) :: row, col
            integer(int32), intent(in) :: row_block, col_block
            real(real64), intent(in) :: value
        end subroutine set_value_block_csr

        !> Sets all values in a row.
        module subroutine set_row_csr(self, op, row, value, row_block)
            implicit none
            class(type_matrix_csr), intent(inout) :: self
            type(type_constant_id), intent(in) :: op
            integer(int32), intent(in) :: row
            real(real64), intent(in) :: value
            integer(int32), intent(in), optional :: row_block
        end subroutine set_row_csr

        !> Sets all stored values in the matrix.
        module subroutine set_all_csr(self, op, value)
            implicit none
            class(type_matrix_csr), intent(inout) :: self
            type(type_constant_id), intent(in) :: op
            real(real64), intent(in) :: value
        end subroutine set_all_csr

        !> Sets a value at a specific index.
        module subroutine set_by_index_csr(self, op, index, row_block, col_block, value)
            implicit none
            class(type_matrix_csr), intent(inout) :: self
            type(type_constant_id), intent(in) :: op
            integer(int32), intent(in) :: index
            integer(int32), intent(in) :: row_block, col_block
            real(real64), intent(in) :: value
        end subroutine set_by_index_csr

        !> Scales all stored matrix values by a scalar factor.
        module subroutine scale_csr(self, op, alpha)
            implicit none
            class(type_matrix_csr), intent(inout) :: self
            type(type_constant_id), intent(in) :: op
            type(type_vector_dp), intent(in) :: alpha
        end subroutine scale_csr

        !> Sets all stored matrix values to zero.
        module subroutine zero_all_csr(self)
            implicit none
            class(type_matrix_csr), intent(inout) :: self
        end subroutine zero_all_csr

        !> Sets all values in a specified row to zero.
        module subroutine zero_row_csr(self, row, row_block)
            implicit none
            class(type_matrix_csr), intent(inout) :: self
            integer(int32), intent(in) :: row
            integer(int32), intent(in), optional :: row_block
        end subroutine zero_row_csr

        !> Displays the matrix contents.
        module subroutine display_csr(self, unit_in)
            implicit none
            class(type_matrix_csr), intent(in) :: self
            integer(int32), intent(in), optional :: unit_in
        end subroutine display_csr
    end interface

    ! ==========================================================
    ! COO Matrix Implementation
    ! ==========================================================

    !>
    !> Represents a sparse matrix in Coordinate (COO) list format.
    !>
    type, extends(abst_matrix) :: type_matrix_coo
        !> Number of non-zero elements.
        integer(int32) :: nnz = 0
        !> Number of rows.
        integer(int32) :: num_rows = 0
        !> Number of columns.
        integer(int32) :: num_cols = 0
        !> Row indices of the non-zero elements.
        integer(int32), allocatable :: row(:)
        !> Column indices of the non-zero elements.
        integer(int32), allocatable :: col(:)
        !> Values of the non-zero elements.
        real(real64), allocatable :: val(:)
    contains
        procedure, pass(self) :: initialize => initialize_type_matrix_coo
        procedure, pass(self) :: destroy => destroy_coo
        procedure, pass(self) :: get_info => get_info_coo
        procedure, pass(self) :: get_diagonal => get_diagonal_coo
        procedure, pass(self) :: get_row => get_row_coo
        procedure, pass(self) :: get_col => get_col_coo
        procedure, pass(self) :: get_val => get_val_coo
        procedure, pass(self) :: set_value => set_value_coo
        procedure, pass(self) :: set_value_block => set_value_block_coo
        procedure, pass(self) :: set_row => set_row_coo
        procedure, pass(self) :: set_all => set_all_coo
        procedure, pass(self) :: set_by_index => set_by_index_coo
        procedure, pass(self) :: scale => scale_coo
        procedure, pass(self) :: zero_all => zero_all_coo
        procedure, pass(self) :: zero_row => zero_row_coo
        procedure, private, pass(self) :: find => find_coo
        procedure, pass(self) :: display => display_coo
    end type type_matrix_coo

    interface
        !> Initializes a COO matrix from a node-level sparsity pattern.
        module subroutine initialize_type_matrix_coo(self, num_nodes, row, col, row_blocks, col_blocks)
            implicit none
            class(type_matrix_coo), intent(inout) :: self
            integer(int32), intent(in) :: num_nodes
            integer(int32), intent(in), optional :: row(:)
            integer(int32), intent(in), optional :: col(:)
            integer(int32), intent(in), optional :: row_blocks
            integer(int32), intent(in), optional :: col_blocks
        end subroutine initialize_type_matrix_coo

        !> Deallocates the COO matrix.
        module subroutine destroy_coo(self)
            implicit none
            class(type_matrix_coo), intent(inout) :: self
        end subroutine destroy_coo

        module subroutine get_info_coo(self, info)
            implicit none
            !> The matrix object.
            class(type_matrix_coo), intent(in) :: self
            !> The matrix information structure to populate.
            type(type_matrix_info), intent(inout) :: info
        end subroutine get_info_coo

        module subroutine get_diagonal_coo(self, diagonal)
            implicit none
            class(type_matrix_coo), intent(in) :: self
            type(type_vector_dp), intent(inout) :: diagonal
        end subroutine get_diagonal_coo

        !> Returns a pointer to the `row` index array.
        module function get_row_coo(self) result(row)
            implicit none
            class(type_matrix_coo), intent(in), target :: self
            integer(int32), dimension(:), pointer :: row
        end function get_row_coo

        !> Returns a pointer to the `col` index array.
        module function get_col_coo(self) result(col)
            implicit none
            class(type_matrix_coo), intent(in), target :: self
            integer(int32), dimension(:), pointer :: col
        end function get_col_coo

        !> Returns a pointer to the `val` array.
        module function get_val_coo(self) result(val)
            implicit none
            class(type_matrix_coo), intent(in), target :: self
            real(real64), dimension(:), pointer :: val
        end function get_val_coo

        !> Finds the storage index of a matrix entry.
        module pure function find_coo(self, row, col) result(index)
            implicit none
            class(type_matrix_coo), intent(in) :: self
            integer(int32), intent(in) :: row, col
            integer(int32) :: index
        end function find_coo

        !> Sets a single value in the matrix.
        module subroutine set_value_coo(self, op, row, col, value)
            implicit none
            class(type_matrix_coo), intent(inout) :: self
            type(type_constant_id), intent(in) :: op
            integer(int32), intent(in) :: row, col
            real(real64), intent(in) :: value
        end subroutine set_value_coo

        !> Sets a single value in the matrix.
        module subroutine set_value_block_coo(self, op, row, col, row_block, col_block, value)
            implicit none
            class(type_matrix_coo), intent(inout) :: self
            type(type_constant_id), intent(in) :: op
            integer(int32), intent(in) :: row, col
            integer(int32), intent(in) :: row_block, col_block
            real(real64), intent(in) :: value
        end subroutine set_value_block_coo

        !> Sets all values in a row.
        module subroutine set_row_coo(self, op, row, value, row_block)
            implicit none
            class(type_matrix_coo), intent(inout) :: self
            type(type_constant_id), intent(in) :: op
            integer(int32), intent(in) :: row
            real(real64), intent(in) :: value
            integer(int32), intent(in), optional :: row_block
        end subroutine set_row_coo

        !> Sets all stored values in the matrix.
        module subroutine set_all_coo(self, op, value)
            implicit none
            class(type_matrix_coo), intent(inout) :: self
            type(type_constant_id), intent(in) :: op
            real(real64), intent(in) :: value
        end subroutine set_all_coo

        !> Sets a value at a specific index.
        module subroutine set_by_index_coo(self, op, index, row_block, col_block, value)
            implicit none
            class(type_matrix_coo), intent(inout) :: self
            type(type_constant_id), intent(in) :: op
            integer(int32), intent(in) :: index
            integer(int32), intent(in) :: row_block, col_block
            real(real64), intent(in) :: value
        end subroutine set_by_index_coo

        !> Scales all stored matrix values by a scalar factor.
        module subroutine scale_coo(self, op, alpha)
            implicit none
            class(type_matrix_coo), intent(inout) :: self
            type(type_constant_id), intent(in) :: op
            type(type_vector_dp), intent(in) :: alpha
        end subroutine scale_coo

        !> Sets all stored matrix values to zero.
        module subroutine zero_all_coo(self)
            implicit none
            class(type_matrix_coo), intent(inout) :: self
        end subroutine zero_all_coo

        !> Sets all values in a specified row to zero.
        module subroutine zero_row_coo(self, row, row_block)
            implicit none
            class(type_matrix_coo), intent(inout) :: self
            integer(int32), intent(in) :: row
            integer(int32), intent(in), optional :: row_block
        end subroutine zero_row_coo

        !> Displays the matrix contents.
        module subroutine display_coo(self, unit_in)
            implicit none
            class(type_matrix_coo), intent(in) :: self
            integer(int32), intent(in), optional :: unit_in
        end subroutine display_coo
    end interface

    type, extends(abst_matrix) :: type_matrix_bsr
        !> Number of non-zero elements.
        integer(int32) :: nnz = 0
        !> Number of rows.
        integer(int32) :: num_rows = 0
        !> Number of entries in the pointer array (num_rows + 1).
        integer(int32) :: num_ptrs = 0
        !> Number of blocks.
        integer(int32) :: num_blocks = 0
        !> Number of block rows.
        integer(int32) :: num_block_rows = 0
        !> Number of block columns.
        integer(int32) :: num_block_cols = 0
        !> Pointers to the start of each row in the `ind` and `val` arrays.
        integer(int32), allocatable :: ptr(:)
        !> Column indices of the non-zero elements.
        integer(int32), allocatable :: ind(:)
        !> Values of the non-zero elements stored in blocks. (row_blocks, col_blocks, index)
        real(real64), allocatable :: val(:, :, :)
    contains
        procedure, pass(self) :: initialize => initialize_type_matrix_bsr
        procedure, pass(self) :: destroy => destroy_bsr
        procedure, pass(self) :: get_info => get_info_bsr
        procedure, pass(self) :: get_diagonal => get_diagonal_bsr
        procedure, pass(self), public :: get_diagonal_block => get_diagonal_block_bsr
        procedure, pass(self) :: get_ptr => get_ptr_bsr
        procedure, pass(self) :: get_ind => get_ind_bsr
        procedure, pass(self) :: get_val => get_val_bsr
        procedure, pass(self) :: set_value => set_value_bsr
        procedure, pass(self) :: set_value_block => set_value_block_bsr
        procedure, pass(self) :: set_row => set_row_bsr
        procedure, pass(self) :: set_all => set_all_bsr
        procedure, pass(self) :: set_by_index => set_by_index_bsr
        procedure, pass(self) :: scale => scale_bsr
        procedure, pass(self) :: zero_all => zero_all_bsr
        procedure, pass(self) :: zero_row => zero_row_bsr
        procedure, pass(self), private :: find => find_bsr
        procedure, pass(self) :: display => display_bsr
    end type type_matrix_bsr

    interface
        module subroutine initialize_type_matrix_bsr(self, num_nodes, row, col, row_blocks, col_blocks)
            implicit none
            class(type_matrix_bsr), intent(inout) :: self
            integer(int32), intent(in) :: num_nodes
            integer(int32), intent(in), optional :: row(:)
            integer(int32), intent(in), optional :: col(:)
            integer(int32), intent(in), optional :: row_blocks
            integer(int32), intent(in), optional :: col_blocks
        end subroutine initialize_type_matrix_bsr

        module subroutine destroy_bsr(self)
            implicit none
            class(type_matrix_bsr), intent(inout) :: self
        end subroutine destroy_bsr

        module subroutine get_info_bsr(self, info)
            implicit none
            !> The matrix object.
            class(type_matrix_bsr), intent(in) :: self
            !> The matrix information structure to populate.
            type(type_matrix_info), intent(inout) :: info
        end subroutine get_info_bsr

        module subroutine get_diagonal_bsr(self, diagonal)
            implicit none
            class(type_matrix_bsr), intent(in) :: self
            type(type_vector_dp), intent(inout) :: diagonal
        end subroutine get_diagonal_bsr

        module function get_ptr_bsr(self) result(ptr)
            implicit none
            class(type_matrix_bsr), intent(in), target :: self
            integer(int32), dimension(:), pointer :: ptr
        end function get_ptr_bsr

        module function get_ind_bsr(self) result(ind)
            implicit none
            class(type_matrix_bsr), intent(in), target :: self
            integer(int32), dimension(:), pointer :: ind
        end function get_ind_bsr

        module function get_val_bsr(self) result(val)
            implicit none
            class(type_matrix_bsr), intent(in), target :: self
            real(real64), dimension(:, :, :), pointer :: val
        end function get_val_bsr

        module subroutine get_diagonal_block_bsr(self, target_block, diagonal_block)
            implicit none
            class(type_matrix_bsr), intent(in) :: self
            integer(int32), intent(in) :: target_block
            real(real64), intent(inout) :: diagonal_block(:, :)
        end subroutine get_diagonal_block_bsr

        module pure function find_bsr(self, row, col) result(index)
            implicit none
            class(type_matrix_bsr), intent(in) :: self
            integer(int32), intent(in) :: row, col
            integer(int32) :: index
        end function find_bsr

        module subroutine set_value_bsr(self, op, row, col, value)
            implicit none
            class(type_matrix_bsr), intent(inout) :: self
            type(type_constant_id), intent(in) :: op
            integer(int32), intent(in) :: row, col
            real(real64), intent(in) :: value
        end subroutine set_value_bsr

        module subroutine set_value_block_bsr(self, op, row, col, row_block, col_block, value)
            implicit none
            class(type_matrix_bsr), intent(inout) :: self
            type(type_constant_id), intent(in) :: op
            integer(int32), intent(in) :: row, col
            integer(int32), intent(in) :: row_block, col_block
            real(real64), intent(in) :: value
        end subroutine set_value_block_bsr

        module subroutine set_row_bsr(self, op, row, value, row_block)
            implicit none
            class(type_matrix_bsr), intent(inout) :: self
            type(type_constant_id), intent(in) :: op
            integer(int32), intent(in) :: row
            real(real64), intent(in) :: value
            integer(int32), intent(in), optional :: row_block
        end subroutine set_row_bsr

        module subroutine set_all_bsr(self, op, value)
            implicit none
            class(type_matrix_bsr), intent(inout) :: self
            type(type_constant_id), intent(in) :: op
            real(real64), intent(in) :: value
        end subroutine set_all_bsr

        module subroutine set_by_index_bsr(self, op, index, row_block, col_block, value)
            implicit none
            class(type_matrix_bsr), intent(inout) :: self
            type(type_constant_id), intent(in) :: op
            integer(int32), intent(in) :: index
            integer(int32), intent(in) :: row_block, col_block
            real(real64), intent(in) :: value
        end subroutine set_by_index_bsr

        module subroutine scale_bsr(self, op, alpha)
            implicit none
            class(type_matrix_bsr), intent(inout) :: self
            type(type_constant_id), intent(in) :: op
            type(type_vector_dp), intent(in) :: alpha
        end subroutine scale_bsr

        module subroutine zero_all_bsr(self)
            implicit none
            class(type_matrix_bsr), intent(inout) :: self
        end subroutine zero_all_bsr

        module subroutine zero_row_bsr(self, row, row_block)
            implicit none
            class(type_matrix_bsr), intent(inout) :: self
            integer(int32), intent(in) :: row
            integer(int32), intent(in), optional :: row_block
        end subroutine zero_row_bsr

        module subroutine display_bsr(self, unit_in)
            implicit none
            class(type_matrix_bsr), intent(in) :: self
            integer(int32), intent(in), optional :: unit_in
        end subroutine display_bsr
    end interface

    ! ==========================================================
    ! DIA Matrix Implementation
    ! ==========================================================

    !>
    !> Represents a sparse matrix in Diagonal (DIA) storage format.
    !> Stores values in a 2D array where each column represents a diagonal.
    !>
    type, extends(abst_matrix) :: type_matrix_dia
        !> Number of non-zero elements.
        integer(int32) :: nnz = 0
        !> Number of rows.
        integer(int32) :: num_rows = 0
        !> Number of columns.
        integer(int32) :: num_cols = 0
        !> Number of diagonals stored.
        integer(int32) :: num_diags = 0
        !> Offsets of the diagonals from the main diagonal.
        !> 0: main, <0: lower, >0: upper.
        integer(int32), allocatable :: offsets(:)
        !> Matrix values stored as (row, diagonal_index).
        real(real64), allocatable :: val(:, :)
    contains
        procedure, pass(self) :: initialize => initialize_type_matrix_dia
        procedure, pass(self) :: destroy => destroy_dia
        procedure, pass(self) :: get_info => get_info_dia
        procedure, pass(self) :: get_diagonal => get_diagonal_dia
        procedure, pass(self) :: get_val => get_val_dia
        procedure, pass(self) :: get_offsets => get_offsets_dia
        procedure, pass(self) :: set_value => set_value_dia
        procedure, pass(self) :: set_value_block => set_value_block_dia
        procedure, pass(self) :: set_row => set_row_dia
        procedure, pass(self) :: set_all => set_all_dia
        procedure, pass(self) :: set_by_index => set_by_index_dia
        procedure, pass(self) :: scale => scale_dia
        procedure, pass(self) :: zero_all => zero_all_dia
        procedure, pass(self) :: zero_row => zero_row_dia
        procedure, pass(self), private :: find => find_dia
        procedure, pass(self) :: display => display_dia
    end type type_matrix_dia

    interface
        !> Initializes a DIA matrix from a node-level sparsity pattern.
        module subroutine initialize_type_matrix_dia(self, num_nodes, row, col, row_blocks, col_blocks)
            implicit none
            class(type_matrix_dia), intent(inout) :: self
            integer(int32), intent(in) :: num_nodes
            integer(int32), intent(in), optional :: row(:)
            integer(int32), intent(in), optional :: col(:)
            integer(int32), intent(in), optional :: row_blocks
            integer(int32), intent(in), optional :: col_blocks
        end subroutine initialize_type_matrix_dia

        !> Deallocates the DIA matrix.
        module subroutine destroy_dia(self)
            implicit none
            class(type_matrix_dia), intent(inout) :: self
        end subroutine destroy_dia

        module subroutine get_info_dia(self, info)
            implicit none
            class(type_matrix_dia), intent(in) :: self
            type(type_matrix_info), intent(inout) :: info
        end subroutine get_info_dia

        module subroutine get_diagonal_dia(self, diagonal)
            implicit none
            class(type_matrix_dia), intent(in) :: self
            type(type_vector_dp), intent(inout) :: diagonal
        end subroutine get_diagonal_dia

        !> Returns a pointer to the `val` array.
        module function get_val_dia(self) result(val)
            implicit none
            class(type_matrix_dia), intent(in), target :: self
            real(real64), dimension(:, :), pointer :: val
        end function get_val_dia

        !> Returns a pointer to the `offsets` array.
        module function get_offsets_dia(self) result(offsets)
            implicit none
            class(type_matrix_dia), intent(in), target :: self
            integer(int32), dimension(:), pointer :: offsets
        end function get_offsets_dia

        !> Finds the storage index (diagonal index) of a matrix entry.
        module pure function find_dia(self, row, col) result(index)
            implicit none
            class(type_matrix_dia), intent(in) :: self
            integer(int32), intent(in) :: row, col
            integer(int32) :: index
        end function find_dia

        module subroutine set_value_dia(self, op, row, col, value)
            implicit none
            class(type_matrix_dia), intent(inout) :: self
            type(type_constant_id), intent(in) :: op
            integer(int32), intent(in) :: row, col
            real(real64), intent(in) :: value
        end subroutine set_value_dia

        module subroutine set_value_block_dia(self, op, row, col, row_block, col_block, value)
            implicit none
            class(type_matrix_dia), intent(inout) :: self
            type(type_constant_id), intent(in) :: op
            integer(int32), intent(in) :: row, col
            integer(int32), intent(in) :: row_block, col_block
            real(real64), intent(in) :: value
        end subroutine set_value_block_dia

        module subroutine set_row_dia(self, op, row, value, row_block)
            implicit none
            class(type_matrix_dia), intent(inout) :: self
            type(type_constant_id), intent(in) :: op
            integer(int32), intent(in) :: row
            real(real64), intent(in) :: value
            integer(int32), intent(in), optional :: row_block
        end subroutine set_row_dia

        module subroutine set_all_dia(self, op, value)
            implicit none
            class(type_matrix_dia), intent(inout) :: self
            type(type_constant_id), intent(in) :: op
            real(real64), intent(in) :: value
        end subroutine set_all_dia

        module subroutine set_by_index_dia(self, op, index, row_block, col_block, value)
            implicit none
            class(type_matrix_dia), intent(inout) :: self
            type(type_constant_id), intent(in) :: op
            integer(int32), intent(in) :: index
            integer(int32), intent(in) :: row_block, col_block
            real(real64), intent(in) :: value
        end subroutine set_by_index_dia

        module subroutine scale_dia(self, op, alpha)
            implicit none
            class(type_matrix_dia), intent(inout) :: self
            type(type_constant_id), intent(in) :: op
            type(type_vector_dp), intent(in) :: alpha
        end subroutine scale_dia

        module subroutine zero_all_dia(self)
            implicit none
            class(type_matrix_dia), intent(inout) :: self
        end subroutine zero_all_dia

        module subroutine zero_row_dia(self, row, row_block)
            implicit none
            class(type_matrix_dia), intent(inout) :: self
            integer(int32), intent(in) :: row
            integer(int32), intent(in), optional :: row_block
        end subroutine zero_row_dia

        module subroutine display_dia(self, unit_in)
            implicit none
            class(type_matrix_dia), intent(in) :: self
            integer(int32), intent(in), optional :: unit_in
        end subroutine display_dia
    end interface

contains
    pure function get_status(self) result(status)
        implicit none
        class(abst_matrix), intent(in) :: self
        type(type_constant_id) :: status

        status = self%status
    end function get_status

    pure function is_initialized(self) result(initialized)
        implicit none
        class(abst_matrix), intent(in) :: self
        logical :: initialized

        initialized = self%is_initialized_matrix
    end function is_initialized

    subroutine check_matrix_status(self)
        implicit none
        class(abst_matrix), intent(in) :: self

        if (self%status /= MATRIX_STATUS%SUCCESS) then
            write (*, *) "Matrix status indicates an error: ", self%status
            stop "Matrix check failed."
        end if

    end subroutine check_matrix_status

end module core_types_algebra_matrix
