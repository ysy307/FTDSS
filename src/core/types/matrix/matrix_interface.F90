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

    ! Abstract base type for matrices.
    type, abstract :: abst_matrix
        private
        integer(int32) :: num_nodes = 0
        integer(int32) :: num_dofs = 0
    contains
        procedure(abst_initialize), public, pass(self), deferred :: initialize
        procedure(abst_destroy), public, pass(self), deferred :: destroy

        procedure(abst_set_value), private, pass(self), deferred :: set_value
        procedure(abst_set_row), private, pass(self), deferred :: set_row
        procedure(abst_set_all), private, pass(self), deferred :: set_all
        generic, public :: set => set_value, set_row, set_all

        procedure(abst_zero), public, pass(self), deferred :: zero
        procedure(abst_add_value), private, pass(self), deferred :: add_value
        procedure(abst_add_matrix), private, pass(self), deferred :: add_matrix
        generic, public :: add => add_value, add_matrix

        procedure(abst_gemv), public, pass(self), deferred :: gemv

        procedure(abst_display), public, pass(self), deferred :: display
    end type abst_matrix

    abstract interface
        subroutine abst_initialize(self, num_nodes, num_dofs, row, col)
            import :: abst_matrix, int32
            implicit none
            class(abst_matrix), intent(inout) :: self
            integer(int32), intent(in) :: num_nodes
            integer(int32), intent(in) :: num_dofs
            integer(int32), intent(in), optional :: row(:)
            integer(int32), intent(in), optional :: col(:)

        end subroutine abst_initialize

        subroutine abst_set_value(self, row_dof, col_dof, row, col, value)
            import :: abst_matrix, int32, real64
            implicit none
            class(abst_matrix), intent(inout) :: self
            integer(int32), intent(in) :: row_dof, col_dof, row, col
            real(real64), intent(in) :: value

        end subroutine abst_set_value

        subroutine abst_set_row(self, row_dof, row, value)
            import :: abst_matrix, real64, int32
            implicit none
            class(abst_matrix), intent(inout) :: self
            integer(int32), intent(in) :: row_dof, row
            real(real64), intent(in) :: value

        end subroutine abst_set_row

        subroutine abst_set_all(self, value)
            import :: abst_matrix, real64
            implicit none
            class(abst_matrix), intent(inout) :: self
            real(real64), intent(in) :: value

        end subroutine abst_set_all

        subroutine abst_zero(self)
            import :: abst_matrix
            implicit none
            class(abst_matrix), intent(inout) :: self

        end subroutine abst_zero

        subroutine abst_add_value(self, row_dof, col_dof, row, col, value)
            import :: abst_matrix, int32, real64
            implicit none
            class(abst_matrix), intent(inout) :: self
            integer(int32), intent(in) :: row_dof, col_dof, row, col
            real(real64), intent(in) :: value

        end subroutine abst_add_value

        subroutine abst_add_matrix(self, alpha, B, C)
            import :: abst_matrix, real64
            implicit none
            class(abst_matrix), intent(in) :: self
            real(real64), intent(in) :: alpha
            class(abst_matrix), intent(in) :: B
            class(abst_matrix), intent(inout) :: C
        end subroutine abst_add_matrix

        subroutine abst_gemv(self, alpha, x, beta, y)
            import :: abst_matrix, real64
            implicit none
            class(abst_matrix), intent(in) :: self
            real(real64), intent(in) :: alpha
            real(real64), intent(in) :: x(:)
            real(real64), intent(in) :: beta
            real(real64), intent(inout) :: y(:)

        end subroutine abst_gemv

        subroutine abst_display(self)
            import :: abst_matrix
            implicit none
            class(abst_matrix), intent(in) :: self

        end subroutine abst_display

        subroutine abst_destroy(self)
            import :: abst_matrix
            implicit none
            class(abst_matrix), intent(inout) :: self

        end subroutine abst_destroy
    end interface

    type, extends(abst_matrix) :: type_dense
        integer(int32) :: num_row = 0
        integer(int32) :: num_col = 0
        real(real64), allocatable :: val(:, :)
    contains
        ! --- initialize/destroy ---
        procedure, pass(self) :: initialize => initialize_dense
        procedure, pass(self) :: destroy => destroy_dense

        ! --- getter ---
        procedure, pass(self) :: get_num_row => get_num_row_dense
        procedure, pass(self) :: get_num_col => get_num_col_dense
        procedure, pass(self) :: get_val => get_val_dense

        ! --- setter ---
        procedure, pass(self) :: set_value => set_value_dense
        procedure, pass(self) :: set_row => set_row_dense
        procedure, pass(self) :: set_all => set_all_dense

        ! --- zero ----
        procedure, pass(self) :: zero => zero_dense

        ! --- addition ---
        procedure, pass(self) :: add_value => add_value_dense
        procedure, pass(self) :: add_matrix => add_matrix_dense
        procedure, pass(self) :: gemv => gemv_dense

        ! --- display ---
        procedure, pass(self) :: display => display_dense
    end type type_dense

    interface
        module subroutine initialize_dense(self, num_nodes, num_dofs, row, col)
            implicit none
            class(type_dense), intent(inout) :: self
            integer(int32), intent(in) :: num_nodes
            integer(int32), intent(in) :: num_dofs
            integer(int32), intent(in), optional :: row(:)
            integer(int32), intent(in), optional :: col(:)

        end subroutine initialize_dense

        module subroutine destroy_dense(self)
            implicit none
            class(type_dense), intent(inout) :: self

        end subroutine destroy_dense

        module pure function get_num_row_dense(self) result(num_row)
            implicit none
            class(type_dense), intent(in) :: self
            integer(int32) :: num_row

        end function get_num_row_dense

        module pure function get_num_col_dense(self) result(num_col)
            implicit none
            class(type_dense), intent(in) :: self
            integer(int32) :: num_col

        end function get_num_col_dense

        module function get_val_dense(self) result(val)
            implicit none
            class(type_dense), intent(in), target :: self
            real(real64), dimension(:, :), pointer :: val

        end function get_val_dense

        module subroutine set_value_dense(self, row_dof, col_dof, row, col, value)
            implicit none
            class(type_dense), intent(inout) :: self
            integer(int32), intent(in) :: row_dof, col_dof, row, col
            real(real64), intent(in) :: value

        end subroutine set_value_dense

        module subroutine set_row_dense(self, row_dof, row, value)
            implicit none
            class(type_dense), intent(inout) :: self
            integer(int32), intent(in) :: row_dof, row
            real(real64), intent(in) :: value

        end subroutine set_row_dense

        module subroutine set_all_dense(self, value)
            class(type_dense), intent(inout) :: self
            real(real64), intent(in) :: value

        end subroutine set_all_dense

        module subroutine zero_dense(self)
            implicit none
            class(type_dense), intent(inout) :: self

        end subroutine zero_dense

        module subroutine add_value_dense(self, row_dof, col_dof, row, col, value)
            implicit none
            class(type_dense), intent(inout) :: self
            integer(int32), intent(in) :: row_dof, col_dof, row, col
            real(real64), intent(in) :: value

        end subroutine add_value_dense

        module subroutine add_matrix_dense(self, alpha, B, C)
            implicit none
            class(type_dense), intent(in) :: self
            real(real64), intent(in) :: alpha
            class(abst_matrix), intent(in) :: B
            class(abst_matrix), intent(inout) :: C

        end subroutine add_matrix_dense

        module subroutine gemv_dense(self, alpha, x, beta, y)
            implicit none
            class(type_dense), intent(in) :: self
            real(real64), intent(in) :: alpha
            real(real64), intent(in) :: x(:)
            real(real64), intent(in) :: beta
            real(real64), intent(inout) :: y(:)

        end subroutine gemv_dense

        module subroutine display_dense(self)
            implicit none
            class(type_dense), intent(in) :: self

        end subroutine display_dense
    end interface

    type, extends(abst_matrix) :: type_crs
        integer(int32) :: nnz = 0 ! number of non-zero elements
        integer(int32) :: num_row = 0 ! number of rows
        integer(int32) :: num_ptr = 0 ! number of pointers

        integer(int32), allocatable :: ptr(:) ! pointers to row starts (num_row + 1 entries)
        integer(int32), allocatable :: ind(:) ! column indices of non-zeros
        real(real64), allocatable :: val(:) ! non-zero values
    contains
        ! --- initialize/destroy ---
        procedure, pass(self) :: initialize => initialize_type_crs
        procedure, pass(self) :: destroy => destroy_crs

        ! --- getter ---
        procedure, pass(self) :: get_nnz => get_nnz_crs
        procedure, pass(self) :: get_num_ptr => get_num_ptr_crs
        procedure, pass(self) :: get_num_row => get_num_row_crs
        procedure, pass(self) :: get_ptr => get_ptr_crs
        procedure, pass(self) :: get_ind => get_ind_crs
        procedure, pass(self) :: get_val => get_val_crs

        ! --- setter ---
        procedure, pass(self) :: set_value => set_crs
        procedure, pass(self) :: set_row => set_row_crs
        procedure, pass(self) :: set_all => set_all_crs

        ! --- zero ----
        procedure, pass(self) :: zero => zero_crs

        procedure, private, pass(self) :: find => find_crs

        ! --- addition ---
        procedure, pass(self) :: add_value => add_crs
        procedure, pass(self) :: add_matrix => add_matrix_crs
        procedure, pass(self) :: gemv => gemv_crs

        ! --- display ---
        procedure, pass(self) :: display => display_crs

    end type type_crs

    interface
        module subroutine initialize_type_crs(self, num_nodes, num_dofs, row, col)
            implicit none
            class(type_crs), intent(inout) :: self
            integer(int32), intent(in) :: num_nodes
            integer(int32), intent(in) :: num_dofs
            integer(int32), intent(in), optional :: row(:)
            integer(int32), intent(in), optional :: col(:)

        end subroutine initialize_type_crs

        module subroutine destroy_crs(self)
            implicit none
            class(type_crs), intent(inout) :: self

        end subroutine destroy_crs

        module pure function get_nnz_crs(self) result(nnz)
            implicit none
            class(type_crs), intent(in) :: self
            integer(int32) :: nnz

        end function get_nnz_crs

        module pure function get_num_ptr_crs(self) result(num_ptr)
            implicit none
            class(type_crs), intent(in) :: self
            integer(int32) :: num_ptr

        end function get_num_ptr_crs

        module pure function get_num_row_crs(self) result(num_row)
            implicit none
            class(type_crs), intent(in) :: self
            integer(int32) :: num_row

        end function get_num_row_crs

        module function get_ptr_crs(self) result(ptr)
            implicit none
            class(type_crs), intent(in), target :: self
            integer(int32), dimension(:), pointer :: ptr

        end function get_ptr_crs

        module function get_ind_crs(self) result(ind)
            implicit none
            class(type_crs), intent(in), target :: self
            integer(int32), dimension(:), pointer :: ind

        end function get_ind_crs

        module function get_val_crs(self) result(val)
            implicit none
            class(type_crs), intent(in), target :: self
            real(real64), dimension(:), pointer :: val

        end function get_val_crs

        module pure function find_crs(self, row_dof, col_dof, row, col) result(index)
            implicit none
            class(type_crs), intent(in) :: self
            integer(int32), intent(in) :: row_dof, col_dof, row, col
            integer(int32) :: index

        end function find_crs

        module subroutine set_crs(self, row_dof, col_dof, row, col, value)
            implicit none
            class(type_crs), intent(inout) :: self
            integer(int32), intent(in) :: row_dof, col_dof, row, col
            real(real64), intent(in) :: value

        end subroutine set_crs

        module subroutine set_row_crs(self, row_dof, row, value)
            implicit none
            class(type_crs), intent(inout) :: self
            integer(int32), intent(in) :: row_dof, row
            real(real64), intent(in) :: value

        end subroutine set_row_crs

        module subroutine set_all_crs(self, value)
            implicit none
            class(type_crs), intent(inout) :: self
            real(real64), intent(in) :: value

        end subroutine set_all_crs

        module subroutine zero_crs(self)
            implicit none
            class(type_crs), intent(inout) :: self

        end subroutine zero_crs

        module subroutine add_crs(self, row_dof, col_dof, row, col, value)
            implicit none
            class(type_crs), intent(inout) :: self
            integer(int32), intent(in) :: row_dof, col_dof, row, col
            real(real64), intent(in) :: value

        end subroutine add_crs

        module subroutine add_matrix_crs(self, alpha, B, C)
            implicit none
            class(type_crs), intent(in) :: self ! This is matrix A
            real(real64), intent(in) :: alpha
            class(abst_matrix), intent(in) :: B
            class(abst_matrix), intent(inout) :: C

        end subroutine add_matrix_crs

        module subroutine gemv_crs(self, alpha, x, beta, y)
            implicit none
            class(type_crs), intent(in) :: self
            real(real64), intent(in) :: alpha
            real(real64), intent(in) :: x(:)
            real(real64), intent(in) :: beta
            real(real64), intent(inout) :: y(:)

        end subroutine gemv_crs

        module subroutine display_crs(self)
            implicit none
            class(type_crs), intent(in) :: self

        end subroutine display_crs

    end interface

    type, extends(abst_matrix) :: type_coo
        integer(int32) :: nnz = 0
        integer(int32) :: num_row = 0
        integer(int32) :: num_col = 0
        integer(int32), allocatable :: row(:)
        integer(int32), allocatable :: col(:)
        real(real64), allocatable :: val(:)
    contains
        ! --- initialize/destroy ---
        procedure, pass(self) :: initialize => initialize_type_coo
        procedure, pass(self) :: destroy => destroy_coo

        ! --- getter ---
        procedure, pass(self) :: get_nnz => get_nnz_coo
        procedure, pass(self) :: get_num_row => get_num_row_coo
        procedure, pass(self) :: get_num_col => get_num_col_coo
        procedure, pass(self) :: get_row => get_row_coo
        procedure, pass(self) :: get_col => get_col_coo
        procedure, pass(self) :: get_val => get_val_coo

        ! --- setter ---
        procedure, pass(self) :: set_value => set_coo
        procedure, pass(self) :: set_all => set_all_coo
        procedure, pass(self) :: set_row => set_row_coo

        procedure, private, pass(self) :: find => find_coo

        ! --- zero ----
        procedure, pass(self) :: zero => zero_coo

        ! --- addition ---
        procedure, pass(self) :: add_value => add_coo
        procedure, pass(self) :: add_matrix => add_matrix_coo
        procedure, pass(self) :: gemv => gemv_coo

        ! --- display ---
        procedure, pass(self) :: display => display_coo

    end type type_coo

    interface
        module subroutine initialize_type_coo(self, num_nodes, num_dofs, row, col)
            implicit none
            class(type_coo), intent(inout) :: self
            integer(int32), intent(in) :: num_nodes
            integer(int32), intent(in) :: num_dofs
            integer(int32), intent(in), optional :: row(:)
            integer(int32), intent(in), optional :: col(:)

        end subroutine initialize_type_coo

        module pure function get_nnz_coo(self) result(nnz)
            implicit none
            class(type_coo), intent(in) :: self
            integer(int32) :: nnz

        end function get_nnz_coo

        module pure function get_num_row_coo(self) result(num_row)
            implicit none
            class(type_coo), intent(in) :: self
            integer(int32) :: num_row

        end function get_num_row_coo

        module pure function get_num_col_coo(self) result(num_col)
            implicit none
            class(type_coo), intent(in) :: self
            integer(int32) :: num_col

        end function get_num_col_coo

        module function get_row_coo(self) result(row)
            implicit none
            class(type_coo), intent(in), target :: self
            integer(int32), dimension(:), pointer :: row

        end function get_row_coo

        module function get_col_coo(self) result(col)
            implicit none
            class(type_coo), intent(in), target :: self
            integer(int32), dimension(:), pointer :: col

        end function get_col_coo

        module function get_val_coo(self) result(val)
            implicit none
            class(type_coo), intent(in), target :: self
            real(real64), dimension(:), pointer :: val

        end function get_val_coo

        module pure function find_coo(self, row_dof, col_dof, row, col) result(index)
            implicit none
            class(type_coo), intent(in) :: self
            integer(int32), intent(in) :: row_dof, col_dof, row, col
            integer(int32) :: index

        end function find_coo

        module subroutine set_coo(self, row_dof, col_dof, row, col, value)
            implicit none
            class(type_coo), intent(inout) :: self
            integer(int32), intent(in) :: row_dof, col_dof, row, col
            real(real64), intent(in) :: value

        end subroutine set_coo

        module subroutine set_row_coo(self, row_dof, row, value)
            implicit none
            class(type_coo), intent(inout) :: self
            integer(int32), intent(in) :: row_dof, row
            real(real64), intent(in) :: value

        end subroutine set_row_coo

        module subroutine set_all_coo(self, value)
            implicit none
            class(type_coo), intent(inout) :: self
            real(real64), intent(in) :: value

        end subroutine set_all_coo

        module subroutine zero_coo(self)
            implicit none
            class(type_coo), intent(inout) :: self

        end subroutine zero_coo

        module subroutine add_coo(self, row_dof, col_dof, row, col, value)
            implicit none
            class(type_coo), intent(inout) :: self
            integer(int32), intent(in) :: row_dof, col_dof, row, col
            real(real64), intent(in) :: value

        end subroutine add_coo

        module subroutine add_matrix_coo(self, alpha, B, C)
            implicit none
            class(type_coo), intent(in) :: self ! This is matrix A
            real(real64), intent(in) :: alpha
            class(abst_matrix), intent(in) :: B
            class(abst_matrix), intent(inout) :: C

        end subroutine add_matrix_coo

        module subroutine gemv_coo(self, alpha, x, beta, y)
            implicit none
            class(type_coo), intent(in) :: self
            real(real64), intent(in) :: alpha
            real(real64), intent(in) :: x(:)
            real(real64), intent(in) :: beta
            real(real64), intent(inout) :: y(:)

        end subroutine gemv_coo

        module subroutine display_coo(self)
            implicit none
            class(type_coo), intent(in) :: self

        end subroutine display_coo

        module subroutine destroy_coo(self)
            implicit none
            class(type_coo), intent(inout) :: self

        end subroutine destroy_coo

    end interface

end module core_types_matrix
