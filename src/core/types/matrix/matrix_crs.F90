module core_types_matrix_crs
    use, intrinsic :: iso_fortran_env
    use :: core_allocate, only:allocate_array
    use :: core_deallocate, only:deallocate_array
    use :: core_types_matrix, only:abst_matrix
    implicit none
    private

    public :: type_crs

    type, extends(abst_matrix) :: type_crs
        integer(int32) :: nnz ! number of non-zero elements
        integer(int32) :: num_row ! number of rows
        integer(int32) :: num_ptr ! size of ptr (num_row+1 entries)
        integer(int32), allocatable :: ptr(:) !& pointers to row starts (1-based)
        integer(int32), allocatable :: ind(:) !& column indices of non-zeros
        real(real64),   allocatable :: val(:) !& non-zero values
    contains
        procedure, public, pass(self) :: initialize => initialize_type_crs !&
        procedure, public, pass(self) :: find       => find_crs !&
        procedure, public, pass(self) :: set        => set_crs !&
        procedure, public, pass(self) :: set_all    => set_all_crs !&
        procedure, public, pass(self) :: zero       => zero_crs !&
        procedure, public, pass(self) :: add        => add_crs !&
        procedure, public, pass(self) :: display    => display_crs !&
        procedure, public, pass(self) :: destroy    => destroy_crs !&
    end type type_crs

contains

    subroutine initialize_type_crs(self, num_nodes, row, col)
        implicit none
        class(type_crs), intent(inout) :: self
        integer(int32), intent(in) :: num_nodes
        integer(int32), intent(in), optional :: row(:)
        integer(int32), intent(in), optional :: col(:)

        integer(int32) :: i

        if (.not. present(row) .or. .not. present(col)) then
            print *, "Error: row and col must be provided for CRS initialization."
            stop
        end if

        self%nnz = size(col)
        self%num_row = num_nodes
        self%num_ptr = self%num_row + 1

        call allocate_array(self%ptr, self%num_ptr)
        do i = 1, self%num_ptr
            self%ptr(i) = row(i)
        end do

        call allocate_array(self%ind, self%nnz)
        call allocate_array(self%val, self%nnz)
        do i = 1, self%nnz
            self%ind(i) = col(i)
            self%val(i) = 0.0d0
        end do

    end subroutine initialize_type_crs

    pure function find_crs(self, row, col) result(index)
        implicit none
        class(type_crs), intent(in) :: self
        integer(int32), intent(in) :: row, col
        integer(int32) :: index
        integer(int32) :: lo, hi, mid, i
        integer(int32), parameter :: LINEAR_SEARCH_THRESHOLD = 32

        index = 0
        lo = self%ptr(row)
        hi = self%ptr(row + 1) - 1

        if (lo > hi) return

        do while (lo <= hi)
            mid = lo + (hi - lo) / 2
            if (self%ind(mid) == col) then
                index = mid
                return
            else if (self%ind(mid) < col) then
                lo = mid + 1
            else
                hi = mid - 1
            end if
        end do
    end function find_crs

    subroutine set_crs(self, row, col, value)
        implicit none
        class(type_crs), intent(inout) :: self
        integer(int32), intent(in) :: row, col
        real(real64), intent(in) :: value

        integer(int32) :: index

        index = self%find(row, col)
        self%val(index) = value

    end subroutine set_crs

    subroutine set_all_crs(self, value)
        implicit none
        class(type_crs), intent(inout) :: self
        real(real64), intent(in) :: value

        integer(int32) :: i

        do i = 1, self%nnz
            self%val(i) = value
        end do

    end subroutine set_all_crs

    subroutine zero_crs(self)
        implicit none
        class(type_crs), intent(inout) :: self

        call self%set_all(0.0d0)

    end subroutine zero_crs

    subroutine add_crs(self, row, col, value)
        implicit none
        class(type_crs), intent(inout) :: self
        integer(int32), intent(in) :: row, col
        real(real64), intent(in) :: value

        integer(int32) :: index

        index = self%find(row, col)
        self%val(index) = self%val(index) + value

    end subroutine add_crs

    subroutine display_crs(self)
        implicit none
        class(type_crs), intent(in) :: self
        integer(int32) :: i, row, col
        integer(int32) :: row_start, row_end

        do row = 1, self%num_row
            row_start = self%ptr(row)
            row_end = self%ptr(row + 1) - 1
            do i = row_start, row_end
                col = self%ind(i)
                write (*, '(i0, 2x, i0, 2X, es16.8)') row, col, self%val(i)
            end do
        end do
    end subroutine display_crs

    subroutine destroy_crs(self)
        implicit none
        class(type_crs), intent(inout) :: self

        call deallocate_array(self%ptr)
        call deallocate_array(self%ind)
        call deallocate_array(self%val)

        self%nnz = 0
        self%num_row = 0
        self%num_ptr = 0
    end subroutine destroy_crs

end module core_types_matrix_crs
