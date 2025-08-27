module core_types_matrix_coo
    use, intrinsic :: iso_fortran_env
    use :: core_allocate, only:allocate_array
    use :: core_deallocate, only:deallocate_array
    use :: core_types_matrix, only:abst_matrix
    implicit none
    private

    public :: type_coo
    ! public :: type_coo_gemv
    ! public :: type_coo_add

    type, extends(abst_matrix) :: type_coo
        integer(int32) :: nnz = 0 ! number of non-zero elements
        integer(int32), allocatable :: row(:)
        integer(int32), allocatable :: col(:)
        real(real64),   allocatable :: val(:) !& non-zero values
    contains
        procedure, public, pass(self) :: initialize => initialize_type_coo !&
        procedure, public, pass(self) :: find       => find_coo !&
        procedure, public, pass(self) :: set        => set_coo !&
        procedure, public, pass(self) :: set_all    => set_all_coo !&
        procedure, public, pass(self) :: zero       => zero_coo !&
        procedure, public, pass(self) :: add        => add_coo !&
        procedure, public, pass(self) :: display    => display_coo !&
        procedure, public, pass(self) :: destroy    => destroy_coo !&
    end type

contains

    subroutine initialize_type_coo(self, num_nodes, row, col)
        implicit none
        class(type_coo), intent(inout) :: self
        integer(int32), intent(in) :: num_nodes
        integer(int32), intent(in), optional :: row(:)
        integer(int32), intent(in), optional :: col(:)

        integer(int32) :: i

        if (.not. present(row) .or. .not. present(col)) then
            print *, "Error: row and col must be provided for COO initialization."
            stop
        end if

        self%nnz = size(row)

        call allocate_array(self%row, self%nnz)
        do i = 1, self%nnz
            self%row(i) = row(i)
        end do

        call allocate_array(self%col, self%nnz)
        call allocate_array(self%val, self%nnz)
        do i = 1, self%nnz
            self%col(i) = col(i)
            self%val(i) = 0.0d0
        end do

    end subroutine initialize_type_coo

    pure function find_coo(self, row, col) result(index)
        implicit none
        class(type_coo), intent(in) :: self
        integer(int32), intent(in) :: row, col
        integer(int32) :: index
        integer(int32) :: lo, hi, mid
        integer(int32) :: r, c

        ! 初期値
        index = 0
        if (self%nnz == 0) return

        lo = 1
        hi = self%nnz

        ! 二分探索
        do while (lo <= hi)
            mid = (lo + hi) / 2
            r = self%row(mid)
            c = self%col(mid)

            if (r == row .and. c == col) then
                index = mid
                return
            else if (r < row .or. (r == row .and. c < col)) then
                lo = mid + 1
            else
                hi = mid - 1
            end if
        end do
    end function find_coo

    subroutine set_coo(self, row, col, value)
        implicit none
        class(type_coo), intent(inout) :: self
        integer(int32), intent(in) :: row, col
        real(real64), intent(in) :: value

        integer(int32) :: index

        index = self%find(row, col)
        self%val(index) = value

    end subroutine set_coo

    subroutine set_all_coo(self, value)
        implicit none
        class(type_coo), intent(inout) :: self
        real(real64), intent(in) :: value

        integer(int32) :: i

        do i = 1, self%nnz
            self%val(i) = value
        end do

    end subroutine set_all_coo

    subroutine zero_coo(self)
        implicit none
        class(type_coo), intent(inout) :: self

        call self%set_all(0.0d0)

    end subroutine zero_coo

    subroutine add_coo(self, row, col, value)
        implicit none
        class(type_coo), intent(inout) :: self
        integer(int32), intent(in) :: row, col
        real(real64), intent(in) :: value

        integer(int32) :: index

        index = self%find(row, col)
        self%val(index) = self%val(index) + value

    end subroutine add_coo

    subroutine display_coo(self)
        implicit none
        class(type_coo), intent(in) :: self
        integer(int32) :: i

        do i = 1, self%nnz
            write (*, '(i0, 2x, i0, 2X, es16.8)') self%row(i), self%col(i), self%val(i)
        end do

    end subroutine display_coo

    subroutine destroy_coo(self)
        implicit none
        class(type_coo), intent(inout) :: self

        ! --- COO構造の解放 ---
        call deallocate_array(self%row)
        call deallocate_array(self%col)
        call deallocate_array(self%val)

        self%nnz = 0
    end subroutine destroy_coo

end module core_types_matrix_coo
