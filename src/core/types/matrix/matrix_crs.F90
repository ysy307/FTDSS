module core_types_matrix_crs
    use, intrinsic :: iso_fortran_env
    use :: core_allocate, only:allocate_array
    use :: core_deallocate, only:deallocate_array
    use :: core_types_matrix, only:abst_matrix
    implicit none
    private

    public :: type_crs
    ! public :: type_crs_gemv
    ! public :: type_crs_add

    type, extends(abst_matrix) :: type_crs
        integer(int32) :: nnz ! number of non-zero elements
        integer(int32) :: num_row ! number of rows
        integer(int32) :: num_ptr ! size of ptr (num_row+1 entries)
        integer(int32), allocatable :: ptr(:) ! pointers to row starts (1-based)
        integer(int32), allocatable :: ind(:) ! column indices of non-zeros
        real(real64), allocatable :: val(:) ! non-zero values
    contains
        procedure, public, pass(self) :: initialize => initialize_type_crs !&
        procedure, public, pass(self) :: find       => find_crs !&
        procedure, public, pass(self) :: set        => set_crs !&
        procedure, public, pass(self) :: set_all    => set_all_crs !&
        procedure, public, pass(self) :: zero       => zero_crs !&
        procedure, public, pass(self) :: add        => add_crs !&
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
        self%num_row = size(row)
        self%num_ptr = self%num_row + 1

        call allocate_array(self%ptr, self%num_row)
        do i = 1, self%num_row
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
        integer(int32), intent(in) :: row
        integer(int32), intent(in) :: col
        integer(int32) :: index

        integer(int32) :: i
        integer(int32) :: search_start, search_end

        index = 0 ! 見つからなかった場合のデフォルト値

        ! 検索範囲を設定
        search_start = self%ptr(row)
        search_end = self%ptr(row + 1) - 1

        ! 範囲が存在しない場合は終了
        if (search_start > search_end) return

        ! 線形探索 (最初から最後まで順番に探す)
        do i = search_start, search_end
            if (self%ind(i) == col) then
                index = i
                return ! 見つかったら即座に終了
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

    ! !-------------------------------------------------------------------------------------------------------------------------------
    ! ! Matrix calculation
    ! !-------------------------------------------------------------------------------------------------------------------------------
    ! subroutine type_crs_gemv(alpha, A, x, beta, y)
    !     ! y := alpha*A*x + beta*y
    !     implicit none
    !     real(real64), intent(in) :: alpha
    !     type(type_crs), intent(in) :: A
    !     real(real64), intent(in) :: x(:)
    !     real(real64), intent(in) :: beta
    !     real(real64), intent(inout) :: y(:)

    !     integer(int32) :: i, j, is, ie
    !     real(real64) :: sum

    !     !$omp parallel do private(i, j, is, ie, sum)
    !     do i = 1, A%num_row
    !         sum = 0.0d0
    !         is = A%ptr(i)
    !         ie = A%ptr(i + 1) - 1
    !         do j = is, ie
    !             sum = sum + A%val(j) * x(A%ind(j))
    !         end do
    !         y(i) = alpha * sum + beta * y(i)
    !     end do
    !     !$omp end parallel do

    ! end subroutine type_crs_gemv

    ! subroutine type_crs_add(alpha, A, B, C)
    !     ! C := alpha*A + B
    !     !
    !     ! [ATTENTION] Assumes A, B, and C have the exact same sparsity pattern.
    !     !
    !     implicit none
    !     real(real64), intent(in) :: alpha
    !     type(type_crs), intent(in) :: A
    !     type(type_crs), intent(in) :: B
    !     type(type_crs), intent(inout) :: C

    !     integer(int32) :: i

    !     !$omp parallel do
    !     do i = 1, A%nnz
    !         C%val(i) = alpha * A%val(i) + B%val(i)
    !     end do
    !     !$omp end parallel do

    ! end subroutine type_crs_add

end module core_types_matrix_crs
