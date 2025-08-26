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
        integer(int32), intent(in) :: row
        integer(int32), intent(in) :: col
        integer(int32) :: index

        integer(int32) :: i

        if (self%nnz == 0) then
            index = -1
            return
        end if

        ! --- 二分探索で行と列の組み合わせを探す ---
        index = -1
        do i = 1, self%nnz
            if (self%row(i) == row .and. self%col(i) == col) then
                index = i
                return
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

    subroutine destroy_coo(self)
        implicit none
        class(type_coo), intent(inout) :: self

        ! --- COO構造の解放 ---
        call deallocate_array(self%row)
        call deallocate_array(self%col)
        call deallocate_array(self%val)

        self%nnz = 0
    end subroutine destroy_coo

    ! !-------------------------------------------------------------------------------------------------------------------------------
    ! ! Matrix calculation
    ! !-------------------------------------------------------------------------------------------------------------------------------
    ! subroutine type_coo_gemv(alpha, A, x, beta, y)
    !     ! y := alpha*A*x + beta*y
    !     use omp_lib
    !     implicit none
    !     real(real64), intent(in) :: alpha
    !     type(type_coo), intent(in) :: A
    !     real(real64), intent(in) :: x(:)
    !     real(real64), intent(in) :: beta
    !     real(real64), intent(inout) :: y(:)

    !     integer(int32) :: i

    !     !$omp parallel do default(shared) private(i)
    !     do i = 1, A%nnz
    !         !$omp atomic
    !         y(A%row(i)) = alpha * A%val(i) * x(A%col(i)) + beta * y(A%row(i))
    !     end do
    !     !$omp end parallel do

    ! end subroutine type_coo_gemv

    ! subroutine type_coo_add(alpha, A, B, C)
    !     ! C := alpha*A + B
    !     !
    !     ! [ATTENTION] Assumes A, B, and C have the exact same sparsity pattern.
    !     !
    !     implicit none
    !     real(real64), intent(in) :: alpha
    !     type(type_coo), intent(in) :: A
    !     type(type_coo), intent(in) :: B
    !     type(type_coo), intent(inout) :: C

    !     integer(int32) :: i

    !     !$omp parallel do
    !     do i = 1, A%nnz
    !         C%val(i) = alpha * A%val(i) + B%val(i)
    !     end do
    !     !$omp end parallel do
    ! end subroutine type_coo_add

end module core_types_matrix_coo
