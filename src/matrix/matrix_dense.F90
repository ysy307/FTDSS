module matrix_dense
!$  use :: omp_lib
    use, intrinsic :: iso_fortran_env
    use :: module_core, only:allocate_array, deallocate_array
    use :: module_calculate, only:multiply_matrix_vector
    use :: module_domain, only:type_domain
    use :: matrix_base, only:abst_matrix
    implicit none
    private

    public :: type_dense

    public :: type_dense_gemv
    public :: type_dense_add

    type, extends(abst_matrix) :: type_dense
        integer(int32) :: num_row
        integer(int32) :: num_col
        real(real64), allocatable :: val(:, :)
    contains
        procedure, public, pass(self) :: initialize       => initialize_type_dense_from_domain !&
        procedure, public, pass(self) :: initialize_local => initialize_type_dense_from_node !&
        procedure, public, pass(self) :: find             => find_dense !&
        procedure, public, pass(self) :: set              => set_dense !&
        procedure, public, pass(self) :: set_all          => set_all_dense !&
        procedure, public, pass(self) :: add              => add_dense !&
        procedure, public, pass(self) :: destroy          => destroy_dense !&
    end type
contains
    subroutine initialize_type_dense_from_domain(self, domain)
        implicit none
        class(type_dense), intent(inout) :: self
        type(type_domain), intent(inout) :: domain
        integer(int32) :: num_nodes

        num_nodes = domain%get_num_nodes()
        call allocate_array(self%val, num_nodes, num_nodes)
        self%val(:, :) = 0.0d0

        self%num_row = num_nodes
        self%num_col = num_nodes

    end subroutine initialize_type_dense_from_domain

    subroutine initialize_type_dense_from_node(self, num_nodes)
        implicit none
        class(type_dense), intent(inout) :: self
        integer(int32), intent(in) :: num_nodes

        call allocate_array(self%val, num_nodes, num_nodes)
        self%num_row = num_nodes
        self%num_col = num_nodes

        self%val(:, :) = 0.0d0

    end subroutine initialize_type_dense_from_node

    pure function find_dense(self, row, col) result(index)
        implicit none
        class(type_dense), intent(in) :: self
        integer(int32), intent(in) :: row
        integer(int32), intent(in) :: col
        integer(int32) :: index

        if (row < 1 .or. row > self%num_row .or. col < 1 .or. col > self%num_col) then
            index = 0
        else
            index = row + (col - 1) * self%num_row
        end if

        return
    end function find_dense

    subroutine set_dense(self, row, col, value)
        implicit none
        class(type_dense), intent(inout) :: self
        integer(int32), intent(in) :: row, col
        real(real64), intent(in) :: value

        self%val(row, col) = value

    end subroutine set_dense

    subroutine set_all_dense(self, value)
        implicit none
        class(type_dense), intent(inout) :: self
        real(real64), intent(in) :: value

        integer(int32) :: i, j

        do j = 1, self%num_col
            do i = 1, self%num_row
                self%val(i, j) = value
            end do
        end do
    end subroutine set_all_dense

    subroutine add_dense(self, row, col, value)
        implicit none
        class(type_dense), intent(inout) :: self
        integer(int32), intent(in) :: row, col
        real(real64), intent(in) :: value

        self%val(row, col) = self%val(row, col) + value

    end subroutine add_dense

    subroutine destroy_dense(self)
        implicit none
        class(type_dense), intent(inout) :: self

        call deallocate_array(self%val)
    end subroutine destroy_dense

    !-------------------------------------------------------------------------------------------------------------------------------
    ! Matrix calculation
    !-------------------------------------------------------------------------------------------------------------------------------
    subroutine type_dense_gemv(alpha, A, x, beta, y)
        ! y := alpha*A*x + beta*y
        implicit none
        real(real64), intent(in) :: alpha
        type(type_dense), intent(in) :: A
        real(real64), intent(in) :: beta
        real(real64), intent(in) :: x(:)
        real(real64), intent(inout) :: y(:)

        call multiply_matrix_vector(alpha, A%val, x, beta, y)

    end subroutine type_dense_gemv

    subroutine type_dense_add(alpha, A, B, C)
        ! C := alpha*A + B
        !
        ! [ATTENTION] Assumes A, B, and C have the exact same sparsity pattern.
        !
        implicit none
        real(real64), intent(in) :: alpha
        type(type_dense), intent(in) :: A
        type(type_dense), intent(in) :: B
        type(type_dense), intent(inout) :: C

        integer(int32) :: i, j

        !$omp parallel do private(i, j) collapse(2)
        do i = 1, size(A%val, 1)
            do j = 1, size(A%val, 2)
                C%val(i, j) = alpha * A%val(i, j) + B%val(i, j)
            end do
        end do
        !$omp end parallel do

    end subroutine type_dense_add
end module matrix_dense
