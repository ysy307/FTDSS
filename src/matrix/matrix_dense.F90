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
        real(real64), allocatable :: val(:, :)
    contains
        procedure, public, pass(self) :: initialize => initialize_type_dense
        procedure, public, pass(self) :: find => find_dense
        ! procedure, public, pass(self) :: copy => copy_dense
        procedure, public, pass(self) :: destroy => destroy_dense
    end type
contains
    subroutine initialize_type_dense(self, domain)
        implicit none
        class(type_dense), intent(inout) :: self
        type(type_domain), intent(inout) :: domain
        integer(int32) :: num_nodes

        num_nodes = domain%get_num_nodes()
        call allocate_array(self%val, num_nodes, num_nodes)
        self%val(:, :) = 0.0d0

    end subroutine initialize_type_dense

    subroutine find_dense(self, row, col, index)
        implicit none
        class(type_dense), intent(in) :: self
        integer(int32), intent(in) :: row, col
        integer(int32), intent(inout) :: index

        return
    end subroutine find_dense

    ! function copy_dense(self) result(B)
    !     implicit none
    !     class(type_dense), intent(in) :: self
    !     class(abst_matrix), allocatable :: B

    !     select type (matrix => B)
    !     type is (type_dense)
    !         call allocate_array(matrix%val, size(self%val, 1), size(self%val, 2))
    !         matrix%val(:, :) = self%val(:, :)
    !     end select
    ! end function copy_dense

    subroutine destroy_dense(self)
        implicit none
        class(type_dense), intent(inout) :: self

        call deallocate_array(self%val)
    end subroutine destroy_dense

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
        implicit none
        real(real64), intent(in) :: alpha
        type(type_dense), intent(in) :: A
        type(type_dense), intent(in) :: B
        type(type_dense), intent(inout) :: C

        integer(int32) :: i, j
        real(real64), allocatable :: tmp(:, :)

        call deallocate_array(tmp)
        allocate (tmp, source=B%val)

        !$omp parallel do private(i, j) collapse(2)
        do i = 1, size(A%val, 1)
            do j = 1, size(A%val, 2)
                C%val(i, j) = alpha * A%val(i, j) + tmp(i, j)
            end do
        end do
        !$omp end parallel do

        call deallocate_array(tmp)

    end subroutine type_dense_add
end module matrix_dense
