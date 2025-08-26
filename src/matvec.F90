module calculate_linalg_matvec
!$  use omp_lib
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core, only:type_coo, type_crs, type_dense
    implicit none
    private

    public :: gemv
    interface gemv
        module procedure :: type_coo_gemv
        module procedure :: type_crs_gemv
        module procedure :: type_dense_gemv
    end interface

contains

    subroutine multiply_matrix_vector(alpha, A, x, beta, y)
        implicit none
        real(real64), intent(in) :: alpha
        real(real64), intent(in) :: A(:, :)
        real(real64), intent(in) :: x(:)
        real(real64), intent(in) :: beta
        real(real64), intent(inout) :: y(:)

        integer(int32) :: i

#ifdef _MKL
        call dgemv('N', size(A, 1), size(A, 2), alpha, A, size(A, 1), x, 1, beta, y, 1)
#else

        !$omp parallel do private(i)
        do i = 1, size(A, 1)
            y(i) = alpha * dot_product(A(i, :), x) + beta * y(i)
        end do
        !$omp end parallel do
#endif

    end subroutine multiply_matrix_vector

    subroutine type_coo_gemv(alpha, A, x, beta, y)
        ! y := alpha*A*x + beta*y
        implicit none
        real(real64), intent(in) :: alpha
        type(type_coo), intent(in) :: A
        real(real64), intent(in) :: x(:)
        real(real64), intent(in) :: beta
        real(real64), intent(inout) :: y(:)

        integer(int32) :: i

        !$omp parallel do default(shared) private(i)
        do i = 1, A%nnz
            !$omp atomic
            y(A%row(i)) = alpha * A%val(i) * x(A%col(i)) + beta * y(A%row(i))
        end do
        !$omp end parallel do

    end subroutine type_coo_gemv

    subroutine type_crs_gemv(alpha, A, x, beta, y)
        ! y := alpha*A*x + beta*y
        implicit none
        real(real64), intent(in) :: alpha
        type(type_crs), intent(in) :: A
        real(real64), intent(in) :: x(:)
        real(real64), intent(in) :: beta
        real(real64), intent(inout) :: y(:)

        integer(int32) :: i, j, is, ie
        real(real64) :: sum

        !$omp parallel do private(i, j, is, ie, sum)
        do i = 1, A%num_row
            sum = 0.0d0
            is = A%ptr(i)
            ie = A%ptr(i + 1) - 1
            do j = is, ie
                sum = sum + A%val(j) * x(A%ind(j))
            end do
            y(i) = alpha * sum + beta * y(i)
        end do
        !$omp end parallel do

    end subroutine type_crs_gemv

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

end module calculate_linalg_matvec
