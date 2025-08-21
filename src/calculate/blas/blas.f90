module calculate_blas
!$  use :: omp_lib
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
#ifdef _OPENMP
#endif
    implicit none
    private
#ifdef _MKL
    include "mkl_blas.fi"

    integer :: converter
#endif

    public :: norm_1
    public :: norm_2
    public :: norm_infinity
    public :: inner_product
    public :: multiply_matrix_vector

contains

    function norm_1(x) result(norm)
        real(real64), dimension(:), intent(in) :: x
        real(real64) :: norm
#ifdef _MKL
        norm = dasum(size(x), x, 1)
#else
        norm = sum(abs(x)) ! 組込み関数の方がシンプルで高速な場合が多い
#endif
    end function norm_1

    ! L2 ノルム
    function norm_2(x) result(norm)
        real(real64), dimension(:), intent(in) :: x
        real(real64) :: norm
#ifdef _MKL
        norm = dnrm2(size(x), x, 1)
#else
        norm = norm2(x) ! 組込み関数を使用
#endif
    end function norm_2

    function norm_infinity(x) result(norm)
        real(real64), dimension(:), intent(in) :: x
        real(real64) :: norm
#ifdef _MKL
        ! idamaxは最大値の「インデックス」を返すため、そのインデックスの値を参照する
        if (size(x) > 0) then
            norm = abs(x(idamax(size(x), x, 1)))
        else
            norm = 0.0_real64
        end if
#else
        norm = maxval(abs(x)) ! 組込み関数を使用
#endif
    end function norm_infinity

    function inner_product(N, x, y) result(d_dot)
        implicit none
        integer(int32), intent(in) :: N
        real(real64), intent(in) :: x(:)
        real(real64), intent(in) :: y(:)
        real(real64) :: d_dot
        integer(int32) :: iN

        d_dot = 0.0d0

#ifdef _MKL
        d_dot = ddot(int(N, kind=kind(converter)), x, 1, y, 1)
#else
        !$omp parallel do private(iN) reduction(+:d_dot)
        do iN = 1, N
            d_dot = d_dot + x(iN) * y(iN)
        end do
        !$omp end parallel do
#endif
    end function inner_product

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

end module calculate_blas
