module Calculate_BLAS
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
#ifdef _OPENMP
    use :: omp_lib
#endif
    implicit none
    private
#ifdef _MKL
    include "mkl_blas.fi"

    integer :: converter
#endif

    public :: norm_2
    public :: dot

contains

    function norm_2(N, x) result(norm)
        implicit none
        integer(int32), intent(in) :: N
        real(real64), intent(in) :: x(:)
        real(real64) :: norm
        integer(int32) :: iN

        norm = 0.0d0

#ifdef _MKL
        norm = dnrm2(transfer(N, converter), x, 1)
#else
        !$omp parallel do private(iN) reduction(+:norm)
        do iN = 1, N
            norm = norm + x(iN)**2
        end do
        !$omp end parallel do
        norm = sqrt(norm)
#endif
    end function norm_2

    function dot(N, x, y) result(d_dot)
        implicit none
        integer(int32), intent(in) :: N
        real(real64), intent(in) :: x(:)
        real(real64), intent(in) :: y(:)
        real(real64) :: d_dot
        integer(int32) :: iN

        d_dot = 0.0d0

#ifdef _MKL
        d_dot = ddot(transfer(N, converter), x, 1, y, 1)
#else
        !$omp parallel do private(iN) reduction(+:d_dot)
        do iN = 1, N
            d_dot = d_dot + x(iN) * y(iN)
        end do
        !$omp end parallel do
#endif
    end function dot

end module Calculate_BLAS
