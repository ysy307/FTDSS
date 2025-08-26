module calculate_linalg_vector_ops
!$  use omp_lib
    use, intrinsic :: iso_fortran_env, only: int32, real64
    implicit none
    private
    ! int32をサイズ指定に使用します。
#ifdef _MKL
    include "mkl_blas.fi"
#endif

    public :: norm_1
    public :: norm_2
    public :: norm_inf
    public :: dot

contains

    ! L1 ノルム
    function norm_1(x) result(norm)
        real(real64), intent(in) :: x(:)
        real(real64) :: norm
#ifdef _MKL
        norm = dasum(int(size(x), int32), x, 1)
#else
        norm = sum(abs(x))
#endif
    end function norm_1

    ! L2 ノルム
    function norm_2(x) result(norm)
        real(real64), intent(in) :: x(:)
        real(real64) :: norm
#ifdef _MKL
        norm = dnrm2(int(size(x), int32), x, 1)
#else
        norm = norm2(x)
#endif
    end function norm_2

    ! L∞ ノルム (無限大ノルム)
    function norm_inf(x) result(norm)
        real(real64), intent(in) :: x(:)
        real(real64) :: norm
#ifdef _MKL
        if (size(x) > 0) then
            norm = abs(x(idamax(int(size(x), int32), x, 1)))
        else
            norm = 0.0d0
        end if
#else
        norm = maxval(abs(x))
#endif
    end function norm_inf

    ! 内積
    function dot(x, y) result(prod)
        real(real64), intent(in) :: x(:)
        real(real64), intent(in) :: y(:)
        real(real64) :: prod

        ! 配列のサイズが異なる場合はエラーとして停止
        if (size(x) /= size(y)) then
            write (*, '(A)') "Error: dot - array sizes do not match."
            error stop 1
        end if

#ifdef _MKL
        prod = ddot(int(size(x), int32), x, 1, y, 1)
#else
        ! 手動ループやOpenMPよりも、dot_product組込み関数が推奨される
        prod = dot_product(x, y)
#endif
    end function dot

end module calculate_linalg_vector_ops
