module linalg_vector_ops
    use, intrinsic :: iso_fortran_env, only: int32, real64
    implicit none

#ifdef _MKL
    interface
        function dasum(n, x, incx)
            use, intrinsic :: iso_fortran_env, only: int32, real64
            integer(int32), intent(in) :: n, incx
            real(real64), intent(in) :: x(*)
            real(real64) :: dasum
        end function dasum

        function dnrm2(n, x, incx)
            use, intrinsic :: iso_fortran_env, only: int32, real64
            integer(int32), intent(in) :: n, incx
            real(real64), intent(in) :: x(*)
            real(real64) :: dnrm2
        end function dnrm2

        function idamax(n, x, incx)
            use, intrinsic :: iso_fortran_env, only: int32, real64
            integer(int32), intent(in) :: n, incx
            real(real64), intent(in) :: x(*)
            integer(int32) :: idamax
        end function idamax

        function ddot(n, x, incx, y, incy)
            use, intrinsic :: iso_fortran_env, only: int32, real64
            integer(int32), intent(in) :: n, incx, incy
            real(real64), intent(in) :: x(*), y(*)
            real(real64) :: ddot
        end function ddot
    end interface
#endif

    private
    public :: norm_1
    public :: norm_2
    public :: norm_inf
    public :: dot

contains

    function norm_1(x) result(norm)
        implicit none
        real(real64), intent(in) :: x(:)
        real(real64) :: norm
#ifdef _MKL
        norm = dasum(int(size(x), int32), x, 1)
#else
        norm = sum(abs(x))
#endif
    end function norm_1

    function norm_2(x) result(norm)
        implicit none
        real(real64), intent(in) :: x(:)
        real(real64) :: norm
#ifdef _MKL
        norm = dnrm2(int(size(x), int32), x, 1)
#else
        norm = norm2(x)
#endif
    end function norm_2

    function norm_inf(x) result(norm)
        implicit none
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

    function dot(x, y) result(prod)
        implicit none
        real(real64), intent(in) :: x(:)
        real(real64), intent(in) :: y(:)
        real(real64) :: prod

        if (size(x) /= size(y)) then
            write (*, '(A)') "Error: dot - array sizes do not match."
            error stop 1
        end if

#ifdef _MKL
        prod = ddot(int(size(x), int32), x, 1, y, 1)
#else
        prod = dot_product(x, y)
#endif
    end function dot

end module linalg_vector_ops
