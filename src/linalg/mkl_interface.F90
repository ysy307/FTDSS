!>
!> Provides external function interfaces for the Intel Math Kernel Library (MKL).
!> This entire module is only compiled if the `_MKL` preprocessor macro is defined.
!> It includes interfaces for both standard and MPI-parallel BLAS routines.
!>
#ifdef _MKL
module linalg_mkl_interface
    use, intrinsic :: iso_fortran_env, only: int32, real64
    implicit none
    private

#ifdef _MPI
    public :: pdasum
    public :: pdnrm2
    public :: pddot
#else
    public :: dasum
    public :: dnrm2
    public :: ddot
#endif
    public :: idamax

    interface
#ifdef _MPI
        !>
        !> Computes the sum of the absolute values of a distributed vector's
        !> elements (1-norm) in parallel.
        !>
        function pdasum(n, x, incx)
            import :: int32, real64
            implicit none
            !> The number of elements in the local portion of the vector.
            integer(int32), intent(in) :: n
            !> The local portion of the input vector.
            real(real64), intent(in) :: x(*)
            !> The storage spacing between elements of x.
            integer(int32), intent(in) :: incx
            !> The globally summed 1-norm of the vector, \( \sum |x_i| \).
            real(real64) :: pdasum
        end function

        !>
        !> Computes the Euclidean norm (2-norm) of a distributed vector in parallel.
        !>
        function pdnrm2(n, x, incx)
            import :: int32, real64
            implicit none
            !> The number of elements in the local portion of the vector.
            integer(int32), intent(in) :: n
            !> The local portion of the input vector.
            real(real64), intent(in) :: x(*)
            !> The storage spacing between elements of x.
            integer(int32), intent(in) :: incx
            !> The globally computed 2-norm of the vector, \( \sqrt{\sum x_i^2} \).
            real(real64) :: pdnrm2
        end function

        !>
        !> Computes the dot product of two distributed vectors in parallel.
        !>
        function pddot(n, x, incx, y, incy)
            import :: int32, real64
            implicit none
            !> The number of elements in the local portion of the vectors.
            integer(int32), intent(in) :: n
            !> The local portion of the first input vector, x.
            real(real64), intent(in) :: x(*)
            !> The storage spacing between elements of x.
            integer(int32), intent(in) :: incx
            !> The local portion of the second input vector, y.
            real(real64), intent(in) :: y(*)
            !> The storage spacing between elements of y.
            integer(int32), intent(in) :: incy
            !> The globally summed dot product, \( \sum x_i y_i \).
            real(real64) :: pddot
        end function
#else
        !>
        !> Computes the sum of the absolute values of a vector's elements (1-norm).
        !>
        function dasum(n, x, incx)
            import :: int32, real64
            implicit none
            !> The number of elements in the vector.
            integer(int32), intent(in) :: n
            !> The input vector.
            real(real64), intent(in) :: x(*)
            !> The storage spacing between elements of x.
            integer(int32), intent(in) :: incx
            !> The computed 1-norm of the vector, \( \sum |x_i| \).
            real(real64) :: dasum
        end function

        !>
        !> Computes the Euclidean norm (2-norm) of a vector.
        !>
        function dnrm2(n, x, incx)
            import :: int32, real64
            implicit none
            !> The number of elements in the vector.
            integer(int32), intent(in) :: n
            !> The input vector.
            real(real64), intent(in) :: x(*)
            !> The storage spacing between elements of x.
            integer(int32), intent(in) :: incx
            !> The computed 2-norm of the vector, \( \sqrt{\sum x_i^2} \).
            real(real64) :: dnrm2
        end function

        !>
        !> Computes the dot product of two vectors.
        !>
        function ddot(n, x, incx, y, incy)
            import :: int32, real64
            implicit none
            !> The number of elements in the vectors.
            integer(int32), intent(in) :: n
            !> The first input vector, x.
            real(real64), intent(in) :: x(*)
            !> The storage spacing between elements of x.
            integer(int32), intent(in) :: incx
            !> The second input vector, y.
            real(real64), intent(in) :: y(*)
            !> The storage spacing between elements of y.
            integer(int32), intent(in) :: incy
            !> The computed dot product, \( \sum x_i y_i \).
            real(real64) :: ddot
        end function
#endif
        !>
        !> Finds the index of the element with the maximum absolute value in a vector.
        !>
        function idamax(n, x, incx)
            import :: int32, real64
            implicit none
            !> The number of elements in the vector.
            integer(int32), intent(in) :: n
            !> The input vector.
            real(real64), intent(in) :: x(*)
            !> The storage spacing between elements of x.
            integer(int32), intent(in) :: incx
            !> The 1-based index of the element with the maximum absolute value.
            integer(int32) :: idamax
        end function
    end interface

end module linalg_mkl_interface
#endif