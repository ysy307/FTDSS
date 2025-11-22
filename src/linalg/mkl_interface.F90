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

    public :: pdasum
    public :: pdnrm2
    public :: pddot

    public :: dasum
    public :: dnrm2
    public :: ddot

    public :: idamax

    interface

        !>
        !> Computes the sum of the absolute values of a distributed vector's
        !> elements (1-norm) in parallel.
        !>
        function pdasum(n, x, incx)
            implicit none
            !> The number of elements in the local portion of the vector.
            integer, intent(in) :: n
            !> The local portion of the input vector.
            double precision, intent(in) :: x(*)
            !> The storage spacing between elements of x.
            integer, intent(in) :: incx
            !> The globally summed 1-norm of the vector, \( \sum |x_i| \).
            double precision :: pdasum
        end function

        !>
        !> Computes the Euclidean norm (2-norm) of a distributed vector in parallel.
        !>
        function pdnrm2(n, x, incx)
            implicit none
            !> The number of elements in the local portion of the vector.
            integer, intent(in) :: n
            !> The local portion of the input vector.
            double precision, intent(in) :: x(*)
            !> The storage spacing between elements of x.
            integer, intent(in) :: incx
            !> The globally computed 2-norm of the vector, \( \sqrt{\sum x_i^2} \).
            double precision :: pdnrm2
        end function

        !>
        !> Computes the dot product of two distributed vectors in parallel.
        !>
        function pddot(n, x, incx, y, incy)
            implicit none
            !> The number of elements in the local portion of the vectors.
            integer, intent(in) :: n
            !> The local portion of the first input vector, x.
            double precision, intent(in) :: x(*)
            !> The storage spacing between elements of x.
            integer, intent(in) :: incx
            !> The local portion of the second input vector, y.
            double precision, intent(in) :: y(*)
            !> The storage spacing between elements of y.
            integer, intent(in) :: incy
            !> The globally summed dot product, \( \sum x_i y_i \).
            double precision :: pddot
        end function

        !>
        !> Computes the sum of the absolute values of a vector's elements (1-norm).
        !>
        function dasum(n, x, incx)
            implicit none
            !> The number of elements in the vector.
            integer, intent(in) :: n
            !> The input vector.
            double precision, intent(in) :: x(*)
            !> The storage spacing between elements of x.
            integer, intent(in) :: incx
            !> The computed 1-norm of the vector, \( \sum |x_i| \).
            double precision :: dasum
        end function

        !>
        !> Computes the Euclidean norm (2-norm) of a vector.
        !>
        function dnrm2(n, x, incx)
            implicit none
            !> The number of elements in the vector.
            integer, intent(in) :: n
            !> The input vector.
            double precision, intent(in) :: x(*)
            !> The storage spacing between elements of x.
            integer, intent(in) :: incx
            !> The computed 2-norm of the vector, \( \sqrt{\sum x_i^2} \).
            double precision :: dnrm2
        end function

        !>
        !> Computes the dot product of two vectors.
        !>
        function ddot(n, x, incx, y, incy)
            implicit none
            !> The number of elements in the vectors.
            integer, intent(in) :: n
            !> The first input vector, x.
            double precision, intent(in) :: x(*)
            !> The storage spacing between elements of x.
            integer, intent(in) :: incx
            !> The second input vector, y.
            double precision, intent(in) :: y(*)
            !> The storage spacing between elements of y.
            integer, intent(in) :: incy
            !> The computed dot product, \( \sum x_i y_i \).
            double precision :: ddot
        end function

        !>
        !> Finds the index of the element with the maximum absolute value in a vector.
        !>
        function idamax(n, x, incx)
            implicit none
            !> The number of elements in the vector.
            integer, intent(in) :: n
            !> The input vector.
            double precision, intent(in) :: x(*)
            !> The storage spacing between elements of x.
            integer, intent(in) :: incx
            !> The 1-based index of the element with the maximum absolute value.
            integer :: idamax
        end function
    end interface

end module linalg_mkl_interface
#endif