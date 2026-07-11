!>
!> Provides external function interfaces for the Intel Math Kernel Library (MKL).
!> This entire module is only compiled if the `_MKL` preprocessor macro is defined.
!> It includes interfaces for both standard and MPI-parallel BLAS routines.
!>
#ifdef _MKL
module linalg_mkl_interface
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use, intrinsic :: iso_c_binding, only: c_int, c_double, c_intptr_t
    implicit none
    private

    public :: mkl_pardiso_handle
    public :: dasum
    public :: dnrm2
    public :: ddot
    public :: daxpy

    public :: idamax

    public :: pardiso
    public :: pardisoinit

    !> C-interoperable opaque handle matching MKL_PARDISO_HANDLE (void* per element).
    type, BIND(C) :: mkl_pardiso_handle
        integer(c_intptr_t) :: dummy = 0
    end type mkl_pardiso_handle

    interface

        !> Initializes the PARDISO internal data structures.
        subroutine pardisoinit(pt, mtype, iparm) bind(C, name="pardisoinit")
            import :: c_int, mkl_pardiso_handle
            implicit none
            !> PARDISO internal data pointer.
            type(mkl_pardiso_handle), intent(inout) :: pt(64)
            !> Matrix type identifier.
            integer(c_int), intent(in) :: mtype
            !> PARDISO parameter array.
            integer(c_int), intent(inout) :: iparm(64)
        end subroutine pardisoinit

        !> PARDISO sparse direct solver interface.
        subroutine pardiso(pt, maxfct, mnum, mtype, phase, n, a, ia, ja, perm, nrhs, iparm, msglvl, b, x, error) &
            bind(C, name="pardiso")
            import :: c_int, c_double, mkl_pardiso_handle
            implicit none
            !> PARDISO internal data pointer.
            type(mkl_pardiso_handle), intent(inout) :: pt(64)
            !> Maximum number of numerical factorizations.
            integer(c_int), intent(in) :: maxfct
            !> Which factorization to use.
            integer(c_int), intent(in) :: mnum
            !> Matrix type identifier.
            integer(c_int), intent(in) :: mtype
            !> PARDISO phase control.
            integer(c_int), intent(in) :: phase
            !> Matrix order.
            integer(c_int), intent(in) :: n
            !> Nonzero matrix values (CSR).
            real(c_double), intent(inout) :: a(*)
            !> CSR row pointer.
            integer(c_int), intent(inout) :: ia(*)
            !> CSR column indices.
            integer(c_int), intent(inout) :: ja(*)
            !> Permutation array (or 0 for default).
            integer(c_int), intent(inout) :: perm(*)
            !> Number of right-hand sides.
            integer(c_int), intent(in) :: nrhs
            !> PARDISO parameter array.
            integer(c_int), intent(inout) :: iparm(*)
            !> Message level.
            integer(c_int), intent(in) :: msglvl
            !> Right-hand side vector.
            real(c_double), intent(inout) :: b(*)
            !> Solution vector.
            real(c_double), intent(inout) :: x(*)
            !> Error flag.
            integer(c_int), intent(inout) :: error
        end subroutine pardiso

        !>
        !> Computes the 1-norm (sum of absolute values) of a vector.
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
        !> Computes \( y \leftarrow a x + y \) (BLAS-1 AXPY).
        !>
        subroutine daxpy(n, a, x, incx, y, incy)
            implicit none
            !> The number of elements in the vectors.
            integer, intent(in) :: n
            !> The scalar multiplier a.
            double precision, intent(in) :: a
            !> The input vector x.
            double precision, intent(in) :: x(*)
            !> The storage spacing between elements of x.
            integer, intent(in) :: incx
            !> The input/output vector y, overwritten with a*x + y.
            double precision, intent(inout) :: y(*)
            !> The storage spacing between elements of y.
            integer, intent(in) :: incy
        end subroutine

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
