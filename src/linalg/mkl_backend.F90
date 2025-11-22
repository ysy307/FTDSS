!>
!> Contains backend implementations for vector norm and dot product calculations.
!> It provides two versions of each routine: one using Intel MKL (if `_MKL`
!> is defined) and a native Fortran version. Both versions support MPI parallelism.
!> This module is intended to be used by `linalg_vector_operations`.
!>
module linalg_mkl_backend
    use, intrinsic :: iso_fortran_env, only: int32, real64
#ifdef _MPI
    use :: mpi_f08
#endif
#ifdef _MKL
    use :: linalg_mkl_interface
#endif

    implicit none
    private

#ifdef _MKL
    public :: norm_1_mkl
    public :: norm_2_mkl
    public :: norm_inf_mkl
    public :: dot_mkl
#else
    public :: norm_1_native
    public :: norm_2_native
    public :: norm_inf_native
    public :: dot_native
#endif

contains

    ! =========================================================================
    ! 1. MKL Backend Implementations
    ! =========================================================================
#ifdef _MKL
    !>
    !> Computes the 1-norm of a vector using MKL's ASUM function.
    !>
    function norm_1_mkl(x) result(norm_value)
        implicit none
        !> The input vector.
        real(real64), intent(in) :: x(:)
        !> The computed 1-norm, \( \sum |x_i| \).
        real(real64) :: norm_value

#ifdef _MPI
        integer(int32) :: nprocs
        call MPI_Comm_size(MPI_COMM_WORLD, nprocs)

        if (nprocs == 1) then
            norm_value = dasum(int(size(x), int32), x, 1)
        else
            norm_value = pdasum(int(size(x), int32), x, 1)
        end if
#else
        norm_value = dasum(int(size(x), int32), x, 1)
#endif
    end function norm_1_mkl

    !>
    !> Computes the 2-norm (Euclidean norm) of a vector using MKL's NRM2 function.
    !>
    function norm_2_mkl(x) result(norm_value)
        implicit none
        !> The input vector.
        real(real64), intent(in) :: x(:)
        !> The computed 2-norm, \( \sqrt{\sum x_i^2} \).
        real(real64) :: norm_value

#ifdef _MPI
        integer(int32) :: nprocs
        call MPI_Comm_size(MPI_COMM_WORLD, nprocs)
        if (nprocs == 1) then
            norm_value = dnrm2(int(size(x), int32), x, 1)
        else
            norm_value = pdnrm2(int(size(x), int32), x, 1)
        end if
#else
        norm_value = dnrm2(int(size(x), int32), x, 1)
#endif
    end function norm_2_mkl

    !>
    !> Computes the infinity-norm (maximum absolute value) of a vector using MKL's IDAMAX.
    !>
    function norm_inf_mkl(x) result(norm_value)
        implicit none
        !> The input vector.
        real(real64), intent(in) :: x(:)
        !> The computed infinity-norm, \( \max(|x_i|) \).
        real(real64) :: norm_value

        real(real64) :: local_max
        real(real64) :: global_max
        integer(int32) :: ierr

        if (size(x) > 0) then
            local_max = abs(x(idamax(int(size(x), int32), x, 1)))
        else
            local_max = 0.0d0
        end if
#ifdef _MPI
        call MPI_Allreduce(local_max, global_max, 1, MPI_REAL8, MPI_MAX, MPI_COMM_WORLD, ierr)
        norm_value = global_max
#else
        norm_value = local_max
#endif
    end function norm_inf_mkl

    !>
    !> Computes the dot product of two vectors using MKL's DOT function.
    !>
    function dot_mkl(x, y) result(product)
        implicit none
        !> The first input vector.
        real(real64), intent(in) :: x(:)
        !> The second input vector.
        real(real64), intent(in) :: y(:)
        !> The computed dot product, \( \sum x_i y_i \).
        real(real64) :: product

#ifdef _MPI
        integer(int32) :: nprocs
        call MPI_Comm_size(MPI_COMM_WORLD, nprocs)
        if (nprocs == 1) then
            product = ddot(int(size(x), int32), x, 1, y, 1)
        else
            product = pddot(int(size(x), int32), x, 1, y, 1)
        end if
#else
        product = ddot(int(size(x), int32), x, 1, y, 1)
#endif
    end function dot_mkl
#endif

    ! =========================================================================
    ! 2. Native Fortran Backend Implementations
    ! =========================================================================
    !>
    !> Computes the 1-norm of a vector using native Fortran intrinsics.
    !>
    function norm_1_native(x) result(norm_value)
        implicit none
        !> The input vector.
        real(real64), intent(in) :: x(:)
        !> The computed 1-norm, \( \sum |x_i| \).
        real(real64) :: norm_value

        real(real64) :: local_sum
        real(real64) :: global_sum
        integer(int32) :: ierr

        local_sum = sum(abs(x))
#ifdef _MPI
        call MPI_Allreduce(local_sum, global_sum, 1, MPI_REAL8, MPI_SUM, MPI_COMM_WORLD, ierr)
        norm_value = global_sum
#else
        norm_value = local_sum
#endif
    end function norm_1_native

    !>
    !> Computes the 2-norm (Euclidean norm) of a vector using native Fortran intrinsics.
    !>
    function norm_2_native(x) result(norm_value)
        implicit none
        !> The input vector.
        real(real64), intent(in) :: x(:)
        !> The computed 2-norm, \( \sqrt{\sum x_i^2} \).
        real(real64) :: norm_value

        real(real64) :: local_sum_sq
        real(real64) :: global_sum_sq
        integer(int32) :: ierr

        local_sum_sq = sum(x**2)
#ifdef _MPI
        call MPI_Allreduce(local_sum_sq, global_sum_sq, 1, MPI_REAL8, MPI_SUM, MPI_COMM_WORLD, ierr)
        norm_value = sqrt(global_sum_sq)
#else
        norm_value = sqrt(local_sum_sq)
#endif
    end function norm_2_native

    !>
    !> Computes the infinity-norm (maximum absolute value) of a vector using native Fortran intrinsics.
    !>
    function norm_inf_native(x) result(norm_value)
        implicit none
        !> The input vector.
        real(real64), intent(in) :: x(:)
        !> The computed infinity-norm, \( \max(|x_i|) \).
        real(real64) :: norm_value

        real(real64) :: local_max
        real(real64) :: global_max
        integer(int32) :: ierr

        if (size(x) > 0) then
            local_max = maxval(abs(x))
        else
            local_max = 0.0d0
        end if
#ifdef _MPI
        call MPI_Allreduce(local_max, global_max, 1, MPI_REAL8, MPI_MAX, MPI_COMM_WORLD, ierr)
        norm_value = global_max
#else
        norm_value = local_max
#endif
    end function norm_inf_native

    !>
    !> Computes the dot product of two vectors using the native Fortran intrinsic.
    !>
    function dot_native(x, y) result(product)
        implicit none
        !> The first input vector.
        real(real64), intent(in) :: x(:)
        !> The second input vector.
        real(real64), intent(in) :: y(:)
        !> The computed dot product, \( \sum x_i y_i \).
        real(real64) :: product

        real(real64) :: local_prod
        real(real64) :: global_prod
        integer(int32) :: ierr

        local_prod = dot_product(x, y)
#ifdef _MPI
        call MPI_Allreduce(local_prod, global_prod, 1, MPI_REAL8, MPI_SUM, MPI_COMM_WORLD, ierr)
        product = global_prod
#else
        product = local_prod
#endif
    end function dot_native

end module linalg_mkl_backend
