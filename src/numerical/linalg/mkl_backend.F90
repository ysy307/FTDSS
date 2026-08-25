!>
!> Contains backend implementations for vector norm and dot product calculations.
!> It provides two versions of each routine: one using Intel MKL (if `_MKL`
!> is defined) and a native Fortran version. Both versions support MPI parallelism.
!> This module is intended to be used by `linalg_vector_operations`.
!>
module linalg_mkl_backend
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use, intrinsic :: ieee_arithmetic
    use, intrinsic :: ieee_exceptions
#ifdef _MPI
    use :: core_parallel_mpi
#endif
    use :: core_parallel_reduce, only:ownership_mask
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
    public :: axpy_mkl
#else
    public :: norm_1_native
    public :: norm_2_native
    public :: norm_inf_native
    public :: dot_native
    public :: axpy_native
#endif

contains

    ! =========================================================================
    ! 1. MKL Backend Implementations
    ! =========================================================================
#ifdef _MKL
    !>
    !> Computes the 1-norm of a vector using the MKL ASUM function.
    !>
    function norm_1_mkl(x) result(norm_value)
        implicit none
        !> The input vector.
        real(real64), intent(in) :: x(:)
        !> The computed 1-norm, \( \sum |x_i| \).
        real(real64) :: norm_value

        real(real64) :: local_norm
#ifdef _MPI
        integer(int32) :: ierr
#endif

        ! Summed over the entries this rank owns: a node on a partition
        ! boundary is stored on every rank that touches it, so summing every
        ! local entry would count it more than once.
        block
            logical, allocatable :: owned(:)
            owned = ownership_mask(int(size(x), int32))
            local_norm = sum(abs(x), mask=owned)
        end block
#ifdef _MPI
        call MPI_Allreduce(local_norm, norm_value, 1, MPI_REAL8, MPI_SUM, MPI_COMM_WORLD, ierr)
#else
        norm_value = local_norm
#endif
    end function norm_1_mkl

    !>
    !> Computes the 2-norm (Euclidean norm) of a vector using the MKL NRM2 function.
    !>
    function norm_2_mkl(x) result(norm_value)
        implicit none
        !> The input vector.
        real(real64), intent(in) :: x(:)
        !> The computed 2-norm, \( \sqrt{\sum x_i^2} \).
        real(real64) :: norm_value

        logical :: halt_overflow

        real(real64) :: local_scale, global_scale, local_sum, global_sum
#ifdef _MPI
        integer(int32) :: ierr
#endif

        call ieee_get_halting_mode(ieee_overflow, halt_overflow)
        call ieee_set_halting_mode(ieee_overflow, .false.)
        block
            logical, allocatable :: owned(:)
            owned = ownership_mask(int(size(x), int32))
            if (size(x) > 0) then
                local_scale = maxval(abs(x), mask=owned)
            else
                local_scale = 0.0d0
            end if
        end block
#ifdef _MPI
        call MPI_Allreduce(local_scale, global_scale, 1, MPI_REAL8, MPI_MAX, MPI_COMM_WORLD, ierr)
#else
        global_scale = local_scale
#endif
        block
            logical, allocatable :: owned(:)
            owned = ownership_mask(int(size(x), int32))
            if (global_scale > 0.0d0) then
                local_sum = sum((x / global_scale)**2, mask=owned)
            else
                local_sum = 0.0d0
            end if
        end block
#ifdef _MPI
        call MPI_Allreduce(local_sum, global_sum, 1, MPI_REAL8, MPI_SUM, MPI_COMM_WORLD, ierr)
#else
        global_sum = local_sum
#endif
        norm_value = global_scale * sqrt(global_sum)
        if (.not. ieee_is_finite(norm_value)) then
            norm_value = huge(1.0d0)
        end if
        call ieee_set_halting_mode(ieee_overflow, halt_overflow)
        call ieee_set_flag(ieee_overflow, .false.)
    end function norm_2_mkl

    !>
    !> Computes the infinity-norm (maximum absolute value) of a vector using the MKL IDAMAX.
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
    !> Computes the dot product of two vectors using the MKL DOT function.
    !>
    function dot_mkl(x, y) result(product)
        implicit none
        !> The first input vector.
        real(real64), intent(in) :: x(:)
        !> The second input vector.
        real(real64), intent(in) :: y(:)
        !> The computed dot product, \( \sum x_i y_i \).
        real(real64) :: product

        real(real64) :: local_prod
        logical :: halt_overflow, halt_invalid
#ifdef _MPI
        real(real64) :: global_prod
        integer(int32) :: ierr
#endif

    ! Temporarily disable FPE halting for the dot product computation.
    ! MKL ddot can overflow/produce NaN for ill-conditioned vectors;
    ! we check the result afterward.
    call ieee_get_halting_mode(ieee_overflow, halt_overflow)
    call ieee_get_halting_mode(ieee_invalid, halt_invalid)
    call ieee_set_halting_mode(ieee_overflow, .false.)
    call ieee_set_halting_mode(ieee_invalid, .false.)

        ! Over the entries this rank owns, for the same reason as the norms.
        block
            logical, allocatable :: owned(:)
            owned = ownership_mask(int(size(x), int32))
            local_prod = sum(x * y, mask=owned)
        end block
#ifdef _MPI
        call MPI_Allreduce(local_prod, global_prod, 1, MPI_REAL8, MPI_SUM, MPI_COMM_WORLD, ierr)
        product = global_prod
#else
        product = local_prod
#endif

    ! Clamp non-finite results to a large finite value preserving sign
    if (ieee_is_nan(product)) then
        product = 0.0d0
    else if (.not. ieee_is_finite(product)) then
        product = sign(huge(1.0d0), product)
    end if

    ! Restore original halting mode and clear raised flags
    call ieee_set_halting_mode(ieee_overflow, halt_overflow)
    call ieee_set_halting_mode(ieee_invalid, halt_invalid)
    call ieee_set_flag(ieee_overflow, .false.)
    call ieee_set_flag(ieee_invalid, .false.)

    end function dot_mkl

    !>
    !> Computes \( y \leftarrow \alpha x + y \) using the MKL DAXPY routine.
    !> Element-wise and local: no MPI communication required.
    !>
    subroutine axpy_mkl(alpha, x, y)
        implicit none
        !> The scalar multiplier.
        real(real64), intent(in) :: alpha
        !> The input vector x.
        real(real64), intent(in) :: x(:)
        !> The input/output vector y, overwritten with alpha*x + y.
        real(real64), intent(inout) :: y(:)

        if (is_contiguous(x) .and. is_contiguous(y)) then
            call daxpy(int(size(x), int32), alpha, x, 1, y, 1)
        else
            y = y + alpha * x
        end if
    end subroutine axpy_mkl
#endif

    ! =========================================================================
    ! 2. Native Fortran Backend Implementations
    ! =========================================================================
#ifndef _MKL
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

    !>
    !> Computes \( y \leftarrow \alpha x + y \) using native Fortran array arithmetic.
    !>
    subroutine axpy_native(alpha, x, y)
        implicit none
        !> The scalar multiplier.
        real(real64), intent(in) :: alpha
        !> The input vector x.
        real(real64), intent(in) :: x(:)
        !> The input/output vector y, overwritten with alpha*x + y.
        real(real64), intent(inout) :: y(:)

        y = y + alpha * x
    end subroutine axpy_native
#endif

end module linalg_mkl_backend
