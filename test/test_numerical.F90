program test_numerical
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64, output_unit
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    use :: mpi_f08
    use :: module_core
    use :: module_linalg
    use :: module_solver
    use :: numerical_special_functions_mkl, only: type_mkl_regularized_incomplete_beta
    implicit none

    integer(int32) :: ierr, mpi_thread_level, rank, num_procs
    integer(int32) :: failures

    failures = 0
    call MPI_Init_thread(MPI_THREAD_MULTIPLE, mpi_thread_level, ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, num_procs, ierr)

    call check_distributed_reductions(rank, num_procs, failures)
    call check_thread_safe_reductions(rank, num_procs, mpi_thread_level, failures)
    call benchmark_vector_api(rank, failures)
    call benchmark_special_functions(rank, failures)
    call check_nonsquare_dense_gemv(rank, failures)
    call benchmark_dense_matrix_api(rank, failures)
    call check_dense_gemm(rank, failures)

    if (rank == 0) then
        if (failures == 0) then
            write (output_unit, '(A)') 'All numerical checks passed.'
        else
            write (output_unit, '(A,I0)') 'Numerical check failures: ', failures
        end if
    end if

    call MPI_Finalize(ierr)
    if (failures /= 0) error stop 1

contains

    subroutine benchmark_vector_api(rank, failures)
        implicit none
        integer(int32), intent(in) :: rank
        integer(int32), intent(inout) :: failures

        integer(int32), parameter :: vector_size = 100000
        integer(int32), parameter :: repetitions = 100
        type(type_vector_dp) :: x, y, z
        real(real64), pointer :: x_data(:), y_data(:), z_data(:)
        real(real64) :: checksum
        integer(int64) :: clock_start, clock_end, clock_rate
        integer(int32) :: repetition

        call x%initialize(vector_size)
        call y%initialize(vector_size)
        call z%initialize(vector_size)
        x_data => x%get_data()
        y_data => y%get_data()
        z_data => z%get_data()
        x_data = 1.0d0
        y_data = 2.0d0
        z_data = 0.0d0
        call system_clock(count_rate=clock_rate)

        call system_clock(clock_start)
        do repetition = 1, repetitions
            checksum = vector_norm1(x)
        end do
        call system_clock(clock_end)
        call report_benchmark('vector_norm1', clock_start, clock_end, clock_rate, checksum, rank)

        call system_clock(clock_start)
        do repetition = 1, repetitions
            checksum = vector_norm2(x)
        end do
        call system_clock(clock_end)
        call report_benchmark('vector_norm2', clock_start, clock_end, clock_rate, checksum, rank)

        call system_clock(clock_start)
        do repetition = 1, repetitions
            checksum = vector_norminf(x)
        end do
        call system_clock(clock_end)
        call report_benchmark('vector_norminf', clock_start, clock_end, clock_rate, checksum, rank)

        call system_clock(clock_start)
        do repetition = 1, repetitions
            checksum = vector_dot(x, y)
        end do
        call system_clock(clock_end)
        call report_benchmark('vector_dot', clock_start, clock_end, clock_rate, checksum, rank)

        call system_clock(clock_start)
        do repetition = 1, repetitions
            call vector_axpy(0.0d0, x, y)
        end do
        call system_clock(clock_end)
        call report_benchmark('vector_axpy', clock_start, clock_end, clock_rate, sum(y_data), rank)

        call system_clock(clock_start)
        do repetition = 1, repetitions
            call vector_xpay(1.0d0, x, y)
        end do
        call system_clock(clock_end)
        call report_benchmark('vector_xpay', clock_start, clock_end, clock_rate, sum(y_data), rank)

        call system_clock(clock_start)
        do repetition = 1, repetitions
            call vector_axpyz(1.0d0, x, y, z)
        end do
        call system_clock(clock_end)
        call report_benchmark('vector_axpyz', clock_start, clock_end, clock_rate, sum(z_data), rank)

        call system_clock(clock_start)
        do repetition = 1, repetitions
            call vector_scale(1.0d0, z)
        end do
        call system_clock(clock_end)
        call report_benchmark('vector_scale', clock_start, clock_end, clock_rate, sum(z_data), rank)

        call system_clock(clock_start)
        do repetition = 1, repetitions
            call vector_abs(z)
        end do
        call system_clock(clock_end)
        call report_benchmark('vector_abs', clock_start, clock_end, clock_rate, sum(z_data), rank)

        z_data = 2.0d0
        call system_clock(clock_start)
        do repetition = 1, repetitions
            call vector_reciprocal(z)
        end do
        call system_clock(clock_end)
        call report_benchmark('vector_reciprocal', clock_start, clock_end, clock_rate, sum(z_data), rank)

        call system_clock(clock_start)
        do repetition = 1, repetitions
            call vector_shift(0_int32, z)
        end do
        call system_clock(clock_end)
        call report_benchmark('vector_shift', clock_start, clock_end, clock_rate, sum(z_data), rank)

        x_data = 1.0d0
        y_data = 2.0d0
        call system_clock(clock_start)
        do repetition = 1, repetitions
            call add(x, y, z)
        end do
        call system_clock(clock_end)
        call report_benchmark('add_vector', clock_start, clock_end, clock_rate, sum(z_data), rank)
        call check_true('vector add validation', maxval(abs(z_data - 3.0d0)) < 1.0d-12, rank, failures)

        call system_clock(clock_start)
        do repetition = 1, repetitions
            call subtract(y, x, z)
        end do
        call system_clock(clock_end)
        call report_benchmark('subtract_vector', clock_start, clock_end, clock_rate, sum(z_data), rank)
        call check_true('vector subtract validation', maxval(abs(z_data - 1.0d0)) < 1.0d-12, rank, failures)

        call system_clock(clock_start)
        do repetition = 1, repetitions
            call multiply(x, y, z)
        end do
        call system_clock(clock_end)
        call report_benchmark('multiply_vector', clock_start, clock_end, clock_rate, sum(z_data), rank)
        call check_true('vector multiply validation', maxval(abs(z_data - 2.0d0)) < 1.0d-12, rank, failures)

        call system_clock(clock_start)
        do repetition = 1, repetitions
            call divide(y, x, z)
        end do
        call system_clock(clock_end)
        call report_benchmark('divide_vector', clock_start, clock_end, clock_rate, sum(z_data), rank)
        call check_true('vector divide validation', maxval(abs(z_data - 2.0d0)) < 1.0d-12, rank, failures)
        call x%destroy()
        call y%destroy()
        call z%destroy()
    end subroutine benchmark_vector_api

    subroutine benchmark_special_functions(rank, failures)
        implicit none
        integer(int32), intent(in) :: rank
        integer(int32), intent(inout) :: failures

        integer(int32), parameter :: repetitions = 100000
        type(type_mkl_regularized_incomplete_beta) :: beta_function
        real(real64) :: result
        logical :: converged
        integer(int64) :: clock_start, clock_end, clock_rate
        integer(int32) :: repetition

        call system_clock(count_rate=clock_rate)
        call system_clock(clock_start)
        do repetition = 1, repetitions
            call beta_function%initialize(2.0d0, 3.0d0)
        end do
        call system_clock(clock_end)
        call report_benchmark('incomplete_beta_initialize', clock_start, clock_end, clock_rate, 1.0d0, rank)

        call beta_function%initialize(2.0d0, 3.0d0)
        result = 0.0d0
        converged = .false.
        call system_clock(clock_start)
        do repetition = 1, repetitions
            call beta_function%evaluate(0.5d0, result, converged)
        end do
        call system_clock(clock_end)
        call report_benchmark('incomplete_beta_evaluate', clock_start, clock_end, clock_rate, result, rank)
        call check_true('incomplete beta validation', converged .and. abs(result - 0.6875d0) < 1.0d-12, rank, failures)
    end subroutine benchmark_special_functions

    subroutine report_benchmark(name, clock_start, clock_end, clock_rate, checksum, rank)
        implicit none
        character(len=*), intent(in) :: name
        integer(int64), intent(in) :: clock_start, clock_end, clock_rate
        real(real64), intent(in) :: checksum
        integer(int32), intent(in) :: rank

        real(real64) :: elapsed

        elapsed = real(clock_end - clock_start, real64) / real(clock_rate, real64)
        if (rank == 0) write (output_unit, '(A,A,A,ES12.4,A,ES12.4)') 'BENCH ', trim(name), '[s]=', elapsed, &
            ' checksum=', checksum
    end subroutine report_benchmark

    subroutine check_distributed_reductions(rank, num_procs, failures)
        implicit none
        integer(int32), intent(in) :: rank, num_procs
        integer(int32), intent(inout) :: failures

        real(real64) :: x(2), y(2)
        real(real64) :: expected_norm1, expected_norm2, expected_norminf, expected_dot
        real(real64) :: actual
        integer(int32) :: process

        x = [real(rank + 1, real64), -2.0d0 * real(rank + 1, real64)]
        y = [0.5d0, -1.0d0]
        expected_norm1 = 0.0d0
        expected_norm2 = 0.0d0
        expected_norminf = 0.0d0
        expected_dot = 0.0d0
        do process = 0, num_procs - 1
            expected_norm1 = expected_norm1 + 3.0d0 * real(process + 1, real64)
            expected_norm2 = expected_norm2 + 5.0d0 * real(process + 1, real64)**2
            expected_norminf = max(expected_norminf, 2.0d0 * real(process + 1, real64))
            expected_dot = expected_dot + 2.5d0 * real(process + 1, real64)
        end do
        expected_norm2 = sqrt(expected_norm2)

        actual = vector_norm1(x)
        call check_close('distributed norm1', actual, expected_norm1, 1.0d-12, rank, failures)
        actual = vector_norm2(x)
        call check_close('distributed norm2', actual, expected_norm2, 1.0d-12, rank, failures)
        actual = vector_norminf(x)
        call check_close('distributed norminf', actual, expected_norminf, 1.0d-12, rank, failures)
        actual = vector_dot(x, y)
        call check_close('distributed dot', actual, expected_dot, 1.0d-12, rank, failures)
    end subroutine check_distributed_reductions

    subroutine check_thread_safe_reductions(rank, num_procs, mpi_thread_level, failures)
        implicit none
        integer(int32), intent(in) :: rank, num_procs, mpi_thread_level
        integer(int32), intent(inout) :: failures

        integer(int32), parameter :: num_evaluations = 64
        real(real64) :: x(8), y(8), results(num_evaluations), expected
        integer(int32) :: i

        if (num_procs /= 1 .or. mpi_thread_level < MPI_THREAD_MULTIPLE) return
        x = [1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0, 6.0d0, 7.0d0, 8.0d0]
        y = [8.0d0, 7.0d0, 6.0d0, 5.0d0, 4.0d0, 3.0d0, 2.0d0, 1.0d0]
        expected = 120.0d0
        results = 0.0d0

        !$omp parallel do default(none) shared(x, y, results)
        do i = 1, num_evaluations
            results(i) = vector_dot(x, y)
        end do
        !$omp end parallel do

        call check_true('thread-safe dot initialization', all(results == expected), rank, failures)
    end subroutine check_thread_safe_reductions

    subroutine check_nonsquare_dense_gemv(rank, failures)
        implicit none
        integer(int32), intent(in) :: rank
        integer(int32), intent(inout) :: failures

        type(type_matrix_dense) :: matrix
        real(real64), pointer :: values(:, :)
        real(real64) :: x(3), y(2), expected(2)
        integer(int32) :: ierr_local

        call matrix%initialize_rectangular(2, 3)
        values => matrix%get_val()
        values(:, :) = reshape([1.0d0, 4.0d0, 2.0d0, 5.0d0, 3.0d0, 6.0d0], [2, 3])
        x = [1.0d0, 2.0d0, 3.0d0]
        y = 0.0d0
        expected = [14.0d0, 32.0d0]
        ierr_local = MATRIX_STATUS%SUCCESS%ID
        call matrix_gemv(1.0d0, matrix, x, 0.0d0, y, ierr_local)
        call check_true('nonsquare dense GEMV status', ierr_local == MATRIX_STATUS%SUCCESS%ID, rank, failures)
        call check_true('nonsquare dense GEMV values', maxval(abs(y - expected)) < 1.0d-12, rank, failures)
        call matrix%destroy()
    end subroutine check_nonsquare_dense_gemv

    subroutine check_dense_gemm(rank, failures)
        implicit none
        integer(int32), intent(in) :: rank
        integer(int32), intent(inout) :: failures

        real(real64) :: A(2, 3), B(3, 2), C(2, 2), invalid_C(3, 2)
        integer(int32) :: ierr_local

        A = reshape([1.0d0, 4.0d0, 2.0d0, 5.0d0, 3.0d0, 6.0d0], shape(A))
        B = reshape([1.0d0, 0.0d0, 1.0d0, 0.0d0, 1.0d0, 1.0d0], shape(B))
        C = 0.0d0
        ierr_local = MATRIX_STATUS%SUCCESS%ID
        call matrix_gemm(A, B, C, ierr_local)
        call check_true('nonsquare dense GEMM status', ierr_local == MATRIX_STATUS%SUCCESS%ID, rank, failures)
        call check_true('nonsquare dense GEMM values', &
                        maxval(abs(C - reshape([4.0d0, 10.0d0, 5.0d0, 11.0d0], shape(C)))) < 1.0d-12, rank, failures)

        invalid_C = 0.0d0
        ierr_local = MATRIX_STATUS%SUCCESS%ID
        call matrix_gemm(A, B, invalid_C, ierr_local)
        call check_true('dense GEMM shape validation', ierr_local == MATRIX_STATUS%ILL_OPERATIONS%ID, rank, failures)
    end subroutine check_dense_gemm

    subroutine benchmark_dense_matrix_api(rank, failures)
        implicit none
        integer(int32), intent(in) :: rank
        integer(int32), intent(inout) :: failures

        integer(int32), parameter :: matrix_size = 64
        integer(int32), parameter :: repetitions = 200
        type(type_matrix_dense) :: A, B, C
        type(type_vector_dp) :: rhs, scale
        real(real64), pointer :: A_data(:, :), B_data(:, :), C_data(:, :), rhs_data(:)
        real(real64) :: raw_A(matrix_size, matrix_size), raw_B(matrix_size, matrix_size)
        real(real64) :: raw_C(matrix_size, matrix_size), x(matrix_size), y(matrix_size)
        real(real64) :: determinant, checksum
        integer(int64) :: clock_start, clock_end, clock_rate
        integer(int32) :: i, repetition, ierr_local

        call A%initialize(matrix_size)
        call B%initialize(matrix_size)
        call C%initialize(matrix_size)
        call rhs%initialize(matrix_size)
        call scale%initialize(matrix_size)
        A_data => A%get_val()
        B_data => B%get_val()
        C_data => C%get_val()
        rhs_data => rhs%get_data()
        A_data = 0.0d0
        B_data = 1.0d0
        do i = 1, matrix_size
            A_data(i, i) = 2.0d0
        end do
        raw_A = A_data
        raw_B = B_data
        raw_C = 0.0d0
        x = 1.0d0
        y = 0.0d0
        rhs_data = 1.0d0
        call system_clock(count_rate=clock_rate)

        call system_clock(clock_start)
        do repetition = 1, repetitions
            ierr_local = MATRIX_STATUS%SUCCESS%ID
            call matrix_axpy(0.0d0, A, B, ierr_local)
        end do
        call system_clock(clock_end)
        call report_benchmark('matrix_axpy', clock_start, clock_end, clock_rate, sum(B_data), rank)

        call system_clock(clock_start)
        do repetition = 1, repetitions
            ierr_local = MATRIX_STATUS%SUCCESS%ID
            call matrix_xpay(0.0d0, A, B, ierr_local)
        end do
        call system_clock(clock_end)
        call report_benchmark('matrix_xpay', clock_start, clock_end, clock_rate, sum(B_data), rank)

        call system_clock(clock_start)
        do repetition = 1, repetitions
            ierr_local = MATRIX_STATUS%SUCCESS%ID
            call matrix_axpyz(1.0d0, A, B, C, ierr_local)
        end do
        call system_clock(clock_end)
        call report_benchmark('matrix_axpyz', clock_start, clock_end, clock_rate, sum(C_data), rank)

        call system_clock(clock_start)
        do repetition = 1, repetitions
            ierr_local = MATRIX_STATUS%SUCCESS%ID
            call matrix_gemv(1.0d0, raw_A, x, 0.0d0, y, ierr_local)
        end do
        call system_clock(clock_end)
        call report_benchmark('matrix_gemv_array', clock_start, clock_end, clock_rate, sum(y), rank)

        call system_clock(clock_start)
        do repetition = 1, repetitions
            ierr_local = MATRIX_STATUS%SUCCESS%ID
            call matrix_gemv(1.0d0, A, x, 0.0d0, y, ierr_local)
        end do
        call system_clock(clock_end)
        call report_benchmark('matrix_gemv_dense', clock_start, clock_end, clock_rate, sum(y), rank)

        call system_clock(clock_start)
        do repetition = 1, repetitions
            ierr_local = MATRIX_STATUS%SUCCESS%ID
            call matrix_gemm(raw_A, raw_B, raw_C, ierr_local)
        end do
        call system_clock(clock_end)
        call report_benchmark('matrix_gemm', clock_start, clock_end, clock_rate, sum(raw_C), rank)

        raw_C = raw_A
        call system_clock(clock_start)
        do repetition = 1, repetitions
            raw_C = raw_A
            ierr_local = MATRIX_STATUS%SUCCESS%ID
            call matrix_inverse(raw_C, ierr_local)
        end do
        call system_clock(clock_end)
        call report_benchmark('matrix_inverse', clock_start, clock_end, clock_rate, sum(raw_C), rank)

        call system_clock(clock_start)
        do repetition = 1, repetitions
            ierr_local = MATRIX_STATUS%SUCCESS%ID
            call matrix_determinant(raw_A, determinant, ierr_local)
        end do
        call system_clock(clock_end)
        call report_benchmark('matrix_determinant', clock_start, clock_end, clock_rate, determinant, rank)

        call system_clock(clock_start)
        do repetition = 1, repetitions
            A_data = raw_A
            rhs_data = 1.0d0
            ierr_local = MATRIX_STATUS%SUCCESS%ID
            call matrix_scale(A, rhs, scale, MATRIX_OPS%SCALE_SYMM_DIAG, ierr_local)
        end do
        call system_clock(clock_end)
        checksum = sum(A_data) + sum(rhs_data)
        call report_benchmark('matrix_scale', clock_start, clock_end, clock_rate, checksum, rank)
        call check_true('dense matrix API validation', ierr_local == MATRIX_STATUS%SUCCESS%ID .and. &
                        abs(checksum - real(matrix_size, real64) * (1.0d0 + 1.0d0 / sqrt(2.0d0))) < 1.0d-10, &
                        rank, failures)

        call A%destroy()
        call B%destroy()
        call C%destroy()
        call rhs%destroy()
        call scale%destroy()
    end subroutine benchmark_dense_matrix_api

    subroutine check_close(name, actual, expected, tolerance, rank, failures)
        implicit none
        character(len=*), intent(in) :: name
        real(real64), intent(in) :: actual, expected, tolerance
        integer(int32), intent(in) :: rank
        integer(int32), intent(inout) :: failures

        call check_true(name, abs(actual - expected) <= tolerance * max(1.0d0, abs(expected)), rank, failures)
    end subroutine check_close

    subroutine check_true(name, condition, rank, failures)
        implicit none
        character(len=*), intent(in) :: name
        logical, intent(in) :: condition
        integer(int32), intent(in) :: rank
        integer(int32), intent(inout) :: failures

        if (condition) then
            if (rank == 0) write (output_unit, '(A,A)') 'PASS: ', trim(name)
        else
            failures = failures + 1
            write (output_unit, '(A,I0,A,A)') 'FAIL rank ', rank, ': ', trim(name)
        end if
    end subroutine check_true

end program test_numerical
