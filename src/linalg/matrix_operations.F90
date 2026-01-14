module linalg_matrix_operations
    use, intrinsic :: iso_fortran_env
    use :: module_core
    implicit none
    private

    public :: matrix_axpy
    public :: matrix_xpay
    public :: matrix_axpyz
    public :: matrix_scale
    public :: matrix_gemv
    public :: matrix_gemm
    public :: matrix_inverse
    public :: matrix_determinant
    public :: matvec

    interface matrix_gemv
        module procedure :: gemv_matrix_real64
        module procedure :: gemv_matrix_dense
        module procedure :: gemv_matrix_coo
        module procedure :: gemv_matrix_csr
        module procedure :: gemv_matrix_bsr
    end interface

    interface matrix_gemm
        module procedure :: matrix_gemm_real64
    end interface

    interface matrix_inverse
        module procedure :: matrix_inverse_real64
    end interface

    interface matrix_determinant
        module procedure :: matrix_determinant_real64
    end interface

    interface matvec
        module procedure :: matvec_real64
        module procedure :: matvec_matrix
    end interface

#ifdef _MKL
    interface
        subroutine dgemv(trans, m, n, alpha, a, lda, x, incx, beta, y, incy)
            import :: real64
            implicit none
            character(len=1), intent(in) :: trans
            integer, intent(in) :: m, n, lda, incx, incy
            real(real64), intent(in) :: alpha, beta
            real(real64), intent(in) :: a(lda, *), x(*), y(*)
        end subroutine dgemv

        subroutine dgemm(transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
            import :: real64
            implicit none
            character(len=1), intent(in) :: transa, transb
            integer, intent(in) :: m, n, k, lda, ldb, ldc
            real(real64), intent(in) :: alpha, beta
            real(real64), intent(in) :: a(lda, *), b(ldb, *)
            real(real64), intent(inout) :: c(ldc, *)
        end subroutine dgemm

        ! ------------------------------------------------------------------
        ! dgetrf: Computes the LU factorization of a general m-by-n matrix
        ! ------------------------------------------------------------------
        subroutine dgetrf(m, n, a, lda, ipiv, info)
            import :: real64
            implicit none
            integer, intent(in) :: m, n, lda
            real(real64), intent(inout) :: a(lda, *)
            integer, intent(inout) :: ipiv(*)
            integer, intent(inout) :: info
        end subroutine dgetrf

        ! ------------------------------------------------------------------
        ! dgetri: Computes the inverse of an LU-factored general matrix
        ! ------------------------------------------------------------------
        subroutine dgetri(n, a, lda, ipiv, work, lwork, info)
            import :: real64
            implicit none
            integer, intent(in) :: n, lda, lwork
            real(real64), intent(inout) :: a(lda, *)
            integer, intent(in) :: ipiv(*)
            real(real64), intent(inout) :: work(*)
            integer, intent(inout) :: info
        end subroutine dgetri
    end interface
#endif

contains

    !> Compute \( B = \alpha A + B \) for matrices.
    !> Performs the update on matrix B.
    subroutine matrix_axpy(alpha, A, B, ierr)
        implicit none
        !> Scaling factor \( \alpha \)
        real(real64), intent(in) :: alpha
        !> Input matrix A
        class(abst_matrix), intent(in) :: A
        !> Input/Output matrix B
        class(abst_matrix), intent(inout) :: B
        !> Error status
        integer(int32), intent(inout) :: ierr

        real(real64), dimension(:, :), pointer :: A_data
        real(real64), dimension(:, :), pointer :: B_data

        select type (A)
        type is (type_matrix_dense)
            select type (B)
            type is (type_matrix_dense)
                A_data => A%get_val()
                B_data => B%get_val()
                B_data = alpha * A_data + B_data
                ierr = MATRIX_STATUS_SUCCESS
            class default
                ierr = MATRIX_STATUS_ILL_OPERATIONS
            end select
        class default
            ierr = MATRIX_STATUS_ILL_OPERATIONS
        end select

    end subroutine matrix_axpy

    !> Compute \( B = A + \alpha B \) for matrices.
    !> Performs the update on matrix B.
    subroutine matrix_xpay(alpha, A, B, ierr)
        implicit none
        !> Scaling factor \( \alpha \)
        real(real64), intent(in) :: alpha
        !> Input matrix A
        class(abst_matrix), intent(in) :: A
        !> Input/Output matrix B
        class(abst_matrix), intent(inout) :: B
        !> Error status
        integer(int32), intent(inout) :: ierr

        real(real64), dimension(:, :), pointer :: A_data
        real(real64), dimension(:, :), pointer :: B_data

        select type (A)
        type is (type_matrix_dense)
            select type (B)
            type is (type_matrix_dense)
                A_data => A%get_val()
                B_data => B%get_val()
                B_data = A_data + alpha * B_data
                ierr = MATRIX_STATUS_SUCCESS
            class default
                ierr = MATRIX_STATUS_ILL_OPERATIONS
            end select
        class default
            ierr = MATRIX_STATUS_ILL_OPERATIONS
        end select

    end subroutine matrix_xpay

    !> Compute \( C = \alpha A + B \) for matrices.
    !> Stores the result in matrix C.
    subroutine matrix_axpyz(alpha, A, B, C, ierr)
        implicit none
        !> Scaling factor \( \alpha \)
        real(real64), intent(in) :: alpha
        !> Input matrix A
        class(abst_matrix), intent(in) :: A
        !> Input matrix B
        class(abst_matrix), intent(in) :: B
        !> Output matrix C
        class(abst_matrix), intent(inout) :: C
        !> Error status
        integer(int32), intent(inout) :: ierr

        real(real64), dimension(:, :), pointer :: A_data
        real(real64), dimension(:, :), pointer :: B_data
        real(real64), dimension(:, :), pointer :: C_data

        select type (A)
        type is (type_matrix_dense)
            select type (B)
            type is (type_matrix_dense)
                select type (C)
                type is (type_matrix_dense)
                    A_data => A%get_val()
                    B_data => B%get_val()
                    C_data => C%get_val()
                    C_data = alpha * A_data + B_data
                    ierr = MATRIX_STATUS_SUCCESS
                class default
                    ierr = MATRIX_STATUS_ILL_OPERATIONS
                end select
            class default
                ierr = MATRIX_STATUS_ILL_OPERATIONS
            end select
        class default
            ierr = MATRIX_STATUS_ILL_OPERATIONS
        end select

    end subroutine matrix_axpyz

    !> Scale the matrix and the right-hand side vector.
    !> Used for symmetric diagonal scaling or Jacobi scaling.
    subroutine matrix_scale(A, b, d, op, ierr)
        implicit none
        !> Matrix to be scaled
        class(abst_matrix), intent(inout) :: A
        !> RHS vector to be scaled
        type(type_vector_dp), intent(inout) :: b
        !> Vector to store the scaling factors (diagonal elements)
        type(type_vector_dp), intent(inout) :: d
        !> Scaling operation identifier
        integer(int32), intent(in) :: op
        !> Error status
        integer(int32), intent(inout) :: ierr

        real(real64), dimension(:), pointer :: diag
        integer(int32) :: i

        call d%copy(b)
        call A%get_diagonal(d)
        diag => d%get_data()

        ! Zero division check
        do i = 1, size(diag)
            if (abs(diag(i)) < epsilon(1.0_real64)) then
                ! Handle zero diagonal element safely to avoid NaN
                diag(i) = 1.0d0
            end if
        end do

        select case (op)
        case (OP_SCALE_SYMM_DIAG)
            diag = 1.0d0 / sqrt(abs(diag))
        case (OP_SCALE_JACOBI)
            diag = 1.0d0 / diag
        case default
            ierr = MATRIX_STATUS_ILL_OPERATIONS
            return
        end select

        call A%scale(op, d)
        call b%scale(op, d)

    end subroutine matrix_scale

    !> Perform general matrix-vector multiplication for real64 matrices.
    !> Computes \( y = \alpha A x + \beta y \).
    !> [FIX] Added contiguous attribute to avoid array temporary creation warning.
    subroutine gemv_matrix_real64(alpha, A, x, beta, y, ierr)
        implicit none
        !> Scalar \( \alpha \)
        real(real64), intent(in) :: alpha
        !> Dense matrix A (contiguous)
        real(real64), intent(in), contiguous :: A(:, :)
        !> Input vector x (contiguous)
        real(real64), intent(in), contiguous :: x(:)
        !> Scalar \( \beta \)
        real(real64), intent(in) :: beta
        !> Input/Output vector y (contiguous)
        real(real64), intent(inout), contiguous :: y(:)
        !> Error status
        integer(int32), intent(inout) :: ierr

        integer(int32) :: i
        integer(int32) :: num_row, num_col

        num_row = size(A, 1)
        num_col = size(A, 2)
#ifdef _MKL
        ! Warning (406) fixed by declaring A as contiguous
        call dgemv('N', num_row, num_col, alpha, A, num_row, x, 1, beta, y, 1)
#else
        !$omp parallel do private(i)
        do i = 1, num_row
            y(i) = alpha * dot_product(A(i, :), x) + beta * y(i)
        end do
        !$omp end parallel do
#endif
        ierr = MATRIX_STATUS_SUCCESS

    end subroutine gemv_matrix_real64

    !> Perform general matrix-vector multiplication for dense matrices.
    !> Computes \( y = \alpha A x + \beta y \).
    subroutine gemv_matrix_dense(alpha, A, x, beta, y, ierr)
        implicit none
        !> Scalar \( \alpha \)
        real(real64), intent(in) :: alpha
        !> Dense matrix A
        class(type_matrix_dense), intent(in) :: A
        !> Input vector x
        real(real64), intent(in) :: x(:)
        !> Scalar \( \beta \)
        real(real64), intent(in) :: beta
        !> Input/Output vector y
        real(real64), intent(inout) :: y(:)
        !> Error status
        integer(int32), intent(inout) :: ierr

        type(type_matrix_info) :: info
        integer(int32) :: i

#ifdef _MKL
        real(real64), dimension(:, :), pointer :: val_ptr

        call A%get_info(info)
        if (info%num_rows /= size(x) .or. info%num_cols /= size(y)) then
            ierr = MATRIX_STATUS_ILL_OPERATIONS
            return
        end if

        val_ptr => A%get_val()
        ! Ensure pointer target is handled correctly by MKL
        call dgemv('N', info%num_rows, info%num_cols, alpha, val_ptr, info%num_rows, x, 1, beta, y, 1)
#else
        call A%get_info(info)
        !$omp parallel do private(i)
        do i = 1, info%num_rows
            y(i) = alpha * dot_product(A%val(i, :), x) + beta * y(i)
        end do
        !$omp end parallel do
#endif
        ierr = MATRIX_STATUS_SUCCESS

    end subroutine gemv_matrix_dense

    !> Perform general matrix-vector multiplication for CSR matrices.
    !> Computes \( y = \alpha A x + \beta y \).
    subroutine gemv_matrix_csr(alpha, A, x, beta, y, ierr)
        implicit none
        !> Scalar \( \alpha \)
        real(real64), intent(in) :: alpha
        !> CSR matrix A
        class(type_matrix_csr), intent(in) :: A
        !> Input vector x
        real(real64), intent(in) :: x(:)
        !> Scalar \( \beta \)
        real(real64), intent(in) :: beta
        !> Input/Output vector y
        real(real64), intent(inout) :: y(:)
        !> Error status
        integer(int32), intent(inout) :: ierr

        type(type_matrix_info) :: info
        integer(int32), dimension(:), pointer :: ind
        integer(int32), dimension(:), pointer :: ptr
        real(real64), dimension(:), pointer :: val
        integer(int32) :: i, j, is, ie
        real(real64) :: sum

        call A%get_info(info)
        if (info%num_nodes /= size(x) .or. info%num_nodes /= size(y)) then
            ierr = MATRIX_STATUS_ILL_OPERATIONS
            return
        end if
        ind => A%get_ind()
        ptr => A%get_ptr()
        val => A%get_val()

        !$omp parallel do private(i, j, is, ie, sum)
        do i = 1, info%num_rows
            sum = 0.0d0
            is = ptr(i)
            ie = ptr(i + 1) - 1
            do j = is, ie
                sum = sum + val(j) * x(ind(j))
            end do
            y(i) = alpha * sum + beta * y(i)
        end do
        !$omp end parallel do

        ierr = MATRIX_STATUS_SUCCESS
    end subroutine gemv_matrix_csr

    !> Perform general matrix-vector multiplication for COO matrices.
    !> Computes \( y = \alpha A x + \beta y \).
    !> Uses atomic updates for parallel execution.
    subroutine gemv_matrix_coo(alpha, A, x, beta, y, ierr)
        implicit none
        !> Scalar \( \alpha \)
        real(real64), intent(in) :: alpha
        !> COO matrix A
        class(type_matrix_coo), intent(in) :: A
        !> Input vector x
        real(real64), intent(in) :: x(:)
        !> Scalar \( \beta \)
        real(real64), intent(in) :: beta
        !> Input/Output vector y
        real(real64), intent(inout) :: y(:)
        !> Error status
        integer(int32), intent(inout) :: ierr

        integer(int32) :: i
        type(type_matrix_info) :: info
        integer(int32), dimension(:), pointer :: col
        integer(int32), dimension(:), pointer :: row
        real(real64), dimension(:), pointer :: val

        call A%get_info(info)
        if (info%num_nodes /= size(x) .or. info%num_nodes /= size(y)) then
            ierr = MATRIX_STATUS_ILL_OPERATIONS
            return
        end if
        col => A%get_col()
        row => A%get_row()
        val => A%get_val()

        ! First, scale the entire y vector by beta
        if (beta == 0.0d0) then
            y = 0.0d0
        else
            y = beta * y
        end if

        ! Add the contribution of each non-zero element
        ! Note: Atomic update may cause bitwise non-reproducibility in parallel
        !$omp parallel do
        do i = 1, info%nnz
            !$omp atomic update
            y(row(i)) = y(row(i)) + alpha * val(i) * x(col(i))
        end do
        !$omp end parallel do

        ierr = MATRIX_STATUS_SUCCESS

    end subroutine gemv_matrix_coo

    !> Perform general matrix-vector multiplication for BSR matrices.
    !> Computes \( y = \alpha A x + \beta y \).
    subroutine gemv_matrix_bsr(alpha, A, x, beta, y, ierr)
        implicit none
        !> Scalar \( \alpha \)
        real(real64), intent(in) :: alpha
        !> BSR matrix A
        class(type_matrix_bsr), intent(in) :: A
        !> Input vector x
        real(real64), intent(in) :: x(:)
        !> Scalar \( \beta \)
        real(real64), intent(in) :: beta
        !> Input/Output vector y
        real(real64), intent(inout) :: y(:)
        !> Error status
        integer(int32), intent(inout) :: ierr

        type(type_matrix_info) :: info
        integer(int32), dimension(:), pointer :: ind
        integer(int32), dimension(:), pointer :: ptr
        real(real64), dimension(:, :, :), pointer :: val
        integer(int32) :: i, k, rb, cb, col
        integer(int32) :: R, C
        integer(int32) :: x_idx, y_idx
        real(real64) :: sum

        call A%get_info(info)

        ! Block dimensions
        R = info%num_block_rows
        C = info%num_block_cols

        ! Validate dimensions
        if (info%num_nodes * C /= size(x) .or. info%num_nodes * R /= size(y)) then
            ierr = MATRIX_STATUS_ILL_OPERATIONS
            return
        end if

        ind => A%get_ind()
        ptr => A%get_ptr()
        val => A%get_val()

        !$omp parallel do private(i, k, col, rb, cb, x_idx, y_idx, sum)
        do i = 1, info%num_nodes ! Iterate over block rows
            ! Iterate over rows within the current block row (local DOF)
            do rb = 1, R
                sum = 0.0d0
                ! Iterate over blocks in the row
                do k = ptr(i), ptr(i + 1) - 1
                    col = ind(k)
                    ! Perform block multiplication for the current local row
                    do cb = 1, C
                        x_idx = (col - 1) * C + cb
                        sum = sum + val(rb, cb, k) * x(x_idx)
                    end do
                end do
                y_idx = (i - 1) * R + rb
                y(y_idx) = alpha * sum + beta * y(y_idx)
            end do
        end do
        !$omp end parallel do

        ierr = MATRIX_STATUS_SUCCESS

    end subroutine gemv_matrix_bsr

    !> General matrix-matrix multiplication.
    !> [FIX] Added contiguous attribute to inputs.
    subroutine matrix_gemm_real64(A, B, C, ierr)
        implicit none
        !> Input matrix A (contiguous)
        real(real64), intent(in), contiguous :: A(:, :)
        !> Input matrix B (contiguous)
        real(real64), intent(in), contiguous :: B(:, :)
        !> Output matrix C (contiguous)
        real(real64), intent(inout), contiguous :: C(:, :)
        !> Error status
        integer(int32), intent(inout) :: ierr

#ifdef _MKL
        integer(int32) :: m, n, k
        m = size(A, 1)
        k = size(A, 2)
        n = size(B, 2)
        call dgemm('N', 'N', m, n, k, 1.0d0, A, m, B, k, 0.0d0, C, m)
        ierr = MATRIX_STATUS_SUCCESS
#else
        integer(int32) :: i, j, l
        integer(int32) :: m, n, k
        m = size(A, 1)
        k = size(A, 2)
        n = size(B, 2)
        !$omp parallel do private(i, j, l)
        do i = 1, m
            do j = 1, n
                C(i, j) = 0.0d0
                do l = 1, k
                    C(i, j) = C(i, j) + A(i, l) * B(l, j)
                end do
            end do
        end do
        !$omp end parallel do
        ierr = MATRIX_STATUS_SUCCESS
#endif
    end subroutine matrix_gemm_real64

    !> Compute the inverse of a square matrix A.
    !> The result overwrites A (In-place).
    !> Uses analytical formulas (Cramer's rule variant) for N <= 3.
    !> For N > 3: Uses LAPACK (dgetrf/dgetri) if _MKL is defined.
    !> [FIX] Corrected fallback implementation (Gauss-Jordan with identity augmentation) for non-MKL case.
    subroutine matrix_inverse_real64(A, ierr)
        implicit none
        !> Input/Output matrix A. On exit, contains the inverse.
        real(real64), intent(inout) :: A(:, :)
        !> Error status
        integer(int32), intent(inout) :: ierr

        integer(int32) :: n, m
        real(real64) :: det, invDet
        real(real64) :: T(3, 3) ! Temporary array for small matrices

#ifdef _MKL
        integer(int32), allocatable :: ipiv(:)
        real(real64), allocatable :: work(:)
        integer(int32) :: lwork
        integer(int32) :: info
        real(real64) :: work_query(1)
#else
        ! Variables for Fallback Gauss-Jordan
        real(real64), allocatable :: invA(:, :)
        real(real64), allocatable :: temp_row_A(:)
        real(real64), allocatable :: temp_row_inv(:)
        real(real64) :: pivot_val, temp_val
        integer(int32) :: i, j, k, pivot_idx
#endif

        n = size(A, 1)
        m = size(A, 2)

        ! Validate square matrix
        if (n /= m) then
            ierr = MATRIX_STATUS_ILL_OPERATIONS
            return
        end if

        ! -----------------------------------------------------------------------
        ! 1. Analytical Solution for Small Matrices (N <= 3)
        ! -----------------------------------------------------------------------
        if (n <= 3) then
            ierr = MATRIX_STATUS_SUCCESS

            select case (n)
            case (1)
                det = A(1, 1)
                if (abs(det) < epsilon(1.0_real64)) then
                    ierr = MATRIX_STATUS_ILL_OPERATIONS
                    return
                end if
                A(1, 1) = 1.0d0 / det

            case (2)
                det = A(1, 1) * A(2, 2) - A(1, 2) * A(2, 1)
                if (abs(det) < epsilon(1.0_real64)) then
                    ierr = MATRIX_STATUS_ILL_OPERATIONS
                    return
                end if
                invDet = 1.0d0 / det

                ! Use temporary variables to allow in-place update
                T(1, 1) = A(2, 2) * invDet
                T(1, 2) = -A(1, 2) * invDet
                T(2, 1) = -A(2, 1) * invDet
                T(2, 2) = A(1, 1) * invDet

                A(1:2, 1:2) = T(1:2, 1:2)

            case (3)
                ! Sarrus rule / Cofactor expansion
                T(1:3, 1:3) = A(1:3, 1:3)

                det = T(1, 1) * (T(2, 2) * T(3, 3) - T(2, 3) * T(3, 2)) &
                      - T(1, 2) * (T(2, 1) * T(3, 3) - T(2, 3) * T(3, 1)) &
                      + T(1, 3) * (T(2, 1) * T(3, 2) - T(2, 2) * T(3, 1))

                if (abs(det) < epsilon(1.0_real64)) then
                    ierr = MATRIX_STATUS_ILL_OPERATIONS
                    return
                end if
                invDet = 1.0d0 / det

                ! Compute Adjugate Matrix * invDet
                A(1, 1) = (T(2, 2) * T(3, 3) - T(2, 3) * T(3, 2)) * invDet
                A(1, 2) = (T(1, 3) * T(3, 2) - T(1, 2) * T(3, 3)) * invDet
                A(1, 3) = (T(1, 2) * T(2, 3) - T(1, 3) * T(2, 2)) * invDet

                A(2, 1) = (T(2, 3) * T(3, 1) - T(2, 1) * T(3, 3)) * invDet
                A(2, 2) = (T(1, 1) * T(3, 3) - T(1, 3) * T(3, 1)) * invDet
                A(2, 3) = (T(1, 3) * T(2, 1) - T(1, 1) * T(2, 3)) * invDet

                A(3, 1) = (T(2, 1) * T(3, 2) - T(2, 2) * T(3, 1)) * invDet
                A(3, 2) = (T(1, 2) * T(3, 1) - T(1, 1) * T(3, 2)) * invDet
                A(3, 3) = (T(1, 1) * T(2, 2) - T(1, 2) * T(2, 1)) * invDet
            end select

            return
        end if

#ifdef _MKL
        ! -----------------------------------------------------------------------
        ! 2. MKL (LAPACK) Implementation for N > 3
        ! -----------------------------------------------------------------------
        call allocate_array(ipiv, n)

        ! LU Factorization
        call dgetrf(n, n, A, n, ipiv, info)

        if (info /= 0) then
            call deallocate_array(ipiv)
            ierr = MATRIX_STATUS_ILL_OPERATIONS
            return
        end if

        ! Query optimal workspace size
        lwork = -1
        call dgetri(n, A, n, ipiv, work_query, lwork, info)

        lwork = int(work_query(1))
        call allocate_array(work, lwork)

        ! Compute inverse
        call dgetri(n, A, n, ipiv, work, lwork, info)

        if (info /= 0) then
            ierr = MATRIX_STATUS_ILL_OPERATIONS
        else
            ierr = MATRIX_STATUS_SUCCESS
        end if

        call deallocate_array(ipiv)
        call deallocate_array(work)

#else
        ! -----------------------------------------------------------------------
        ! 3. Fallback Implementation (Gauss-Jordan) for N > 3
        ! -----------------------------------------------------------------------
        ! Initialize auxiliary identity matrix
        call allocate_array(invA, n, n)
        invA = 0.0d0
        do i = 1, n
            invA(i, i) = 1.0d0
        end do

        call allocate_array(temp_row_A, n)
        call allocate_array(temp_row_inv, n)
        ierr = MATRIX_STATUS_SUCCESS

        do i = 1, n
            ! Find pivot
            pivot_idx = i
            pivot_val = abs(A(i, i))
            do k = i + 1, n
                if (abs(A(k, i)) > pivot_val) then
                    pivot_idx = k
                    pivot_val = abs(A(k, i))
                end if
            end do

            ! Check singularity
            if (pivot_val < epsilon(1.0_real64)) then
                ierr = MATRIX_STATUS_ILL_OPERATIONS
                exit
            end if

            ! Swap rows (both A and invA)
            if (pivot_idx /= i) then
                temp_row_A = A(i, :)
                A(i, :) = A(pivot_idx, :)
                A(pivot_idx, :) = temp_row_A

                temp_row_inv = invA(i, :)
                invA(i, :) = invA(pivot_idx, :)
                invA(pivot_idx, :) = temp_row_inv
            end if

            ! Scale pivot row
            temp_val = 1.0d0 / A(i, i)
            A(i, :) = A(i, :) * temp_val
            invA(i, :) = invA(i, :) * temp_val

            ! Eliminate other rows
            do k = 1, n
                if (k /= i) then
                    temp_val = A(k, i)
                    A(k, :) = A(k, :) - temp_val * A(i, :)
                    invA(k, :) = invA(k, :) - temp_val * invA(i, :)
                end if
            end do
        end do

        ! If successful, copy result back
        if (ierr == MATRIX_STATUS_SUCCESS) then
            A = invA
        end if

        call deallocate_array(invA)
        call deallocate_array(temp_row_A)
        call deallocate_array(temp_row_inv)
#endif

    end subroutine matrix_inverse_real64

    !> Compute the determinant of a square matrix A.
    !> Input A is preserved (intent(in)).
    subroutine matrix_determinant_real64(A, det, ierr)
        implicit none
        real(real64), intent(in) :: A(:, :)
        real(real64), intent(inout) :: det
        integer(int32), intent(inout) :: ierr

        integer(int32) :: n, m
        real(real64), allocatable :: temp_A(:, :)

#ifdef _MKL
        integer(int32), allocatable :: ipiv(:)
        integer(int32) :: info
        integer(int32) :: i
#else
        integer(int32) :: i, k, pivot_idx
        real(real64) :: pivot_val, factor
        real(real64), allocatable :: temp_row(:)
        real(real64) :: det_sign
#endif

        n = size(A, 1)
        m = size(A, 2)

        if (n /= m) then
            ierr = MATRIX_STATUS_ILL_OPERATIONS
            det = 0.0d0
            return
        end if
        ierr = MATRIX_STATUS_SUCCESS

        ! --- N <= 3: Analytical Solution ---
        if (n <= 3) then
            select case (n)
            case (1)
                det = A(1, 1)
            case (2)
                det = A(1, 1) * A(2, 2) - A(1, 2) * A(2, 1)
            case (3)
                det = A(1, 1) * (A(2, 2) * A(3, 3) - A(2, 3) * A(3, 2)) &
                      - A(1, 2) * (A(2, 1) * A(3, 3) - A(2, 3) * A(3, 1)) &
                      + A(1, 3) * (A(2, 1) * A(3, 2) - A(2, 2) * A(3, 1))
            end select
            return
        end if

        ! --- N > 3: LU Decomposition or Gaussian Elimination ---
        call allocate_array(temp_A, n, n)
        temp_A = A

#ifdef _MKL
        call allocate_array(ipiv, n)

        ! LU Factorization (P * A = L * U)
        call dgetrf(n, n, temp_A, n, ipiv, info)

        if (info < 0) then
            ierr = MATRIX_STATUS_ILL_OPERATIONS
            det = 0.0d0
        else if (info > 0) then
            ! Singular matrix
            det = 0.0d0
        else
            ! Det = product(diag(U)) * (-1)^swaps
            det = 1.0d0
            do i = 1, n
                det = det * temp_A(i, i)
                if (ipiv(i) /= i) det = -det
            end do
        end if

        call deallocate_array(ipiv)
#else
        call allocate_array(temp_row, n)
        det_sign = 1.0d0
        det = 1.0d0

        do i = 1, n
            ! Pivot search
            pivot_idx = i
            pivot_val = abs(temp_A(i, i))
            do k = i + 1, n
                if (abs(temp_A(k, i)) > pivot_val) then
                    pivot_idx = k
                    pivot_val = abs(temp_A(k, i))
                end if
            end do

            if (pivot_val < epsilon(1.0_real64)) then
                det = 0.0d0
                exit
            end if

            ! Swap
            if (pivot_idx /= i) then
                temp_row = temp_A(i, :)
                temp_A(i, :) = temp_A(pivot_idx, :)
                temp_A(pivot_idx, :) = temp_row
                det_sign = -det_sign
            end if

            ! Eliminate
            do k = i + 1, n
                factor = temp_A(k, i) / temp_A(i, i)
                temp_A(k, i + 1:) = temp_A(k, i + 1:) - factor * temp_A(i, i + 1:)
            end do

            det = det * temp_A(i, i)
        end do

        det = det * det_sign
        call deallocate_array(temp_row)
#endif

        call deallocate_array(temp_A)

    end subroutine matrix_determinant_real64

    !> Compute matrix-vector multiplication \( y = A x \).
    !> Wrapper for GEMV with \( \alpha = 1, \beta = 0 \).
    subroutine matvec_real64(A, x, y, ierr)
        implicit none
        !> System matrix A
        real(real64), intent(in) :: A(:, :)
        !> Input vector x
        real(real64), intent(in) :: x(:)
        !> Output vector y
        real(real64), intent(inout) :: y(:)
        !> Error status
        integer(int32), intent(inout) :: ierr

        call gemv_matrix_real64(1.0d0, A, x, 0.0d0, y, ierr)
    end subroutine matvec_real64

    !> Compute matrix-vector multiplication \( y = A x \).
    !> Wrapper for GEMV with \( \alpha = 1, \beta = 0 \).
    subroutine matvec_matrix(A, x, y, ierr)
        implicit none
        !> System matrix A
        class(abst_matrix), intent(in) :: A
        !> Input vector x
        class(type_vector_dp), intent(in) :: x
        !> Output vector y
        class(type_vector_dp), intent(inout) :: y
        !> Error status
        integer(int32), intent(inout) :: ierr

        real(real64), dimension(:), pointer :: x_data
        real(real64), dimension(:), pointer :: y_data

        x_data => x%get_data()
        y_data => y%get_data()
        select type (A)
        type is (type_matrix_dense)
            call gemv_matrix_dense(1.0d0, A, x_data, 0.0d0, y_data, ierr)
        type is (type_matrix_csr)
            call gemv_matrix_csr(1.0d0, A, x_data, 0.0d0, y_data, ierr)
        type is (type_matrix_coo)
            call gemv_matrix_coo(1.0d0, A, x_data, 0.0d0, y_data, ierr)
        type is (type_matrix_bsr)
            call gemv_matrix_bsr(1.0d0, A, x_data, 0.0d0, y_data, ierr)
        class default
            ierr = MATRIX_STATUS_ILL_OPERATIONS
        end select

    end subroutine matvec_matrix

end module linalg_matrix_operations
