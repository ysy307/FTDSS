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
    public :: matvec

    interface matrix_gemv
        module procedure :: gemv_matrix_dense
        module procedure :: gemv_matrix_coo
        module procedure :: gemv_matrix_csr
        module procedure :: gemv_matrix_bsr
    end interface

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

        call d%copy(b)
        call A%get_diagonal(d)
        diag => d%get_data()

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

        integer(int32) :: i
        integer(int32) :: num_row, num_col
        type(type_matrix_info) :: info

#ifdef _MKL
        interface
            subroutine dgemv(trans, m, n, alpha, a, lda, x, incx, beta, y, incy)
                use, intrinsic :: iso_fortran_env
                implicit none
                character(len=1), intent(in) :: trans
                integer, intent(in) :: m, n, lda, incx, incy
                real(real64), intent(in) :: alpha, beta
                real(real64), intent(in) :: a(lda, *), x(*), y(*)
            end subroutine dgemv
        end interface

        call A%get_info(info)
        if (info%num_rows /= size(x) .or. info%num_cols /= size(y)) then
            ierr = MATRIX_STATUS_ILL_OPERATIONS
            return
        end if
        call dgemv('N', info%num_rows, info%num_cols, alpha, A%val, info%num_rows, x, 1, beta, y, 1)
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

    !> Compute matrix-vector multiplication \( y = A x \).
    !> Wrapper for GEMV with \( \alpha = 1, \beta = 0 \).
    subroutine matvec(A, x, y, ierr)
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

    end subroutine matvec
end module linalg_matrix_operations
