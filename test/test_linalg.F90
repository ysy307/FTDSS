program test_linalg
    use, intrinsic :: iso_fortran_env
#ifdef _MPI
    use :: mpi_f08
#endif
    use :: module_core
    use :: module_linalg ! Imports vector types and ops
    implicit none
    integer(int32) :: unit
#ifdef _MPI

    integer(int32) :: ierr
    call MPI_Init(ierr)
#endif
    open (newunit=unit, file="log/test/linalg.log", status="replace", action="write", iostat=ierr)
    ! Initialize the linear algebra backend (MKL or Native)
    call initialize_linalg()
    call run_test_vector_ops_dp()
    write (unit, *) ""
    call run_test_matrix_operations()

    write (unit, *) "========================================"
    write (unit, *) "   Linalg tests completed."
    write (unit, *) "========================================"

#ifdef _MPI
    call MPI_Finalize(ierr)
#endif

contains

    !>
    !> Operation tests for type_vector_dp
    !> Arithmetic, Norms, Dot Product
    !>
    subroutine run_test_vector_ops_dp()
        type(type_vector_dp) :: v1, v2, res
        integer(int32), parameter :: N = 5
        real(real64) :: n1, n2, ninf, dot_val

        real(real64), dimension(:), allocatable :: vals1, vals2

        write (unit, *) "========================================"
        write (unit, *) "  Vector operations Tests"
        write (unit, *) "========================================"
        write (unit, *) "--- Testing DP Vector Operations ---"

        call v1%initialize(N)
        call v2%initialize(N)
        call res%initialize(N)

        ! v1 = [1, -2, 3, -4, 5]
        call v1%set(OP_INS, [1.0d0, -2.0d0, 3.0d0, -4.0d0, 5.0d0])

        ! v2 = [1, 1, 1, 1, 1]
        call v2%set(OP_INS, 1.0d0)

        ! ---------------------------------------------------------
        ! Norms
        ! ---------------------------------------------------------
        n1 = vector_norm1(v1)
        if (abs(n1 - 15.0d0) < 1.0d-10) then
            write (unit, *) "[PASS] Norm1"
        else
            write (unit, *) "[FAIL] Norm1", n1
        end if
        n2 = vector_norm2(v1)
        if (abs(n2 - 7.416198487095663d0) < 1.0d-10) then
            write (unit, *) "[PASS] Norm2"
        else
            write (unit, *) "[FAIL] Norm2", n2
        end if
        ninf = vector_norminf(v1)
        if (abs(ninf - 5.0d0) < 1.0d-10) then
            write (unit, *) "[PASS] NormInf"
        else
            write (unit, *) "[FAIL] NormInf", ninf
        end if

        ! ---------------------------------------------------------
        ! Dot Product
        ! ---------------------------------------------------------
        dot_val = vector_dot(v1, v2)
        if (abs(dot_val - 3.0d0) < 1.0d-10) then
            write (unit, *) "[PASS] Dot Product"
        else
            write (unit, *) "[FAIL] Dot Product"
        end if

        ! ---------------------------------------------------------
        ! Arithmetic
        ! ---------------------------------------------------------
        call add(v1, v2, res)
        vals1 = res%get_data()
        if (all(vals1 == [2.0d0, -1.0d0, 4.0d0, -3.0d0, 6.0d0])) then
            write (unit, *) "[PASS] Addition"
        else
            write (unit, *) "[FAIL] Addition"
        end if

        call subtract(v1, v2, res)
        vals1 = res%get_data()
        if (all(vals1 == [0.0d0, -3.0d0, 2.0d0, -5.0d0, 4.0d0])) then
            write (unit, *) "[PASS] Subtraction"
        else
            write (unit, *) "[FAIL] Subtraction"
        end if

        call multiply(v1, v2, res)
        vals1 = res%get_data()
        if (all(vals1 == [1.0d0, -2.0d0, 3.0d0, -4.0d0, 5.0d0])) then
            write (unit, *) "[PASS] Multiplication"
        else
            write (unit, *) "[FAIL] Multiplication"
        end if

        call divide(v1, v2, res)
        vals1 = res%get_data()
        if (all(vals1 == [1.0d0, -2.0d0, 3.0d0, -4.0d0, 5.0d0])) then
            write (unit, *) "[PASS] Division"
        else
            write (unit, *) "[FAIL] Division"
        end if

        ! ---------------------------------------------------------
        ! Scalar Ops
        ! ---------------------------------------------------------
        call vector_scale(2.0d0, v2)
        vals2 = v2%get_data()
        if (all(vals2 == [2.0d0, 2.0d0, 2.0d0, 2.0d0, 2.0d0])) then
            write (unit, *) "[PASS] Scale"
        else
            write (unit, *) "[FAIL] Scale"
        end if

        call vector_axpy(2.0d0, v1, v2)
        if (all(v2%get_data() == [4.0d0, -2.0d0, 8.0d0, -6.0d0, 12.0d0])) then
            write (unit, *) "[PASS] AXPY"
        else
            write (unit, *) "[FAIL] AXPY"
        end if

        call vector_axpyz(0.5d0, v1, v2, res)
        vals1 = res%get_data()
        if (all(vals1 == [4.5d0, -3.0d0, 9.5d0, -8.0d0, 14.5d0])) then
            write (unit, *) "[PASS] AXPYZ"
        else
            write (unit, *) "[FAIL] AXPYZ"
            write (unit, *) vals1
        end if

        ! ---------------------------------------------------------
        ! Utilities
        ! ---------------------------------------------------------
        call v1%set(OP_INS, -10.0d0)
        call vector_abs(v1)
        vals1 = v1%get_data()
        if (all(vals1 == 10.0d0)) then
            write (unit, *) "[PASS] Absolute Value"
        else
            write (unit, *) "[FAIL] Absolute Value"
        end if

        call v1%set(OP_INS, 4.0d0)
        call vector_reciprocal(v1)
        vals1 = v1%get_data()
        if (all(abs(vals1 - 0.25d0) < 1.0d-10)) then
            write (unit, *) "[PASS] Reciprocal"
        else
            write (unit, *) "[FAIL] Reciprocal"
        end if

        call v1%set(OP_INS, 10.0d0)
        call vector_shift(1, v1)
        vals1 = v1%get_data()
        if (all(vals1 == 9.0d0)) then
            write (unit, *) "[PASS] Shift"
        else
            write (unit, *) "[FAIL] Shift"
        end if
        ! ---------------------------------------------------------
        ! Assignment Operator
        ! ---------------------------------------------------------
        res = v1
        vals1 = res%get_data()
        if (all(vals1 == 9.0d0)) then
            write (unit, *) "[PASS] Assignment"
        else
            write (unit, *) "[FAIL] Assignment"
        end if

        call v1%destroy()
        call v2%destroy()
        call res%destroy()

    end subroutine run_test_vector_ops_dp

    !>
    !> Master subroutine for matrix arithmetic operations
    !> Tests: scale, axpy, xpay, axpyz
    !>
    subroutine run_test_matrix_operations()
        write (unit, *) "========================================"
        write (unit, *) "   Matrix operations Tests"
        write (unit, *) "========================================"

        call test_matrix_dense_operations()
        write (unit, *) ""
        call test_matrix_coo_operations()
        write (unit, *) ""
        call test_matrix_csr_operations()
        write (unit, *) ""
        call test_matrix_bsr_operations()
    end subroutine run_test_matrix_operations

    !>
    !> Dense Matrix Arithmetic
    !>
    subroutine test_matrix_dense_operations()
        implicit none
        class(abst_matrix), allocatable :: A, B, C
        integer(int32), parameter :: N = 3
        integer(int32) :: i, j
        integer(int32) :: ierr

        type(type_vector_dp) :: vb
        type(type_vector_dp) :: diag
        type(type_vector_dp) :: y

        real(real64), dimension(:, :), pointer :: A_data
        real(real64), dimension(:, :), pointer :: B_data
        real(real64), dimension(:, :), pointer :: C_data
        real(real64), dimension(:), pointer :: vb_data

        write (unit, *) "--- Testing DENSE Arithmetic ---"
        A = create_matrix(MATRIX_DENSE, N)
        do i = 1, N
            do j = 1, N
                call A%set(OP_INS, i, j, dble(i + j - 1)) ! A(i,j) = i + j -1
            end do
        end do
        B = create_matrix(MATRIX_DENSE, N)
        call B%set(OP_INS, 3.0d0) ! B = 3.0
        C = create_matrix(MATRIX_DENSE, N)
        call C%zero()

        ! 1. AXPY Test (B = alpha * A + B)
        ! A= [1 2 3; 2 3 4; 3 4 5], B=3.0
        call matrix_axpy(2.0d0, A, B, ierr)
        select type (A)
        type is (type_matrix_dense)
            select type (B)
            type is (type_matrix_dense)
                A_data => A%get_val()
                B_data => B%get_val()
                if (all(B_data == 2.0d0 * A_data + 3.0d0)) then
                    write (unit, *) "[PASS] Dense Matrix: AXPY"
                else
                    write (unit, *) "[FAIL] Dense Matrix: AXPY"
                end if
            end select
        end select

        ! 2. xpay Test (C = A + beta * C)
        ! A= [1 2 3; 2 3 4; 3 4 5], C=0.0
        call matrix_xpay(4.0d0, A, C, ierr)
        select type (A)
        type is (type_matrix_dense)
            select type (C)
            type is (type_matrix_dense)
                A_data => A%get_val()
                C_data => C%get_val()
                if (all(C_data == A_data + 4.0d0 * 0.0d0)) then
                    write (unit, *) "[PASS] Dense Matrix: XPAY"
                else
                    write (unit, *) "[FAIL] Dense Matrix: XPAY"
                end if
            end select
        end select

        ! 3. AXPYZ Test (C = alpha * A + B)
        ! A= [1 2 3; 2 3 4; 3 4 5], B=3.0, C from previous step
        call matrix_axpyz(3.0d0, A, B, C, ierr)
        select type (A)
        type is (type_matrix_dense)
            select type (B)
            type is (type_matrix_dense)
                select type (C)
                type is (type_matrix_dense)
                    A_data => A%get_val()
                    B_data => B%get_val()
                    C_data => C%get_val()
                    if (all(C_data == 3.0d0 * A_data + B_data)) then
                        write (unit, *) "[PASS] Dense Matrix: AXPYZ"
                    else
                        write (unit, *) "[FAIL] Dense Matrix: AXPYZ"
                    end if
                end select
            end select
        end select

        ! 4. Scale Test (A = alpha * A)
        ! A= [1 2 3; 2 3 4; 3 4 5]
        do i = 1, N
            do j = 1, N
                call A%set(OP_INS, i, j, dble(i + j - 1)) ! A(i,j) = i + j -1
            end do
        end do
        call vb%initialize(N)
        call vb%set(OP_INS, [0.5d0, 2.0d0, 1.0d0]) ! vb = [0.5, 2.0, 1.0]
        call matrix_scale(A, vb, diag, OP_SCALE_JACOBI, ierr)
        select type (A)
        type is (type_matrix_dense)
            A_data => A%get_val()
            vb_data => vb%get_data()
            block
                real(real64), parameter :: A_expected(3, 3) = transpose(reshape([ &
                                                                                1.0d0, 2.0d0, 3.0d0, &
                                                                                2.0d0 / 3.0d0, 1.0d0, 4.0d0 / 3.0d0, &
                                                                                3.0d0 / 5.0d0, 4.0d0 / 5.0d0, 1.0d0], [3, 3]))

                real(real64), parameter :: vb_expected(3) = [ &
                                           0.5d0, 2.0d0 / 3.0d0, 1.0d0 / 5.0d0]

                if (all(abs(A_data - A_expected) < 1.0d-10) .and. &
                    all(abs(vb_data - vb_expected) < 1.0d-10)) then
                    write (unit, *) "[PASS] Dense Matrix: Scale (Jacobi)"
                else
                    write (unit, *) "[FAIL] Dense Matrix: Scale (Jacobi)"
                end if

            end block
        end select
        do i = 1, N
            do j = 1, N
                call A%set(OP_INS, i, j, dble(i + j - 1)) ! A(i,j) = i + j -1
            end do
        end do
        call vb%set(OP_INS, [0.5d0, 2.0d0, 1.0d0]) ! vb = [0.5, 2.0, 1.0]
        call matrix_scale(A, vb, diag, OP_SCALE_SYMM_DIAG, ierr)
        select type (A)
        type is (type_matrix_dense)
            A_data => A%get_val()
            vb_data => vb%get_data()
            block
                real(real64) :: A_expected(3, 3)

                real(real64) :: vb_expected(3)
                real(real64) :: s3, s5, s15
                s3 = sqrt(3.0d0)
                s5 = sqrt(5.0d0)
                s15 = sqrt(15.0d0)
                vb_expected = [0.5d0, 2.0d0 / s3, 1.0d0 / s5]
                A_expected = transpose(reshape([ &
                                               1.0d0, 2.0d0 / s3, 3.0d0 / s5, &
                                               2.0d0 / s3, 1.0d0, 4.0d0 / s15, &
                                               3.0d0 / s5, 4.0d0 / s15, 1.0d0], [3, 3]))

                if (all(abs(A_data - A_expected) < 1.0d-10) .and. &
                    all(abs(vb_data - vb_expected) < 1.0d-10)) then
                    write (unit, *) "[PASS] Dense Matrix: Scale (Symmetric Diag)"
                else
                    write (unit, *) "[FAIL] Dense Matrix: Scale (Symmetric Diag)"
                end if

            end block

            !----------------------------------------
            ! 6. MatVec Test
            !----------------------------------------
            ! 行列Aを初期状態に戻す
            do i = 1, N
                do j = 1, N
                    call A%set(OP_INS, i, j, dble(i + j - 1))
                end do
            end do
            ! A = [1 2 3; 2 3 4; 3 4 5]

            call y%initialize(N)
            call y%zero()
            call vb%set(OP_INS, [0.5d0, 2.0d0, 1.0d0])

            ! 計算: y = A * vb
            ! y1 = 1*0.5 + 2*2.0 + 3*1.0 = 0.5 + 4.0 + 3.0 = 7.5
            ! y2 = 2*0.5 + 3*2.0 + 4*1.0 = 1.0 + 6.0 + 4.0 = 11.0
            ! y3 = 3*0.5 + 4*2.0 + 5*1.0 = 1.5 + 8.0 + 5.0 = 14.5

            call matvec(A, vb, y, ierr)

            block
                real(real64), dimension(N) :: val_expected = [7.5d0, 11.0d0, 14.5d0]

                if (all(abs(y%get_data() - val_expected) < 1.0d-10)) then
                    write (unit, *) "[PASS] Dense Matrix: MatVec"
                else
                    write (unit, *) "[FAIL] Dense Matrix: MatVec"
                end if
            end block
        end select

    end subroutine test_matrix_dense_operations

    subroutine test_matrix_coo_operations()
        implicit none

        class(abst_matrix), allocatable :: A, B, C
        integer(int32), parameter :: N = 5
        integer(int32) :: ierr, i

        type(type_vector_dp) :: vb, y
        type(type_vector_dp) :: diag

        real(real64), dimension(:), pointer :: vb_data
        real(real64), dimension(:), pointer :: C_data, B_data, A_data
        integer(int32), dimension(:), pointer :: row_idx, col_idx
        real(real64), dimension(:), pointer :: val

        integer(int32), parameter :: nnz = 13
        integer(int32), dimension(nnz) :: row_idx_init = [1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5]
        integer(int32), dimension(nnz) :: col_idx_init = [1, 2, 1, 2, 3, 2, 3, 4, 3, 4, 5, 4, 5]
        real(real64), dimension(nnz) :: val_init = [1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0, 6.0d0, 7.0d0, 8.0d0, 9.0d0, 10.0d0, 11.0d0, 12.0d0, 13.0d0]

        write (unit, *) "--- Testing COO Arithmetic ---"

        !----------------------------------------
        ! 1. COO行列作成
        !----------------------------------------
        A = create_matrix(MATRIX_COO, N, row_idx_init, col_idx_init)
        do i = 1, nnz
            call A%set(OP_INS, row_idx_init(i), col_idx_init(i), val_init(i))
        end do

        ! B = all 3.0
        B = create_matrix(MATRIX_COO, N, row_idx_init, col_idx_init)
        call B%set(OP_INS, 3.0d0)

        ! C = zeros
        C = create_matrix(MATRIX_COO, N, row_idx_init, col_idx_init)
        call C%zero()

        !----------------------------------------
        ! 2. AXPY Test (B = 2*A + B)
        !----------------------------------------
        call matrix_axpy(2.0d0, A, B, ierr)
        if (ierr == MATRIX_STATUS_ILL_OPERATIONS) then
            write (unit, *) "[PASS] COO Matrix: AXPY (Ill Operations Caught)"
        else
            write (unit, *) "[FAIL] COO Matrix: AXPY"
        end if

        !----------------------------------------
        ! 3. XPAY Test (C = A + 4*C)
        !----------------------------------------
        call matrix_xpay(4.0d0, A, C, ierr)
        if (ierr == MATRIX_STATUS_ILL_OPERATIONS) then
            write (unit, *) "[PASS] COO Matrix: XPAY (Ill Operations Caught)"
        else
            write (unit, *) "[FAIL] COO Matrix: XPAY"
        end if

        !----------------------------------------
        ! 4. AXPYZ Test (C = 3*A + B)
        !----------------------------------------
        call matrix_axpyz(3.0d0, A, B, C, ierr)
        if (ierr == MATRIX_STATUS_ILL_OPERATIONS) then
            write (unit, *) "[PASS] COO Matrix: AXPYZ (Ill Operations Caught)"
        else
            write (unit, *) "[FAIL] COO Matrix: AXPYZ"
        end if

        !----------------------------------------
        ! 5. Scale Test
        !----------------------------------------
        call vb%initialize(N)
        call vb%set(OP_INS, [0.5d0, 2.0d0, 1.0d0, 1.5d0, 0.2d0])
        call matrix_scale(A, vb, diag, OP_SCALE_JACOBI, ierr)

        select type (A)
        type is (type_matrix_coo)
            val => A%get_val()
            vb_data => vb%get_data()

            block
                real(real64), dimension(nnz), parameter :: val_expected = [ &
                                                           1.0d0, 2.0d0, 3.0d0 / 4.0d0, 1.0d0, 5.0d0 / 4.0d0, 6.0d0 / 7.0d0, 1.0d0, &
                                                           8.0d0 / 7.0d0, 9.0d-1, 1.0d0, 11.0d-1, 12.0d0 / 13.0d0, 1.0d0]

                real(real64), dimension(N), parameter :: vb_expected = [0.5d0, 0.5d0, 1.0d0 / 7.0d0, 1.5d-1, 0.2d0 / 13.0d0]

                if (all(abs(val - val_expected) < 1.0d-12) .and. &
                    all(abs(vb_data - vb_expected) < 1.0d-12)) then
                    write (unit, *) "[PASS] COO Matrix: Scale (Jacobi)"
                else
                    write (unit, *) "[FAIL] COO Matrix: Scale (Jacobi)"
                end if

            end block
        end select

        call vb%set(OP_INS, [0.5d0, 2.0d0, 1.0d0, 1.5d0, 0.2d0])
        do i = 1, nnz
            call A%set(OP_INS, row_idx_init(i), col_idx_init(i), val_init(i))
        end do
        call matrix_scale(A, vb, diag, OP_SCALE_SYMM_DIAG, ierr)
        select type (A)
        type is (type_matrix_coo)
            val => A%get_val()
            vb_data => vb%get_data()

            block
                real(real64), dimension(nnz) :: val_expected
                real(real64), dimension(N) :: vb_expected
                real(real64) :: s7, s10, s13, s70, s130

                s7 = sqrt(7.0d0)
                s10 = sqrt(10.0d0)
                s13 = sqrt(13.0d0)
                s70 = sqrt(70.0d0) ! sqrt(7)*sqrt(10)
                s130 = sqrt(130.0d0) ! sqrt(10)*sqrt(13)

                ! --- 行列 A の期待値 ---
                ! 1. (1,1) 1.0 * 1.0 * 1.0
                val_expected(1) = 1.0d0
                ! 2. (1,2) 2.0 * 1.0 * 0.5
                val_expected(2) = 1.0d0

                ! 3. (2,1) 3.0 * 0.5 * 1.0
                val_expected(3) = 1.5d0
                ! 4. (2,2) 4.0 * 0.5 * 0.5
                val_expected(4) = 1.0d0
                ! 5. (2,3) 5.0 * 0.5 * (1/s7)
                val_expected(5) = 2.5d0 / s7

                ! 6. (3,2) 6.0 * (1/s7) * 0.5
                val_expected(6) = 3.0d0 / s7
                ! 7. (3,3) 7.0 * (1/s7) * (1/s7)
                val_expected(7) = 1.0d0
                ! 8. (3,4) 8.0 * (1/s7) * (1/s10)
                val_expected(8) = 8.0d0 / s70

                ! 9. (4,3) 9.0 * (1/s10) * (1/s7)
                val_expected(9) = 9.0d0 / s70
                ! 10. (4,4) 10.0 * (1/s10) * (1/s10)
                val_expected(10) = 1.0d0
                ! 11. (4,5) 11.0 * (1/s10) * (1/s13)
                val_expected(11) = 11.0d0 / s130

                ! 12. (5,4) 12.0 * (1/s13) * (1/s10)
                val_expected(12) = 12.0d0 / s130
                ! 13. (5,5) 13.0 * (1/s13) * (1/s13)
                val_expected(13) = 1.0d0

                ! --- ベクトル b の期待値 ---
                ! init: [0.5, 2.0, 1.0, 1.5, 0.2]
                ! calc: b[i] * S[i]
                vb_expected(1) = 0.5d0 * 1.0d0 ! 0.5
                vb_expected(2) = 2.0d0 * 0.5d0 ! 1.0
                vb_expected(3) = 1.0d0 / s7 ! 1.0 / sqrt(7)
                vb_expected(4) = 1.5d0 / s10 ! 1.5 / sqrt(10)
                vb_expected(5) = 0.2d0 / s13 ! 0.2 / sqrt(13)

                if (all(abs(val - val_expected) < 1.0d-12) .and. &
                    all(abs(vb_data - vb_expected) < 1.0d-12)) then
                    write (unit, *) "[PASS] COO Matrix: Scale (Symmetric Diag)"
                else
                    write (unit, *) "[FAIL] COO Matrix: Scale (Symmetric Diag)"
                end if

            end block
        end select

        call y%initialize(N)
        call y%zero()
        call vb%set(OP_INS, [0.5d0, 2.0d0, 1.0d0, 1.5d0, 0.2d0])
        do i = 1, nnz
            call A%set(OP_INS, row_idx_init(i), col_idx_init(i), val_init(i))
        end do
        call matvec(A, vb, y, ierr)
        block
            real(real64) :: val_expected(N) = [4.5d0, &
                                               14.5d0, &
                                               31.0d0, &
                                               26.2d0, &
                                               20.6d0]
            if (all(abs(y%get_data() - val_expected) < 1.0d-10)) then
                write (unit, *) "[PASS] COO Matrix: MatVec"
            else
                write (unit, *) "[FAIL] COO Matrix: MatVec"
            end if
        end block

    end subroutine test_matrix_coo_operations

    subroutine test_matrix_csr_operations()
        implicit none
        class(abst_matrix), allocatable :: A, B, C
        integer(int32), parameter :: N = 5
        integer(int32) :: ierr, i, j

        type(type_vector_dp) :: vb, y
        type(type_vector_dp) :: diag

        real(real64), dimension(:), pointer :: vb_data
        real(real64), dimension(:), pointer :: C_data, B_data, A_data
        integer(int32), dimension(:), pointer :: row_idx, col_idx
        real(real64), dimension(:), pointer :: val

        integer(int32), parameter :: nnz = 13
        integer(int32), dimension(N + 1) :: row_idx_init = [1, 3, 6, 9, 12, 14]
        integer(int32), dimension(nnz) :: col_idx_init = [1, 2, 1, 2, 3, 2, 3, 4, 3, 4, 5, 4, 5]
        real(real64), dimension(nnz) :: val_init = [1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0, 6.0d0, 7.0d0, 8.0d0, 9.0d0, 10.0d0, 11.0d0, 12.0d0, 13.0d0]

        write (unit, *) "--- Testing CSR Arithmetic ---"

        !----------------------------------------
        ! 1. CSR行列作成
        !----------------------------------------
        A = create_matrix(MATRIX_CSR, N, row_idx_init, col_idx_init)
        do i = 1, N
            do j = row_idx_init(i), row_idx_init(i + 1) - 1
                call A%set(OP_INS, i, col_idx_init(j), val_init(j))
            end do
        end do

        ! B = all 3.0
        B = create_matrix(MATRIX_CSR, N, row_idx_init, col_idx_init)
        call B%set(OP_INS, 3.0d0)

        ! C = zeros
        C = create_matrix(MATRIX_CSR, N, row_idx_init, col_idx_init)
        call C%zero()

        !----------------------------------------
        ! 2. AXPY Test (B = 2*A + B)
        !----------------------------------------
        call matrix_axpy(2.0d0, A, B, ierr)
        if (ierr == MATRIX_STATUS_ILL_OPERATIONS) then
            write (unit, *) "[PASS] CSR Matrix: AXPY (Ill Operations Caught)"
        else
            write (unit, *) "[FAIL] CSR Matrix: AXPY"
        end if

        !----------------------------------------
        ! 3. XPAY Test (C = A + 4*C)
        !----------------------------------------
        call matrix_xpay(4.0d0, A, C, ierr)
        if (ierr == MATRIX_STATUS_ILL_OPERATIONS) then
            write (unit, *) "[PASS] CSR Matrix: XPAY (Ill Operations Caught)"
        else
            write (unit, *) "[FAIL] CSR Matrix: XPAY"
        end if

        !----------------------------------------
        ! 4. AXPYZ Test (C = 3*A + B)
        !----------------------------------------
        call matrix_axpyz(3.0d0, A, B, C, ierr)
        if (ierr == MATRIX_STATUS_ILL_OPERATIONS) then
            write (unit, *) "[PASS] CSR Matrix: AXPYZ (Ill Operations Caught)"
        else
            write (unit, *) "[FAIL] CSR Matrix: AXPYZ"
        end if

        !----------------------------------------
        ! 5. Scale Test
        !----------------------------------------
        call vb%initialize(N)
        call vb%set(OP_INS, [0.5d0, 2.0d0, 1.0d0, 1.5d0, 0.2d0])
        call matrix_scale(A, vb, diag, OP_SCALE_JACOBI, ierr)

        select type (A)
        type is (type_matrix_csr)
            val => A%get_val()
            vb_data => vb%get_data()

            block
                real(real64), dimension(nnz), parameter :: val_expected = [ &
                                                           1.0d0, 2.0d0, 3.0d0 / 4.0d0, 1.0d0, 5.0d0 / 4.0d0, 6.0d0 / 7.0d0, 1.0d0, &
                                                           8.0d0 / 7.0d0, 9.0d-1, 1.0d0, 11.0d-1, 12.0d0 / 13.0d0, 1.0d0]

                real(real64), dimension(N), parameter :: vb_expected = [0.5d0, 0.5d0, 1.0d0 / 7.0d0, 1.5d-1, 0.2d0 / 13.0d0]

                if (all(abs(val - val_expected) < 1.0d-12) .and. &
                    all(abs(vb_data - vb_expected) < 1.0d-12)) then
                    write (unit, *) "[PASS] CSR Matrix: Scale (Jacobi)"
                else
                    write (unit, *) "[FAIL] CSR Matrix: Scale (Jacobi)"
                end if

            end block
        end select

        call vb%set(OP_INS, [0.5d0, 2.0d0, 1.0d0, 1.5d0, 0.2d0])
        do i = 1, N
            do j = row_idx_init(i), row_idx_init(i + 1) - 1
                call A%set(OP_INS, i, col_idx_init(j), val_init(j))
            end do
        end do
        call matrix_scale(A, vb, diag, OP_SCALE_SYMM_DIAG, ierr)
        select type (A)
        type is (type_matrix_csr)
            val => A%get_val()
            vb_data => vb%get_data()

            block
                real(real64), dimension(nnz) :: val_expected
                real(real64), dimension(N) :: vb_expected
                real(real64) :: s7, s10, s13, s70, s130

                s7 = sqrt(7.0d0)
                s10 = sqrt(10.0d0)
                s13 = sqrt(13.0d0)
                s70 = sqrt(70.0d0) ! sqrt(7)*sqrt(10)
                s130 = sqrt(130.0d0) ! sqrt(10)*sqrt(13)

                ! --- 行列 A の期待値 ---
                ! 1. (1,1) 1.0 * 1.0 * 1.0
                val_expected(1) = 1.0d0
                ! 2. (1,2) 2.0 * 1.0 * 0.5
                val_expected(2) = 1.0d0

                ! 3. (2,1) 3.0 * 0.5 * 1.0
                val_expected(3) = 1.5d0
                ! 4. (2,2) 4.0 * 0.5 * 0.5
                val_expected(4) = 1.0d0
                ! 5. (2,3) 5.0 * 0.5 * (1/s7)
                val_expected(5) = 2.5d0 / s7

                ! 6. (3,2) 6.0 * (1/s7) * 0.5
                val_expected(6) = 3.0d0 / s7
                ! 7. (3,3) 7.0 * (1/s7) * (1/s7)
                val_expected(7) = 1.0d0
                ! 8. (3,4) 8.0 * (1/s7) * (1/s10)
                val_expected(8) = 8.0d0 / s70

                ! 9. (4,3) 9.0 * (1/s10) * (1/s7)
                val_expected(9) = 9.0d0 / s70
                ! 10. (4,4) 10.0 * (1/s10) * (1/s10)
                val_expected(10) = 1.0d0
                ! 11. (4,5) 11.0 * (1/s10) * (1/s13)
                val_expected(11) = 11.0d0 / s130

                ! 12. (5,4) 12.0 * (1/s13) * (1/s10)
                val_expected(12) = 12.0d0 / s130
                ! 13. (5,5) 13.0 * (1/s13) * (1/s13)
                val_expected(13) = 1.0d0

                ! --- ベクトル b の期待値 ---
                ! init: [0.5, 2.0, 1.0, 1.5, 0.2]
                ! calc: b[i] * S[i]
                vb_expected(1) = 0.5d0 * 1.0d0 ! 0.5
                vb_expected(2) = 2.0d0 * 0.5d0 ! 1.0
                vb_expected(3) = 1.0d0 / s7 ! 1.0 / sqrt(7)
                vb_expected(4) = 1.5d0 / s10 ! 1.5 / sqrt(10)
                vb_expected(5) = 0.2d0 / s13 ! 0.2 / sqrt(13)

                if (all(abs(val - val_expected) < 1.0d-12) .and. &
                    all(abs(vb_data - vb_expected) < 1.0d-12)) then
                    write (unit, *) "[PASS] CSR Matrix: Scale (Symmetric Diag)"
                else
                    write (unit, *) "[FAIL] CSR Matrix: Scale (Symmetric Diag)"
                end if

            end block
        end select

        call y%initialize(N)
        call y%zero()
        call vb%set(OP_INS, [0.5d0, 2.0d0, 1.0d0, 1.5d0, 0.2d0])
        do i = 1, N
            do j = row_idx_init(i), row_idx_init(i + 1) - 1
                call A%set(OP_INS, i, col_idx_init(j), val_init(j))
            end do
        end do
        call matvec(A, vb, y, ierr)
        block
            real(real64) :: val_expected(N) = [4.5d0, &
                                               14.5d0, &
                                               31.0d0, &
                                               26.2d0, &
                                               20.6d0]
            if (all(abs(y%get_data() - val_expected) < 1.0d-10)) then
                write (unit, *) "[PASS] CSR Matrix: MatVec"
            else
                write (unit, *) "[FAIL] CSR Matrix: MatVec"
            end if
        end block

    end subroutine test_matrix_csr_operations

    !>
    !> BSR Matrix Arithmetic (Block Size = 3)
    !>
    subroutine test_matrix_bsr_operations()
        implicit none

        class(abst_matrix), allocatable :: A, B, C
        integer(int32), parameter :: nb = 5 ! ブロック数 (Nodes)
        integer(int32), parameter :: bn = 3 ! ブロックサイズ (Block Size)
        integer(int32), parameter :: N = nb * bn ! 全行列サイズ (15)
        integer(int32) :: ierr, k
        integer(int32) :: bi, bj, col_blk
        real(real64) :: diff

        type(type_vector_dp) :: vb, y
        type(type_vector_dp) :: diag

        real(real64), dimension(:, :, :), pointer :: A_data
        real(real64), dimension(:), pointer :: vb_data, y_data

        ! スパースパターン (Block CSR structure)
        integer(int32), dimension(nb + 1) :: row_ptr_init = [1, 3, 6, 9, 12, 14]
        integer(int32), dimension(13) :: col_idx_init = [1, 2, 1, 2, 3, 2, 3, 4, 3, 4, 5, 4, 5]

        write (unit, '(A)') "--- Testing BSR Arithmetic (Block=3) ---"

        !----------------------------------------
        ! 1. BSR行列作成
        !----------------------------------------
        A = create_matrix(MATRIX_BSR, nb, row_ptr_init, col_idx_init, bn)
        B = create_matrix(MATRIX_BSR, nb, row_ptr_init, col_idx_init, bn)
        C = create_matrix(MATRIX_BSR, nb, row_ptr_init, col_idx_init, bn)

        ! 初期値セット (AXPY, XPAYテスト用)
        ! 複雑な値を入れておく (位置によって値が変わる)
        call A%zero()
        do k = 1, nb
            do ierr = row_ptr_init(k), row_ptr_init(k + 1) - 1
                col_blk = col_idx_init(ierr)
                do bi = 1, bn
                    do bj = 1, bn
                        call A%set(OP_INS, k, col_blk, bi, bj, dble(k + col_blk))
                    end do
                end do
            end do
        end do
        call B%set(OP_INS, 3.0d0)
        call C%zero()

        !----------------------------------------
        ! 2. AXPY Test (B = 2*A + B)
        !----------------------------------------
        call matrix_axpy(2.0d0, A, B, ierr)
        if (ierr == MATRIX_STATUS_ILL_OPERATIONS) then
            write (unit, '(A)') "[PASS] BSR Matrix: AXPY (Ill Operations Caught)"
        else
            write (unit, '(A)') "[FAIL] BSR Matrix: AXPY (Result code not checked here)"
        end if

        !----------------------------------------
        ! 3. XPAY Test (C = A + 4*C)
        !----------------------------------------
        call matrix_xpay(4.0d0, A, C, ierr)
        if (ierr == MATRIX_STATUS_ILL_OPERATIONS) then
            write (unit, '(A)') "[PASS] BSR Matrix: XPAY (Ill Operations Caught)"
        else
            write (unit, '(A)') "[FAIL] BSR Matrix: XPAY"
        end if
        !----------------------------------------
        ! 4. AXPYZ Test (C = 3*A + B)
        !----------------------------------------
        call matrix_axpyz(3.0d0, A, B, C, ierr)
        if (ierr == MATRIX_STATUS_ILL_OPERATIONS) then
            write (unit, '(A)') "[PASS] BSR Matrix: AXPYZ (Ill Operations Caught)"
        else
            write (unit, '(A)') "[FAIL] BSR Matrix: AXPYZ"
        end if

        !----------------------------------------
        ! 5. Scale Test
        !----------------------------------------
        call vb%initialize(N)

        ! >>>>>>>>>> Jacobi Scaling Check <<<<<<<<<<
        ! 検証ロジック:
        !   Matrix A = All 2.0
        !   Vector v = All 0.5
        !   Result   = 2.0 * 0.5 = 1.0 (Expected)

        ! 値を定数(2.0)にリセット
        select type (A)
        type is (type_matrix_bsr)
            call A%set_all(OP_INS, 2.0d0)
        end select

        call vb%set(OP_INS, 0.5d0)

        ! 計算実行: A <- D^{-1} A (Scaling)
        call matrix_scale(A, vb, diag, OP_SCALE_JACOBI, ierr)

        select type (A)
        type is (type_matrix_bsr)
            A_data => A%get_val()

            diff = maxval(abs(A_data - 1.0d0))
            if (diff < 1.0d-10) then
                write (unit, '(A)') "[PASS] BSR Matrix: Scale (Jacobi)"
            else
                write (unit, '(A, E12.4)') "[FAIL] BSR Matrix: Scale (Jacobi) - Max Diff: ", diff
            end if
        end select

        ! >>>>>>>>>> Symmetric Scaling Check <<<<<<<<<<
        ! 検証ロジック:
        !   Matrix A = All 4.0
        !   Vector v = All 0.5
        !   Result   = 0.5 * 4.0 * 0.5 = 1.0 (Expected)

        ! 値を定数(4.0)にリセット
        select type (A)
        type is (type_matrix_bsr)
            call A%set_all(OP_INS, 4.0d0)
        end select

        call vb%set(OP_INS, 0.5d0)

        ! 計算実行: A <- D^{-1/2} A D^{-1/2}
        call matrix_scale(A, vb, diag, OP_SCALE_SYMM_DIAG, ierr)

        select type (A)
        type is (type_matrix_bsr)
            A_data => A%get_val()

            diff = maxval(abs(A_data - 1.0d0))
            if (diff < 1.0d-10) then
                write (unit, '(A)') "[PASS] BSR Matrix: Scale (Symmetric Diag)"
            else
                write (unit, '(A, E12.4)') "[FAIL] BSR Matrix: Scale (Symmetric Diag) - Max Diff: ", diff
            end if
        end select

        !----------------------------------------
        ! 5. MatVec Test (変更なし)
        !----------------------------------------
        select type (A)
        type is (type_matrix_bsr)
            call A%set_all(OP_INS, 1.0d0)
        end select

        call y%initialize(N)
        call y%zero()
        call vb%set(OP_INS, 1.0d0)

        call matvec(A, vb, y, ierr)

        block
            real(real64), dimension(N) :: y_expected
            real(real64), dimension(:), pointer :: y_actual
            integer(int32) :: r, blk_row_idx, num_blocks_in_row

            y_expected = 0.0d0
            do r = 1, N
                blk_row_idx = (r - 1) / bn + 1
                num_blocks_in_row = row_ptr_init(blk_row_idx + 1) - row_ptr_init(blk_row_idx)
                y_expected(r) = dble(num_blocks_in_row) * 3.0d0 ! bn(3) * 1.0 * 1.0
            end do

            y_actual => y%get_data()
            diff = maxval(abs(y_actual - y_expected))
            if (diff < 1.0d-10) then
                write (unit, '(A)') "[PASS] BSR Matrix: MatVec"
            else
                write (unit, '(A, E12.4)') "[FAIL] BSR Matrix: MatVec - Max Diff: ", diff
            end if
        end block

    end subroutine test_matrix_bsr_operations
end program test_linalg
