program test_linalg
    use, intrinsic :: iso_fortran_env, only: int32, real64
#ifdef _MPI
    use :: mpi_f08
#endif
    use :: module_core
    use :: module_linalg ! Imports vector types and ops
    implicit none
#ifdef _MPI

    integer(int32) :: ierr
    call MPI_Init(ierr)
#endif
    print *, "========================================"
    print *, "   Linear Algebra Test Suite"
    print *, "========================================"
    print *

    ! Initialize the linear algebra backend (MKL or Native)
    call initialize_linalg()

    call run_test_vector_basic_dp()
    print *
    call run_test_vector_ops_dp()
    print *
    call run_test_matrix_arithmetic()

    print *
    print *, "========================================"
    print *, "   Linalg tests completed."
    print *, "========================================"

#ifdef _MPI
    call MPI_Finalize(ierr)
#endif

contains

    !>
    !> Basic functionality tests for type_vector_dp
    !> Creation, Setting, Getting, Copying
    !>
    subroutine run_test_vector_basic_dp()
        implicit none
        type(type_vector_dp) :: v, v_copy
        integer(int32), parameter :: N = 5
        real(real64), allocatable :: vals(:)
        integer(int32) :: i

        print *, "--- Testing Basic Vector Functions (DP) ---"

        !-------------------------------------------------------
        ! 1. Initialization
        !-------------------------------------------------------
        call v%initialize(N)
        if (v%get_size() == N) then
            print *, "[PASS] Initialization (Size=", N, ")"
        else
            print *, "[FAIL] Initialization size mismatch."
        end if

        !-------------------------------------------------------
        ! 2. Set Scalar
        !-------------------------------------------------------
        call v%set(OP_INS, 1.0d0)
        if (all(v%get_data() == 1.0d0)) then
            print *, "[PASS] Set Scalar (1.0)"
        else
            print *, "[FAIL] Set Scalar mismatch."
        end if

        !-------------------------------------------------------
        ! 3. Set Array
        !-------------------------------------------------------
        allocate (vals(N))
        vals = [(dble(i), i=1, N)] ! [1.0, 2.0, ..., 5.0]

        call v%set(OP_INS, vals)

        if (all(v%get_data() == vals)) then
            print *, "[PASS] Set Array [1..5]"
        else
            print *, "[FAIL] Set Array mismatch."
            call v%display()
        end if

        !-------------------------------------------------------
        ! 4. Set at Index & Scatter
        !-------------------------------------------------------
        call v%set(OP_INS, 1, 10.0d0) ! v[1] = 10
        call v%set(OP_INS, [2, 4], [-2.0d0, -4.0d0]) ! v[2] = -2, v[4] = -4

        if (all(v%get_data() == [10.0d0, -2.0d0, 3.0d0, -4.0d0, 5.0d0])) then
            print *, "[PASS] Set at Index & Scatter"
        else
            print *, "[FAIL] Set at Index & Scatter mismatch."
            call v%display()
        end if

        !-------------------------------------------------------
        ! 5. Copy
        !-------------------------------------------------------
        call v_copy%initialize(N)
        call v_copy%copy(v)

        if (all(v_copy%get_data() == v%get_data())) then
            print *, "[PASS] Copy Vector"
        else
            print *, "[FAIL] Copy Vector"
        end if

        !-------------------------------------------------------
        ! 6. Zero
        !-------------------------------------------------------
        call v_copy%zero()
        if (all(v_copy%get_data() == 0.0d0)) then
            print *, "[PASS] Zero Vector"
        else
            print *, "[FAIL] Zero Vector"
        end if

        call v%destroy()
        call v_copy%destroy()
    end subroutine run_test_vector_basic_dp

    !>
    !> Operation tests for type_vector_dp
    !> Arithmetic, Norms, Dot Product
    !>
    subroutine run_test_vector_ops_dp()
        type(type_vector_dp) :: v1, v2, res
        integer(int32), parameter :: N = 5
        real(real64) :: n1, n2, ninf, dot_val

        real(real64), dimension(:), allocatable :: vals1, vals2

        print *, "--- Testing Vector Operations (DP) ---"

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
            print *, "[PASS] Norm1"
        else
            print *, "[FAIL] Norm1", n1
        end if
        n2 = vector_norm2(v1)
        if (abs(n2 - 7.416198487095663d0) < 1.0d-10) then
            print *, "[PASS] Norm2"
        else
            print *, "[FAIL] Norm2", n2
        end if
        ninf = vector_norminf(v1)
        if (abs(ninf - 5.0d0) < 1.0d-10) then
            print *, "[PASS] NormInf"
        else
            print *, "[FAIL] NormInf", ninf
        end if

        ! ---------------------------------------------------------
        ! Dot Product
        ! ---------------------------------------------------------
        dot_val = vector_dot(v1, v2)
        if (abs(dot_val - 3.0d0) < 1.0d-10) then
            print *, "[PASS] Dot Product"
        else
            print *, "[FAIL] Dot Product"
        end if

        ! ---------------------------------------------------------
        ! Arithmetic
        ! ---------------------------------------------------------
        call add(v1, v2, res)
        vals1 = res%get_data()
        if (all(vals1 == [2.0d0, -1.0d0, 4.0d0, -3.0d0, 6.0d0])) then
            print *, "[PASS] Addition"
        else
            print *, "[FAIL] Addition"
        end if

        call subtract(v1, v2, res)
        vals1 = res%get_data()
        if (all(vals1 == [0.0d0, -3.0d0, 2.0d0, -5.0d0, 4.0d0])) then
            print *, "[PASS] Subtraction"
        else
            print *, "[FAIL] Subtraction"
        end if

        call multiply(v1, v2, res)
        vals1 = res%get_data()
        if (all(vals1 == [1.0d0, -2.0d0, 3.0d0, -4.0d0, 5.0d0])) then
            print *, "[PASS] Multiplication"
        else
            print *, "[FAIL] Multiplication"
        end if

        call divide(v1, v2, res)
        vals1 = res%get_data()
        if (all(vals1 == [1.0d0, -2.0d0, 3.0d0, -4.0d0, 5.0d0])) then
            print *, "[PASS] Division"
        else
            print *, "[FAIL] Division"
        end if

        ! ---------------------------------------------------------
        ! Scalar Ops
        ! ---------------------------------------------------------
        call vector_scale(2.0d0, v2)
        vals2 = v2%get_data()
        if (all(vals2 == [2.0d0, 2.0d0, 2.0d0, 2.0d0, 2.0d0])) then
            print *, "[PASS] Scale"
        else
            print *, "[FAIL] Scale"
        end if

        call vector_axpy(2.0d0, v1, v2)
        if (all(v2%get_data() == [4.0d0, -2.0d0, 8.0d0, -6.0d0, 12.0d0])) then
            print *, "[PASS] AXPY"
        else
            print *, "[FAIL] AXPY"
        end if

        call vector_axpyz(0.5d0, v1, v2, res)
        vals1 = res%get_data()
        if (all(vals1 == [4.5d0, -3.0d0, 9.5d0, -8.0d0, 14.5d0])) then
            print *, "[PASS] AXPYZ"
        else
            print *, "[FAIL] AXPYZ"
            print *, vals1
        end if

        ! ---------------------------------------------------------
        ! Utilities
        ! ---------------------------------------------------------
        call v1%set(OP_INS, -10.0d0)
        call vector_abs(v1)
        vals1 = v1%get_data()
        if (all(vals1 == 10.0d0)) then
            print *, "[PASS] Absolute Value"
        else
            print *, "[FAIL] Absolute Value"
        end if

        call v1%set(OP_INS, 4.0d0)
        call vector_reciprocal(v1)
        vals1 = v1%get_data()
        if (all(abs(vals1 - 0.25d0) < 1.0d-10)) then
            print *, "[PASS] Reciprocal"
        else
            print *, "[FAIL] Reciprocal"
        end if

        call v1%set(OP_INS, 10.0d0)
        call vector_shift(1, v1)
        vals1 = v1%get_data()
        if (all(vals1 == 9.0d0)) then
            print *, "[PASS] Shift"
        else
            print *, "[FAIL] Shift"
        end if
        ! ---------------------------------------------------------
        ! Assignment Operator
        ! ---------------------------------------------------------
        res = v1
        vals1 = res%get_data()
        if (all(vals1 == 9.0d0)) then
            print *, "[PASS] Assignment"
        else
            print *, "[FAIL] Assignment"
        end if

        call v1%destroy()
        call v2%destroy()
        call res%destroy()

    end subroutine run_test_vector_ops_dp

    !>
    !> Master subroutine for matrix arithmetic operations
    !> Tests: scale, axpy, xpay, axpyz
    !>
    subroutine run_test_matrix_arithmetic()
        print *, "========================================"
        print *, "   Matrix Arithmetic Tests"
        print *, "   (Scale, AXPY, XPAY, AXPYZ)"
        print *, "========================================"

        call test_dense_arithmetic()
        print *
        call test_coo_arithmetic()
        print *
        ! call test_csr_arithmetic()
        print *
        ! call test_bsr_arithmetic()
    end subroutine run_test_matrix_arithmetic

    !>
    !> Dense Matrix Arithmetic
    !>
    subroutine test_dense_arithmetic()
        class(abst_matrix), allocatable :: A, B, C
        integer(int32), parameter :: N = 3
        integer(int32) :: i, j
        integer(int32) :: ierr

        type(type_vector_dp) :: vb
        type(type_vector_dp) :: diag

        real(real64), dimension(:, :), pointer :: A_data
        real(real64), dimension(:, :), pointer :: B_data
        real(real64), dimension(:, :), pointer :: C_data
        real(real64), dimension(:), pointer :: vb_data

        print *, "--- Testing DENSE Arithmetic ---"
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
        type is (type_dense)
            select type (B)
            type is (type_dense)
                A_data => A%get_val()
                B_data => B%get_val()
                if (all(B_data == 2.0d0 * A_data + 3.0d0)) then
                    print *, "[PASS] Dense Matrix: AXPY"
                else
                    print *, "[FAIL] Dense Matrix: AXPY"
                end if
            end select
        end select

        ! 2. xpay Test (C = A + beta * C)
        ! A= [1 2 3; 2 3 4; 3 4 5], C=0.0
        call matrix_xpay(4.0d0, A, C, ierr)
        select type (A)
        type is (type_dense)
            select type (C)
            type is (type_dense)
                A_data => A%get_val()
                C_data => C%get_val()
                if (all(C_data == A_data + 4.0d0 * 0.0d0)) then
                    print *, "[PASS] Dense Matrix: XPAY"
                else
                    print *, "[FAIL] Dense Matrix: XPAY"
                end if
            end select
        end select

        ! 3. AXPYZ Test (C = alpha * A + B)
        ! A= [1 2 3; 2 3 4; 3 4 5], B=3.0, C from previous step
        call matrix_axpyz(3.0d0, A, B, C, ierr)
        select type (A)
        type is (type_dense)
            select type (B)
            type is (type_dense)
                select type (C)
                type is (type_dense)
                    A_data => A%get_val()
                    B_data => B%get_val()
                    C_data => C%get_val()
                    if (all(C_data == 3.0d0 * A_data + B_data)) then
                        print *, "[PASS] Dense Matrix: AXPYZ"
                    else
                        print *, "[FAIL] Dense Matrix: AXPYZ"
                    end if
                end select
            end select
        end select

        ! 4. Scale Test (A = alpha * A)
        ! A= [1 2 3; 2 3 4; 3 4 5]

        call vb%initialize(N)
        call vb%set(OP_INS, [0.5d0, 2.0d0, 1.0d0]) ! vb = [0.5, 2.0, 1.0]
        call matrix_scale(A, vb, diag, OP_SCALE_JACOBI, ierr)
        select type (A)
        type is (type_dense)
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
                    print *, "[PASS] Dense Matrix: Scale"
                else
                    print *, "[FAIL] Dense Matrix: Scale"
                end if

            end block
        end select
    end subroutine test_dense_arithmetic

    subroutine test_coo_arithmetic()
        use iso_fortran_env, only: int32, real64
        implicit none

        class(abst_matrix), allocatable :: A, B, C
        integer(int32), parameter :: N = 5
        integer(int32) :: ierr, i

        type(type_vector_dp) :: vb
        type(type_vector_dp) :: diag

        real(real64), dimension(:), pointer :: vb_data
        real(real64), dimension(:), pointer :: C_data, B_data, A_data
        integer(int32), dimension(:), pointer :: row_idx, col_idx
        real(real64), dimension(:), pointer :: val

        integer(int32), parameter :: nnz = 13
        integer(int32), dimension(nnz) :: row_idx_init = [1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5]
        integer(int32), dimension(nnz) :: col_idx_init = [1, 2, 1, 2, 3, 2, 3, 4, 3, 4, 5, 4, 5]
        real(real64), dimension(nnz) :: val_init = [1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0, 6.0d0, 7.0d0, 8.0d0, 9.0d0, 10.0d0, 11.0d0, 12.0d0, 13.0d0]

        print *, "--- Testing COO Arithmetic ---"

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
            print *, "[PASS] COO Matrix: AXPY (Ill Operations Caught)"
        else
            print *, "[FAIL] COO Matrix: AXPY"
        end if

        !----------------------------------------
        ! 3. XPAY Test (C = A + 4*C)
        !----------------------------------------
        call matrix_xpay(4.0d0, A, C, ierr)
        if (ierr == MATRIX_STATUS_ILL_OPERATIONS) then
            print *, "[PASS] COO Matrix: XPAY (Ill Operations Caught)"
        else
            print *, "[FAIL] COO Matrix: XPAY"
        end if

        !----------------------------------------
        ! 4. AXPYZ Test (C = 3*A + B)
        !----------------------------------------
        call matrix_axpyz(3.0d0, A, B, C, ierr)
        if (ierr == MATRIX_STATUS_ILL_OPERATIONS) then
            print *, "[PASS] COO Matrix: AXPYZ (Ill Operations Caught)"
        else
            print *, "[FAIL] COO Matrix: AXPYZ"
        end if

        !----------------------------------------
        ! 5. Scale Test
        !----------------------------------------
        call vb%initialize(N)
        call vb%set(OP_INS, [0.5d0, 2.0d0, 1.0d0, 1.5d0, 0.2d0])
        call matrix_scale(A, vb, diag, OP_SCALE_JACOBI, ierr)

        select type (A)
        type is (type_coo)
            val => A%get_val()
            vb_data => vb%get_data()

            block
                real(real64), dimension(nnz), parameter :: val_expected = [ &
                                                           1.0d0, 2.0d0, 3.0d0 / 4.0d0, 1.0d0, 5.0d0 / 4.0d0, 6.0d0 / 7.0d0, 1.0d0, &
                                                           8.0d0 / 7.0d0, 9.0d-1, 1.0d0, 11.0d-1, 12.0d0 / 13.0d0, 1.0d0]

                real(real64), dimension(N), parameter :: vb_expected = [0.5d0, 0.5d0, 1.0d0 / 7.0d0, 1.5d-1, 0.2d0 / 13.0d0]

                if (all(abs(val - val_expected) < 1.0d-12) .and. &
                    all(abs(vb_data - vb_expected) < 1.0d-12)) then
                    print *, "[PASS] COO Matrix: Scale (Jacobi)"
                else
                    print *, "[FAIL] COO Matrix: Scale (Jacobi)"
                end if

            end block
        end select

        call vb%set(OP_INS, [0.5d0, 2.0d0, 1.0d0, 1.5d0, 0.2d0])
        do i = 1, nnz
            call A%set(OP_INS, row_idx_init(i), col_idx_init(i), val_init(i))
        end do
        call matrix_scale(A, vb, diag, OP_SCALE_SYMM_DIAG, ierr)
        select type (A)
        type is (type_coo)
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
                    print *, "[PASS] COO Matrix: Scale (Symmetric Diag)"
                else
                    print *, "[FAIL] COO Matrix: Scale (Symmetric Diag)"
                end if

            end block
        end select

    end subroutine test_coo_arithmetic

end program test_linalg
