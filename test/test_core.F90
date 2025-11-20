program test_core
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: mpi_f08
    use :: module_core
    implicit none
    integer(int32) :: ierr
    call MPI_Init(ierr)

    print *, "========================================"
    print *, "   Core Matrix Library Test Suite"
    print *, "========================================"
    print *

    call run_test_dense()
    print *
    call run_test_csr()
    print *
    call run_test_bsr()

    print *, "========================================"
    print *, "   Vector Type Test Suite"
    print *, "========================================"
    print *

    ! Double Precision Vector Tests
    call test_dp_normal()
    print *
    call test_dp_error()

    print *
    print *, "----------------------------------------"
    print *

    ! Integer Vector Tests
    call test_int_normal()
    print *
    call test_int_error()

    print *
    print *, "========================================"
    print *, "   All tests completed."
    print *, "========================================"
    call MPI_Finalize(ierr)

contains

    ! ==================================================================
    ! Test: Dense Matrix
    ! ==================================================================
    subroutine run_test_dense()
        class(abst_matrix), allocatable :: mat
        real(real64), dimension(:), pointer :: diag
        integer(int32) :: i, j
        integer(int32), parameter :: N = 3
        logical :: success

        print *, "--- [Dense] Testing Functionality ---"

        ! 1. Create
        mat = create_matrix(MATRIX_DENSE, N)

        ! 2. Test OP_INS (Insert)
        ! Matrix:
        ! 10.0  1.0  1.0
        !  1.0 10.0  1.0
        !  1.0  1.0 10.0
        call mat%set(OP_INS, 10.0d0) ! Set all to 10.0 first (if impl supports scalar set_all)
        ! If set_all scalar is not avail, loop:
        do i = 1, N
            do j = 1, N
                if (i == j) then
                    call mat%set(OP_INS, i, j, 10.0d0)
                else
                    call mat%set(OP_INS, i, j, 1.0d0)
                end if
            end do
        end do

        ! 3. Test OP_ADD (Add)
        ! Add 5.0 to (1,1) -> Should become 15.0
        call mat%set(OP_ADD, 1, 1, 5.0d0)

        ! 4. Verify Diagonal
        diag => mat%get_diagonal()
        success = .true.
        if (abs(diag(1) - 15.0d0) > 1.0d-10) success = .false.
        if (abs(diag(2) - 10.0d0) > 1.0d-10) success = .false.
        if (abs(diag(3) - 10.0d0) > 1.0d-10) success = .false.

        if (success) then
            print *, "  [PASS] Dense values & diagonal verification."
        else
            print *, "  [FAIL] Dense values verification failed."
            print *, "Expected diag: 15.0, 10.0, 10.0. Got:", diag
        end if

        ! 5. Test Error Handling (Out of Bounds)
        print *, "  [Check] Testing Out-of-Bounds Access..."
        call mat%set(OP_INS, N + 1, 1, 99.0d0)

        if (mat%get_status() /= MATRIX_STATUS_SUCCESS) then
            print *, "  [PASS] Error correctly detected."
        else
            print *, "  [FAIL] Error NOT detected (Status remained SUCCESS)."
        end if

        call mat%destroy()
    end subroutine run_test_dense

    ! ==================================================================
    ! Test: CSR Matrix
    ! ==================================================================
    subroutine run_test_csr()
        class(abst_matrix), allocatable :: mat
        real(real64), dimension(:), pointer :: diag
        integer(int32), allocatable :: row_ptr(:), col_ind(:)
        integer(int32) :: num_nodes
        logical :: success

        print *, "--- [CSR] Testing Functionality ---"

        ! Graph: 1-2-3 (Linear)
        ! 1: [1, 2]
        ! 2: [1, 2, 3]
        ! 3: [2, 3]
        num_nodes = 3
        row_ptr = [1, 3, 6, 8]
        col_ind = [1, 2, 1, 2, 3, 2, 3]

        ! 1. Create
        mat = create_matrix(MATRIX_CSR, num_nodes, row_ptr, col_ind)

        ! 2. Test Set Values (Laplacian-like)
        call mat%zero()
        ! Node 1
        call mat%set(OP_INS, 1, 1, 2.0d0)
        call mat%set(OP_INS, 1, 2, -1.0d0)
        ! Node 2
        call mat%set(OP_INS, 2, 1, -1.0d0)
        call mat%set(OP_INS, 2, 2, 2.0d0)
        call mat%set(OP_INS, 2, 3, -1.0d0)
        ! Node 3
        call mat%set(OP_INS, 3, 2, -1.0d0)
        call mat%set(OP_INS, 3, 3, 2.0d0)

        ! 3. Test Add Value
        ! Add 10.0 to (2,2) -> Should become 12.0
        call mat%set(OP_ADD, 2, 2, 10.0d0)

        ! 4. Verify Diagonal
        diag => mat%get_diagonal()
        success = .true.
        if (abs(diag(1) - 2.0d0) > 1.0d-10) success = .false.
        if (abs(diag(2) - 12.0d0) > 1.0d-10) success = .false. ! 2.0 + 10.0
        if (abs(diag(3) - 2.0d0) > 1.0d-10) success = .false.

        if (success) then
            print *, "  [PASS] CSR sparsity & operations verification."
        else
            print *, "  [FAIL] CSR verification failed."
            print *, "Expected diag: 2.0, 12.0, 2.0. Got:", diag
        end if

        ! 5. Test Structural Zero Access (Crucial for Sparse Matrix)
        print *, "  [Check] Testing Structural Zero Access (1, 3)..."
        ! Node 1 is NOT connected to Node 3 in the graph above.
        call mat%set(OP_INS, 1, 3, 999.0d0)

        if (mat%get_status() /= MATRIX_STATUS_SUCCESS) then
            print *, "  [PASS] Structural zero access correctly flagged as error."
        else
            print *, "  [FAIL] Failed to detect structural zero access."
        end if

        call mat%destroy()
    end subroutine run_test_csr

    ! ==================================================================
    ! Test: BSR Matrix
    ! ==================================================================
    subroutine run_test_bsr()
        class(abst_matrix), allocatable :: mat
        real(real64), dimension(:), pointer :: diag
        integer(int32), allocatable :: row_ptr(:), col_ind(:)
        integer(int32) :: num_nodes, blk_sz
        integer(int32) :: r, c
        logical :: success

        print *, "--- [BSR] Testing Functionality ---"

        ! 2 Nodes fully connected, 2x2 Blocks
        num_nodes = 2
        blk_sz = 2
        row_ptr = [1, 3, 5]
        col_ind = [1, 2, 1, 2]

        ! 1. Create
        mat = create_matrix(MATRIX_BSR, num_nodes, row_ptr, col_ind, blk_sz)

        ! 2. Fill Blocks
        ! Fill diagonal blocks with Identity, off-diagonal with 0.5
        do r = 1, num_nodes
            do c = 1, num_nodes
                if (r == c) then
                    ! Identity Matrix in Block (r, r)
                    call mat%set(OP_INS, r, c, 1, 1, 1.0d0)
                    call mat%set(OP_INS, r, c, 2, 2, 1.0d0)
                    call mat%set(OP_INS, r, c, 1, 2, 0.0d0)
                    call mat%set(OP_INS, r, c, 2, 1, 0.0d0)
                else
                    ! 0.5 everywhere in off-diagonal block
                    call mat%set(OP_INS, r, c, 1, 1, 0.5d0)
                    call mat%set(OP_INS, r, c, 1, 2, 0.5d0)
                    call mat%set(OP_INS, r, c, 2, 1, 0.5d0)
                    call mat%set(OP_INS, r, c, 2, 2, 0.5d0)
                end if
            end do
        end do

        ! 3. Test Add Operation on Block
        ! Add 10.0 to Node 1, DOF 1 (Global index 1)
        call mat%set(OP_ADD, 1, 1, 1, 1, 10.0d0)

        ! 4. Verify Values (via Display or future get_val)
        ! For now, we rely on successful execution and display,
        ! as get_diagonal implementation for BSR might be complex (extracting block diagonals).
        ! Let's assume get_diagonal returns the main diagonal of the expanded matrix.

        diag => mat%get_diagonal()
        success = .true.
        ! Node 1, DOF 1: 1.0 + 10.0 = 11.0
        if (abs(diag(1) - 11.0d0) > 1.0d-10) success = .false.
        ! Node 1, DOF 2: 1.0
        if (abs(diag(2) - 1.0d0) > 1.0d-10) success = .false.

        if (success) then
            print *, "  [PASS] BSR block values & operations verification."
        else
            print *, "  [FAIL] BSR verification failed. Check diagonal extraction logic."
            print *, "Expected diag(1:2): 11.0, 1.0. Got:", diag(1:2)
        end if

        ! 5. Test Invalid Block Sub-index
        print *, "  [Check] Testing Invalid Block Sub-index (3,3)..."
        call mat%set(OP_INS, 1, 1, 3, 3, 99.0d0) ! Block is 2x2

        if (mat%get_status() /= MATRIX_STATUS_SUCCESS) then
            print *, "  [PASS] Invalid block index correctly detected."
        else
            print *, "  [FAIL] Error NOT detected."
        end if

        call mat%destroy()
    end subroutine run_test_bsr

    ! ==================================================================
    ! Double Precision Tests
    ! ==================================================================
    subroutine test_dp_normal()
        type(type_vector_dp) :: v, v_copy
        real(real64), pointer :: data_ptr(:)
        integer(int32) :: n = 5
        integer(int32) :: nb = 2 ! 2 Blocks
        logical :: success

        print *, "--- [DP] Normal Case: Block Vector Operations ---"

        ! 1. Initialize
        call v%initialize(n, nb)

        ! 2. Set Scalar (Block-wise)
        call v%set(OP_INS, 1.0d0, row_block=1)
        call v%set(OP_INS, 2.0d0, row_block=2)

        ! 3. Add Operation
        call v%set(OP_ADD, 0.5d0, row_block=1)

        ! 4. Set specific index
        call v%set(OP_INS, 3, 99.0d0, row_block=1)

        ! 5. Scatter
        call v%set(OP_INS, [1, 5], [10.0d0, 10.0d0], row_block=2)

        ! Verify Data
        data_ptr => v%get_data()
        success = .true.

        if (abs(data_ptr(1) - 1.5d0) > 1.0d-10) success = .false.
        if (abs(data_ptr(5) - 99.0d0) > 1.0d-10) success = .false.
        if (abs(data_ptr(2) - 10.0d0) > 1.0d-10) success = .false.
        if (abs(data_ptr(4) - 2.0d0) > 1.0d-10) success = .false.

        if (success) then
            print *, "  [PASS] Values verified correctly."
        else
            print *, "  [FAIL] Value mismatch found!"
            call v%display()
        end if

        ! 6. Copy Test
        call v_copy%copy(v)

        ! ステータスチェック (正常系なのでエラーが出ないことを確認)
        call v_copy%check()

        if (v_copy%get_status() == VECTOR_STATUS_SUCCESS) then
            if (v_copy%get_size() == v%get_size()) then
                print *, "  [PASS] Copy successful."
            else
                print *, "  [FAIL] Copy size mismatch."
            end if
        else
            print *, "  [FAIL] Copy operation reported error."
        end if

        call v%destroy()
        call v_copy%destroy()
    end subroutine test_dp_normal

    subroutine test_dp_error()
        type(type_vector_dp) :: v
        integer(int32) :: n = 5

        print *, "--- [DP] Error Case: Out of Bounds ---"

        call v%initialize(n) ! Default 1 block

        ! Test 1: Access Node N+1
        print *, "  Trying to set value at index", n + 1
        call v%set(OP_INS, n + 1, 10.0d0)

        ! check() でエラーメッセージを表示し、get_status() で判定
        call v%check()
        if (v%get_status() /= VECTOR_STATUS_SUCCESS) then
            print *, "  [PASS] Error correctly detected (Index out of bounds)."
        else
            print *, "  [FAIL] Error NOT detected."
        end if

        ! Reset status for next test (if destroy/init is not called)
        ! Re-initialize to clear status
        call v%initialize(n)

        ! Test 2: Access Invalid Block
        print *, "  Trying to set scalar for Block 2 (Allocated only 1)"
        call v%set(OP_INS, 5.0d0, row_block=2)

        call v%check()
        if (v%get_status() /= VECTOR_STATUS_SUCCESS) then
            print *, "  [PASS] Error correctly detected (Invalid Block)."
        else
            print *, "  [FAIL] Error NOT detected."
        end if

        call v%destroy()
    end subroutine test_dp_error

    ! ==================================================================
    ! Integer Tests
    ! ==================================================================
    subroutine test_int_normal()
        type(type_vector_int) :: v, v_copy
        integer(int32), pointer :: data_ptr(:)
        integer(int32) :: n = 5
        integer(int32) :: nb = 2
        logical :: success
        integer(int32) :: i

        print *, "--- [INT] Normal Case: Multi-Block Operations ---"

        call v%initialize(n, nb)

        call v%set(OP_INS, [(10, i=1, n)], row_block=1)
        call v%set(OP_INS, [(20, i=1, n)], row_block=2)
        call v%set(OP_ADD, [(5, i=1, n)], row_block=1)
        call v%set(OP_INS, 3, 999, row_block=1)
        call v%set(OP_INS, [1, 5], [888, 888], row_block=2)

        ! Copy & Check Status
        call v_copy%copy(v)
        call v_copy%check()

        if (v_copy%get_status() /= VECTOR_STATUS_SUCCESS) then
            print *, "  [FAIL] Copy reported error."
            return
        end if

        ! Verify
        data_ptr => v_copy%get_data()
        success = .true.

        if (data_ptr(1) /= 15) success = .false.
        if (data_ptr(5) /= 999) success = .false.
        if (data_ptr(2) /= 888) success = .false.
        if (data_ptr(4) /= 20) success = .false.

        if (success) then
            print *, "  [PASS] All values match expected results."
        else
            print *, "  [FAIL] Data mismatch detected."
            call v_copy%display()
        end if

        call v%destroy()
        call v_copy%destroy()
    end subroutine test_int_normal

    subroutine test_int_error()
        type(type_vector_int) :: v
        integer(int32) :: n = 5
        integer(int32) :: i

        print *, "--- [INT] Error Case: Boundary Checks ---"

        call v%initialize(n) ! Default 1 Block

        ! Test 1: 存在しないブロックへのアクセス
        print *, "  Trying to set Block 2 (Allocated only 1)..."
        call v%set(OP_INS, [(0, i=1, n)], row_block=2)

        ! check() でエラーメッセージを表示
        call v%check()

        ! get_status() で判定
        if (v%get_status() /= VECTOR_STATUS_SUCCESS) then
            print *, "  [PASS] Status correctly indicates error."
        else
            print *, "  [FAIL] Status remained SUCCESS but should have failed."
        end if

        print *
        print *, "  NOTE: Index out-of-bounds checks (e.g. Node N+1) in"
        print *, "        set_value_at_index are guarded by #ifdef USE_DEBUG."

        call v%destroy()
    end subroutine test_int_error

end program test_core
