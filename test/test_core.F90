program test_core
    use iso_fortran_env, only: int32, real64
    use core_types_datetime, only: type_datetime
    use core_types_solar, only: type_solar_system
    implicit none

    type(type_solar_system) :: tokyo
    type(type_datetime) :: dt

    real(real64) :: el, az, rise, set, t_noon, alt_noon
    integer(int32) :: status

    ! Initialize
    el = 0.0d0; az = 0.0d0
    rise = 0.0d0; set = 0.0d0
    t_noon = 0.0d0; alt_noon = 0.0d0
    status = 0

    ! 1. Initialize location (NAOJ Tokyo)
    print *, "=== Initialization ==="
    call tokyo%initialize(35.6581d0, 139.7414d0, 0.0d0)
    print *, "Location set to Tokyo (NAOJ)."

    ! 2. Position calculation using type_datetime
    print *, "=== 1. Position from type_datetime ==="
    call dt%set_now()
    call tokyo%get_position(dt, el, az)
    print *, "Current Time: ", dt%format()
    print '(A, F7.2, A, F7.2)', "Elevation   : ", el, " deg / Azimuth: ", az

    ! 3. Position calculation using raw numeric values
    print *, "=== 2. Position from raw values (2025/6/21 12:00) ==="
    call tokyo%get_position(2025, 6, 21, 12.0d0, 9.0d0, el, az)
    print '(A, F7.2, A, F7.2)', "Elevation   : ", el, " deg / Azimuth: ", az

    ! 4. Sunrise/sunset/transit calculation (datetime argument)
    print *, "=== 3. Day Events using Datetime (Today) ==="
    call dt%set_now()
    ! Call generic get_day_events with datetime
    call tokyo%get_day_events(dt, rise, set, t_noon, alt_noon, status)

    if (status == 0) then
        call print_hm("Sunrise   ", rise)
        call print_hm("Transit   ", t_noon)
        call print_hm("Sunset    ", set)
        print '(A, F7.2, A)', "Noon Alt  : ", alt_noon, " deg"
    else
        print *, "Polar day or night."
    end if

    ! 5. Sunrise/sunset/transit calculation (numeric arguments)
    print *, "=== 4. Day Events using Raw Values (2025/12/31) ==="
    ! Call generic get_day_events with raw values
    call tokyo%get_day_events(2025, 12, 31, 9.0d0, rise, set, t_noon, alt_noon, status)

    if (status == 0) then
        call print_hm("Sunrise   ", rise)
        call print_hm("Transit   ", t_noon)
        call print_hm("Sunset    ", set)
        print '(A, F7.2, A)', "Noon Alt  : ", alt_noon, " deg"
    end if

contains

    subroutine print_hm(label, t_val)
        character(len=*), intent(in) :: label
        real(real64), intent(in) :: t_val
        integer(int32) :: h, m
        h = floor(t_val)
        m = nint((t_val - h) * 60.0d0)
        
        if (m == 60) then
            m = 0
            h = h + 1
        end if
        if (h >= 24) h = h - 24
        
        print '(A, ": ", I2.2, ":", I2.2)', label, h, m
    end subroutine print_hm

end program test_core

! ! program test_core
! !     use, intrinsic :: iso_fortran_env, only: int32, real64
! !     use :: mpi_f08
! !     use :: module_core
! !     implicit none
! !     integer(int32) :: ierr
! !     call MPI_Init(ierr)

! !     print *, "========================================"
! !     print *, "   Core Matrix Library Test Suite"
! !     print *, "========================================"
! !     print *

! !     call run_test_dense()
! !     print *
! !     call run_test_csr()
! !     print *
! !     call run_test_bsr()

! !     print *, "========================================"
! !     print *, "   Vector Type Test Suite"
! !     print *, "========================================"
! !     print *

! !     ! Double Precision Vector Tests
! !     call test_dp_normal()
! !     print *
! !     call test_dp_error()

! !     print *
! !     print *, "----------------------------------------"
! !     print *

! !     ! Integer Vector Tests
! !     call test_int_normal()
! !     print *
! !     call test_int_error()

! !     print *
! !     print *, "========================================"
! !     print *, "   All tests completed."
! !     print *, "========================================"
! !     call MPI_Finalize(ierr)

! ! contains

! !     ! ==================================================================
! !     ! Test: Dense Matrix
! !     ! ==================================================================
! !     subroutine run_test_dense()
! !         class(abst_matrix), allocatable :: mat
! !         real(real64), dimension(:), pointer :: diag
! !         integer(int32) :: i, j
! !         integer(int32), parameter :: N = 3
! !         logical :: success

! !         print *, "--- [Dense] Testing Functionality ---"

! !         ! 1. Create
! !         mat = create_matrix(MATRIX_DENSE, N)

! !         ! 2. Test OP_INS (Insert)
! !         ! Matrix:
! !         ! 10.0  1.0  1.0
! !         !  1.0 10.0  1.0
! !         !  1.0  1.0 10.0
! !         call mat%set(OP_INS, 10.0d0) ! Set all to 10.0 first (if impl supports scalar set_all)
! !         ! If set_all scalar is not avail, loop:
! !         do i = 1, N
! !             do j = 1, N
! !                 if (i == j) then
! !                     call mat%set(OP_INS, i, j, 10.0d0)
! !                 else
! !                     call mat%set(OP_INS, i, j, 1.0d0)
! !                 end if
! !             end do
! !         end do

! !         ! 3. Test OP_ADD (Add)
! !         ! Add 5.0 to (1,1) -> Should become 15.0
! !         call mat%set(OP_ADD, 1, 1, 5.0d0)

! !         ! 4. Verify Diagonal
! !         diag => mat%get_diagonal()
! !         success = .true.
! !         if (abs(diag(1) - 15.0d0) > 1.0d-10) success = .false.
! !         if (abs(diag(2) - 10.0d0) > 1.0d-10) success = .false.
! !         if (abs(diag(3) - 10.0d0) > 1.0d-10) success = .false.

! !         if (success) then
! !             print *, "  [PASS] Dense values & diagonal verification."
! !         else
! !             print *, "  [FAIL] Dense values verification failed."
! !             print *, "Expected diag: 15.0, 10.0, 10.0. Got:", diag
! !         end if

! !         ! 5. Test Error Handling (Out of Bounds)
! !         print *, "  [Check] Testing Out-of-Bounds Access..."
! !         call mat%set(OP_INS, N + 1, 1, 99.0d0)

! !         if (mat%get_status() /= MATRIX_STATUS_SUCCESS) then
! !             print *, "  [PASS] Error correctly detected."
! !         else
! !             print *, "  [FAIL] Error NOT detected (Status remained SUCCESS)."
! !         end if

! !         call mat%destroy()
! !     end subroutine run_test_dense

! !     ! ==================================================================
! !     ! Test: CSR Matrix
! !     ! ==================================================================
! !     subroutine run_test_csr()
! !         class(abst_matrix), allocatable :: mat
! !         real(real64), dimension(:), pointer :: diag
! !         integer(int32), allocatable :: row_ptr(:), col_ind(:)
! !         integer(int32) :: num_nodes
! !         logical :: success

! !         print *, "--- [CSR] Testing Functionality ---"

! !         ! Graph: 1-2-3 (Linear)
! !         ! 1: [1, 2]
! !         ! 2: [1, 2, 3]
! !         ! 3: [2, 3]
! !         num_nodes = 3
! !         row_ptr = [1, 3, 6, 8]
! !         col_ind = [1, 2, 1, 2, 3, 2, 3]

! !         ! 1. Create
! !         mat = create_matrix(MATRIX_CSR, num_nodes, row_ptr, col_ind)

! !         ! 2. Test Set Values (Laplacian-like)
! !         call mat%zero()
! !         ! Node 1
! !         call mat%set(OP_INS, 1, 1, 2.0d0)
! !         call mat%set(OP_INS, 1, 2, -1.0d0)
! !         ! Node 2
! !         call mat%set(OP_INS, 2, 1, -1.0d0)
! !         call mat%set(OP_INS, 2, 2, 2.0d0)
! !         call mat%set(OP_INS, 2, 3, -1.0d0)
! !         ! Node 3
! !         call mat%set(OP_INS, 3, 2, -1.0d0)
! !         call mat%set(OP_INS, 3, 3, 2.0d0)

! !         ! 3. Test Add Value
! !         ! Add 10.0 to (2,2) -> Should become 12.0
! !         call mat%set(OP_ADD, 2, 2, 10.0d0)

! !         ! 4. Verify Diagonal
! !         diag => mat%get_diagonal()
! !         success = .true.
! !         if (abs(diag(1) - 2.0d0) > 1.0d-10) success = .false.
! !         if (abs(diag(2) - 12.0d0) > 1.0d-10) success = .false. ! 2.0 + 10.0
! !         if (abs(diag(3) - 2.0d0) > 1.0d-10) success = .false.

! !         if (success) then
! !             print *, "  [PASS] CSR sparsity & operations verification."
! !         else
! !             print *, "  [FAIL] CSR verification failed."
! !             print *, "Expected diag: 2.0, 12.0, 2.0. Got:", diag
! !         end if

! !         ! 5. Test Structural Zero Access (Crucial for Sparse Matrix)
! !         print *, "  [Check] Testing Structural Zero Access (1, 3)..."
! !         ! Node 1 is NOT connected to Node 3 in the graph above.
! !         call mat%set(OP_INS, 1, 3, 999.0d0)

! !         if (mat%get_status() /= MATRIX_STATUS_SUCCESS) then
! !             print *, "  [PASS] Structural zero access correctly flagged as error."
! !         else
! !             print *, "  [FAIL] Failed to detect structural zero access."
! !         end if

! !         call mat%destroy()
! !     end subroutine run_test_csr

! !     ! ==================================================================
! !     ! Test: BSR Matrix
! !     ! ==================================================================
! !     subroutine run_test_bsr()
! !         class(abst_matrix), allocatable :: mat
! !         real(real64), dimension(:), pointer :: diag
! !         integer(int32), allocatable :: row_ptr(:), col_ind(:)
! !         integer(int32) :: num_nodes, blk_sz
! !         integer(int32) :: r, c
! !         logical :: success

! !         print *, "--- [BSR] Testing Functionality ---"

! !         ! 2 Nodes fully connected, 2x2 Blocks
! !         num_nodes = 2
! !         blk_sz = 2
! !         row_ptr = [1, 3, 5]
! !         col_ind = [1, 2, 1, 2]

! !         ! 1. Create
! !         mat = create_matrix(MATRIX_BSR, num_nodes, row_ptr, col_ind, blk_sz)

! !         ! 2. Fill Blocks
! !         ! Fill diagonal blocks with Identity, off-diagonal with 0.5
! !         do r = 1, num_nodes
! !             do c = 1, num_nodes
! !                 if (r == c) then
! !                     ! Identity Matrix in Block (r, r)
! !                     call mat%set(OP_INS, r, c, 1, 1, 1.0d0)
! !                     call mat%set(OP_INS, r, c, 2, 2, 1.0d0)
! !                     call mat%set(OP_INS, r, c, 1, 2, 0.0d0)
! !                     call mat%set(OP_INS, r, c, 2, 1, 0.0d0)
! !                 else
! !                     ! 0.5 everywhere in off-diagonal block
! !                     call mat%set(OP_INS, r, c, 1, 1, 0.5d0)
! !                     call mat%set(OP_INS, r, c, 1, 2, 0.5d0)
! !                     call mat%set(OP_INS, r, c, 2, 1, 0.5d0)
! !                     call mat%set(OP_INS, r, c, 2, 2, 0.5d0)
! !                 end if
! !             end do
! !         end do

! !         ! 3. Test Add Operation on Block
! !         ! Add 10.0 to Node 1, DOF 1 (Global index 1)
! !         call mat%set(OP_ADD, 1, 1, 1, 1, 10.0d0)

! !         ! 4. Verify Values (via Display or future get_val)
! !         ! For now, we rely on successful execution and display,
! !         ! as get_diagonal implementation for BSR might be complex (extracting block diagonals).
! !         ! Let's assume get_diagonal returns the main diagonal of the expanded matrix.

! !         diag => mat%get_diagonal()
! !         success = .true.
! !         ! Node 1, DOF 1: 1.0 + 10.0 = 11.0
! !         if (abs(diag(1) - 11.0d0) > 1.0d-10) success = .false.
! !         ! Node 1, DOF 2: 1.0
! !         if (abs(diag(2) - 1.0d0) > 1.0d-10) success = .false.

! !         if (success) then
! !             print *, "  [PASS] BSR block values & operations verification."
! !         else
! !             print *, "  [FAIL] BSR verification failed. Check diagonal extraction logic."
! !             print *, "Expected diag(1:2): 11.0, 1.0. Got:", diag(1:2)
! !         end if

! !         ! 5. Test Invalid Block Sub-index
! !         print *, "  [Check] Testing Invalid Block Sub-index (3,3)..."
! !         call mat%set(OP_INS, 1, 1, 3, 3, 99.0d0) ! Block is 2x2

! !         if (mat%get_status() /= MATRIX_STATUS_SUCCESS) then
! !             print *, "  [PASS] Invalid block index correctly detected."
! !         else
! !             print *, "  [FAIL] Error NOT detected."
! !         end if

! !         call mat%destroy()
! !     end subroutine run_test_bsr

! !     ! ==================================================================
! !     ! Double Precision Tests
! !     ! ==================================================================
! !     subroutine test_dp_normal()
! !         type(type_vector_dp) :: v, v_copy
! !         real(real64), pointer :: data_ptr(:)
! !         integer(int32) :: n = 5
! !         integer(int32) :: nb = 2 ! 2 Blocks
! !         logical :: success

! !         print *, "--- [DP] Normal Case: Block Vector Operations ---"

! !         ! 1. Initialize
! !         call v%initialize(n, nb)

! !         ! 2. Set Scalar (Block-wise)
! !         call v%set(OP_INS, 1.0d0, row_block=1)
! !         call v%set(OP_INS, 2.0d0, row_block=2)

! !         ! 3. Add Operation
! !         call v%set(OP_ADD, 0.5d0, row_block=1)

! !         ! 4. Set specific index
! !         call v%set(OP_INS, 3, 99.0d0, row_block=1)

! !         ! 5. Scatter
! !         call v%set(OP_INS, [1, 5], [10.0d0, 10.0d0], row_block=2)

! !         ! Verify Data
! !         data_ptr => v%get_data()
! !         success = .true.

! !         if (abs(data_ptr(1) - 1.5d0) > 1.0d-10) success = .false.
! !         if (abs(data_ptr(5) - 99.0d0) > 1.0d-10) success = .false.
! !         if (abs(data_ptr(2) - 10.0d0) > 1.0d-10) success = .false.
! !         if (abs(data_ptr(4) - 2.0d0) > 1.0d-10) success = .false.

! !         if (success) then
! !             print *, "  [PASS] Values verified correctly."
! !         else
! !             print *, "  [FAIL] Value mismatch found!"
! !             call v%display()
! !         end if

! !         ! 6. Copy Test
! !         call v_copy%copy(v)

! !         ! Status check (verify no errors in normal case)
! !         call v_copy%check()

! !         if (v_copy%get_status() == VECTOR_STATUS_SUCCESS) then
! !             if (v_copy%get_size() == v%get_size()) then
! !                 print *, "  [PASS] Copy successful."
! !             else
! !                 print *, "  [FAIL] Copy size mismatch."
! !             end if
! !         else
! !             print *, "  [FAIL] Copy operation reported error."
! !         end if

! !         call v%destroy()
! !         call v_copy%destroy()
! !     end subroutine test_dp_normal

! !     subroutine test_dp_error()
! !         type(type_vector_dp) :: v
! !         integer(int32) :: n = 5

! !         print *, "--- [DP] Error Case: Out of Bounds ---"

! !         call v%initialize(n) ! Default 1 block

! !         ! Test 1: Access Node N+1
! !         print *, "  Trying to set value at index", n + 1
! !         call v%set(OP_INS, n + 1, 10.0d0)

! !         ! check() displays error messages, get_status() determines result
! !         call v%check()
! !         if (v%get_status() /= VECTOR_STATUS_SUCCESS) then
! !             print *, "  [PASS] Error correctly detected (Index out of bounds)."
! !         else
! !             print *, "  [FAIL] Error NOT detected."
! !         end if

! !         ! Reset status for next test (if destroy/init is not called)
! !         ! Re-initialize to clear status
! !         call v%initialize(n)

! !         ! Test 2: Access Invalid Block
! !         print *, "  Trying to set scalar for Block 2 (Allocated only 1)"
! !         call v%set(OP_INS, 5.0d0, row_block=2)

! !         call v%check()
! !         if (v%get_status() /= VECTOR_STATUS_SUCCESS) then
! !             print *, "  [PASS] Error correctly detected (Invalid Block)."
! !         else
! !             print *, "  [FAIL] Error NOT detected."
! !         end if

! !         call v%destroy()
! !     end subroutine test_dp_error

! !     ! ==================================================================
! !     ! Integer Tests
! !     ! ==================================================================
! !     subroutine test_int_normal()
! !         type(type_vector_int) :: v, v_copy
! !         integer(int32), pointer :: data_ptr(:)
! !         integer(int32) :: n = 5
! !         integer(int32) :: nb = 2
! !         logical :: success
! !         integer(int32) :: i

! !         print *, "--- [INT] Normal Case: Multi-Block Operations ---"

! !         call v%initialize(n, nb)

! !         call v%set(OP_INS, [(10, i=1, n)], row_block=1)
! !         call v%set(OP_INS, [(20, i=1, n)], row_block=2)
! !         call v%set(OP_ADD, [(5, i=1, n)], row_block=1)
! !         call v%set(OP_INS, 3, 999, row_block=1)
! !         call v%set(OP_INS, [1, 5], [888, 888], row_block=2)

! !         ! Copy & Check Status
! !         call v_copy%copy(v)
! !         call v_copy%check()

! !         if (v_copy%get_status() /= VECTOR_STATUS_SUCCESS) then
! !             print *, "  [FAIL] Copy reported error."
! !             return
! !         end if

! !         ! Verify
! !         data_ptr => v_copy%get_data()
! !         success = .true.

! !         if (data_ptr(1) /= 15) success = .false.
! !         if (data_ptr(5) /= 999) success = .false.
! !         if (data_ptr(2) /= 888) success = .false.
! !         if (data_ptr(4) /= 20) success = .false.

! !         if (success) then
! !             print *, "  [PASS] All values match expected results."
! !         else
! !             print *, "  [FAIL] Data mismatch detected."
! !             call v_copy%display()
! !         end if

! !         call v%destroy()
! !         call v_copy%destroy()
! !     end subroutine test_int_normal

! !     subroutine test_int_error()
! !         type(type_vector_int) :: v
! !         integer(int32) :: n = 5
! !         integer(int32) :: i

! !         print *, "--- [INT] Error Case: Boundary Checks ---"

! !         call v%initialize(n) ! Default 1 Block

! !         ! Test 1: Access to non-existent block
! !         print *, "  Trying to set Block 2 (Allocated only 1)..."
! !         call v%set(OP_INS, [(0, i=1, n)], row_block=2)

! !         ! check() displays error messages
! !         call v%check()

! !         ! get_status() determines result
! !         if (v%get_status() /= VECTOR_STATUS_SUCCESS) then
! !             print *, "  [PASS] Status correctly indicates error."
! !         else
! !             print *, "  [FAIL] Status remained SUCCESS but should have failed."
! !         end if

! !         print *
! !         print *, "  NOTE: Index out-of-bounds checks (e.g. Node N+1) in"
! !         print *, "        set_value_at_index are guarded by #ifdef USE_DEBUG."

! !         call v%destroy()
! !     end subroutine test_int_error

! ! end program test_core

! program test_graph_construction
!     use core_types_graph
!     use, intrinsic :: iso_fortran_env
!     implicit none

!     print *, "========================================"
!     print *, " Test: Build Simple Undirected CSR Graph"
!     print *, "========================================"

!     call test_case_basic()
!     call test_case_complex()
!     call test_case_disconnected()

! contains

!     ! ------------------------------------------------------------------
!     ! Case 1: Basic triangle graph
!     ! Nodes: 3
!     ! Edges: (1-2), (2-3), (3-1)
!     ! ------------------------------------------------------------------
!     subroutine test_case_basic()
!         type(type_graph) :: g
!         integer(int32), allocatable :: pairs(:, :)
!         integer(int32) :: num_nodes = 3

!         print *, new_line('a')//"--- Case 1: Basic Triangle (1-2-3-1) ---"

!         allocate (pairs(2, 3))
!         pairs(:, 1) = [1, 2]
!         pairs(:, 2) = [2, 3]
!         pairs(:, 3) = [3, 1]

!         call g%build(pairs, num_nodes)
!         call print_graph_info(g)

!         ! Simple check
!         if (g%num_edges == 3) then
!             print *, "[PASS] Edge count is correct (3)."
!         else
!             print *, "[FAIL] Edge count is wrong."
!         end if
!     end subroutine test_case_basic

!     ! ------------------------------------------------------------------
!     ! Case 2: Dirty data with duplicates, self-loops, and reverse-order edges
!     ! Nodes: 4
!     ! Input data:
!     !  (1, 2) -> Valid
!     !  (1, 2) -> Duplicate (should be ignored)
!     !  (2, 1) -> Reverse duplicate (should be ignored)
!     !  (3, 3) -> Self-loop (should be ignored)
!     !  (1, 4) -> Valid
!     ! Expected result:
!     !  Only 2 edges: {1,2} and {1,4}
!     ! ------------------------------------------------------------------
!     subroutine test_case_complex()
!         type(type_graph) :: g
!         integer(int32), allocatable :: pairs(:, :)
!         integer(int32) :: num_nodes = 4

!         print *, new_line('a')//"--- Case 2: Dirty Data (Loops & Duplicates) ---"

!         allocate (pairs(2, 5))
!         pairs(:, 1) = [1, 2] ! OK
!         pairs(:, 2) = [1, 2] ! Duplicate
!         pairs(:, 3) = [2, 1] ! Duplicate (Reverse)
!         pairs(:, 4) = [3, 3] ! Self-loop
!         pairs(:, 5) = [1, 4] ! OK

!         call g%build(pairs, num_nodes)
!         call print_graph_info(g)

!         ! Check
!         if (g%num_edges == 2) then
!             print *, "[PASS] Edge count is correct (2). Duplicates/Loops removed."
!         else
!             print *, "[FAIL] Edge count is wrong. Expected 2, got ", g%num_edges
!         end if

!         ! Verify adjacency list (Node 1 should be connected to 2 and 4)
!         ! Since sorted, col_ind should have 2 first, then 4
!         if (g%col_ind(g%row_ptr(1)) == 2 .and. g%col_ind(g%row_ptr(1) + 1) == 4) then
!             print *, "[PASS] Node 1 neighbors are sorted and correct: [2, 4]"
!         else
!             print *, "[FAIL] Node 1 neighbors are incorrect."
!         end if
!     end subroutine test_case_complex

!     ! ------------------------------------------------------------------
!     ! Case 3: Graph with isolated nodes
!     ! Nodes: 5
!     ! Edges: (1, 5) only. Nodes 2, 3, 4 are isolated.
!     ! ------------------------------------------------------------------
!     subroutine test_case_disconnected()
!         type(type_graph) :: g
!         integer(int32), allocatable :: pairs(:, :)
!         integer(int32) :: num_nodes = 5

!         print *, new_line('a')//"--- Case 3: Disconnected Nodes ---"

!         allocate (pairs(2, 1))
!         pairs(:, 1) = [1, 5]

!         call g%build(pairs, num_nodes)
!         call print_graph_info(g)

!         ! row_ptr consistency check
!         ! For isolated node i, row_ptr(i) == row_ptr(i+1)
!         if (g%row_ptr(2) == g%row_ptr(3) .and. g%row_ptr(3) == g%row_ptr(4)) then
!             print *, "[PASS] Isolated nodes (2,3) handled correctly (empty range)."
!         else
!             print *, "[FAIL] Isolated nodes row_ptr structure is wrong."
!         end if
!     end subroutine test_case_disconnected

!     ! ------------------------------------------------------------------
!     ! Utility: Display graph contents
!     ! ------------------------------------------------------------------
!     subroutine print_graph_info(g)
!         type(type_graph), intent(in) :: g
!         integer :: i, j, start_idx, end_idx

!         print *, "Num Nodes:", g%num_nodes
!         print *, "Num Edges:", g%num_edges

!         print *, "Row Ptr: ", g%row_ptr
!         print *, "Col Ind: ", g%col_ind

!         print *, "--- Adjacency List (Sorted) ---"
!         do i = 1, g%num_nodes
!             start_idx = g%row_ptr(i)
!             end_idx = g%row_ptr(i + 1) - 1

!             write (*, '(A, I0, A)', advance='no') "Node ", i, ": ["
!             if (start_idx <= end_idx) then
!                 do j = start_idx, end_idx
!                     write (*, '(I0)', advance='no') g%col_ind(j)
!                     if (j < end_idx) write (*, '(A)', advance='no') ", "
!                 end do
!             end if
!             print *, "]"
!         end do
!     end subroutine print_graph_info

! end program test_graph_construction
