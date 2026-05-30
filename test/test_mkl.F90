!>
!> Integration test: MKL Inspector-Executor Sparse BLAS with type_matrix_bsr.
!>
!> Test scenario:
!>   2-node mesh, 2 DOFs/node (block size = 2).
!>   Sparsity (all-to-all): ptr=[1,3,5], ind=[1,2,1,2].
!>
!>   Local stiffness (1-element, nodes 1+2), for each DOF pair (rb,cb):
!>     K_local = [ 1 -1 ; -1  1 ]   (node-level, scalar per DOF pair)
!>
!>   Global 4x4 matrix A:
!>     [ 1  0 -1  0 ]
!>     [ 0  1  0 -1 ]
!>     [-1  0  1  0 ]
!>     [ 0 -1  0  1 ]
!>
!>   x = [1, 2, 3, 4]^T  =>  y = A*x = [-2, -2, 2, 2]^T
!>
program test_mkl_bsr
    use, intrinsic :: iso_fortran_env, only: int32, real64, error_unit, output_unit
    use :: testdrive, only: new_unittest, unittest_type, run_testsuite, error_type, check
    use :: module_core
    use :: module_linalg
    implicit none

    integer(int32) :: stat

    stat = 0
    call run_testsuite(collect_tests, output_unit, stat)

    if (stat /= 0) then
        write (output_unit, '(a,i0,a)') "  [FAIL] ", stat, " test(s) failed."
        error stop 1
    end if

contains

    subroutine collect_tests(tests)
        implicit none
        type(unittest_type), allocatable, intent(out) :: tests(:)
        tests = [ &
            new_unittest("bsr_set_local_matrix", test_set_local_matrix), &
            new_unittest("bsr_commit_to_mkl", test_commit_to_mkl), &
            new_unittest("bsr_spmv_mkl", test_spmv_mkl), &
            new_unittest("bsr_spmv_reference", test_spmv_reference) &
            ]
    end subroutine collect_tests

    ! ----------------------------------------------------------------
    ! Helper: build 2-node, 2-DOF/node BSR matrix with known values
    ! ----------------------------------------------------------------
    subroutine build_test_matrix(mat)
        implicit none
        type(type_matrix_bsr), intent(inout) :: mat

        integer(int32), parameter :: NNODE = 2, BDOF = 2
        integer(int32) :: ptr(3), ind(4)
        type(type_matrix_dense) :: k_local
        integer(int32) :: global_idx(2, 2)

        ! All-to-all sparsity: each node connects to both nodes
        ptr = [1, 3, 5]
        ind = [1, 2, 1, 2]

        call mat%initialize(NNODE, ptr, ind, BDOF, BDOF)

        ! Full local stiffness (NNODE*BDOF x NNODE*BDOF = 4x4):
        !   Rows/cols indexed as (i_node-1)*BDOF + dof.
        !   Global 4x4: [ 1  0 -1  0; 0  1  0 -1; -1  0  1  0; 0 -1  0  1 ]
        call k_local%initialize(NNODE * BDOF)
        call k_local%set(MATRIX_OPS%INS, 1, 1,  1.0d0)
        call k_local%set(MATRIX_OPS%INS, 2, 2,  1.0d0)
        call k_local%set(MATRIX_OPS%INS, 3, 3,  1.0d0)
        call k_local%set(MATRIX_OPS%INS, 4, 4,  1.0d0)
        call k_local%set(MATRIX_OPS%INS, 1, 3, -1.0d0)
        call k_local%set(MATRIX_OPS%INS, 3, 1, -1.0d0)
        call k_local%set(MATRIX_OPS%INS, 2, 4, -1.0d0)
        call k_local%set(MATRIX_OPS%INS, 4, 2, -1.0d0)

        ! BSR block indices: global_idx(i_node, j_node) = flat block index
        global_idx(1, 1) = 1
        global_idx(1, 2) = 2
        global_idx(2, 1) = 3
        global_idx(2, 2) = 4

        ! Single call: scatter full (4x4) local matrix into BSR val array
        call mat%set(MATRIX_OPS%ADD, NNODE, global_idx, 1, 1, k_local%get_val())

        call k_local%destroy()
    end subroutine build_test_matrix

    ! ----------------------------------------------------------------
    ! Test 1: verify assembled values directly
    ! ----------------------------------------------------------------
    subroutine test_set_local_matrix(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error

        type(type_matrix_bsr) :: mat
        real(real64), dimension(:, :, :), pointer :: val

        call build_test_matrix(mat)
        val => mat%get_val()

        ! Block (rb=1,cb=1): val(1,1,*) for blocks [1,2,3,4]
        !   K_local: node1->node1=1, node1->node2=-1, node2->node1=-1, node2->node2=1
        call check(error, abs(val(1, 1, 1) - 1.0d0)  < 1.0d-12); if (allocated(error)) return
        call check(error, abs(val(1, 1, 2) - (-1.0d0)) < 1.0d-12); if (allocated(error)) return
        call check(error, abs(val(1, 1, 3) - (-1.0d0)) < 1.0d-12); if (allocated(error)) return
        call check(error, abs(val(1, 1, 4) - 1.0d0)  < 1.0d-12); if (allocated(error)) return

        ! Block (rb=2,cb=2): same pattern
        call check(error, abs(val(2, 2, 1) - 1.0d0)  < 1.0d-12); if (allocated(error)) return
        call check(error, abs(val(2, 2, 4) - 1.0d0)  < 1.0d-12); if (allocated(error)) return

        call mat%destroy()
    end subroutine test_set_local_matrix

    ! ----------------------------------------------------------------
    ! Test 2: commit_to_mkl sets is_mkl_committed = .true.
    ! ----------------------------------------------------------------
    subroutine test_commit_to_mkl(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error

        type(type_matrix_bsr) :: mat
        integer(int32) :: ierr

        call build_test_matrix(mat)
        ierr = MATRIX_STATUS%SUCCESS%ID
        call mat%commit_to_mkl(ierr)

        call check(error, ierr == MATRIX_STATUS%SUCCESS%ID); if (allocated(error)) return
        call check(error, mat%is_mkl_handle_ready()); if (allocated(error)) return

        call mat%destroy()
    end subroutine test_commit_to_mkl

    ! ----------------------------------------------------------------
    ! Test 3: SpMV via MKL Inspector-Executor
    ! ----------------------------------------------------------------
    subroutine test_spmv_mkl(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error

        type(type_matrix_bsr) :: mat
        type(type_vector_dp)  :: xv, yv
        real(real64), parameter :: tol = 1.0d-10
        real(real64) :: y_ref(4)
        real(real64), dimension(:), pointer :: y_data
        integer(int32) :: ierr, i

        y_ref = [-2.0d0, -2.0d0, 2.0d0, 2.0d0]

        call build_test_matrix(mat)
        ierr = MATRIX_STATUS%SUCCESS%ID
        call mat%commit_to_mkl(ierr)
        call check(error, ierr == MATRIX_STATUS%SUCCESS%ID); if (allocated(error)) return

        call xv%initialize(4)
        call yv%initialize(4)
        call xv%set(VECTOR_OPS%INS, [1.0d0, 2.0d0, 3.0d0, 4.0d0])
        call yv%set(VECTOR_OPS%INS, 0.0d0)

        call matvec(mat, xv, yv, ierr)
        call check(error, ierr == MATRIX_STATUS%SUCCESS%ID); if (allocated(error)) return

        y_data => yv%get_data()
        do i = 1, 4
            call check(error, abs(y_data(i) - y_ref(i)) < tol)
            if (allocated(error)) return
        end do

        call xv%destroy()
        call yv%destroy()
        call mat%destroy()
    end subroutine test_spmv_mkl

    ! ----------------------------------------------------------------
    ! Test 4: SpMV fallback (without committed handle)
    ! ----------------------------------------------------------------
    subroutine test_spmv_reference(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error

        type(type_matrix_bsr) :: mat
        type(type_vector_dp)  :: xv, yv
        real(real64), parameter :: tol = 1.0d-10
        real(real64) :: y_ref(4)
        real(real64), dimension(:), pointer :: y_data
        integer(int32) :: ierr, i

        y_ref = [-2.0d0, -2.0d0, 2.0d0, 2.0d0]

        call build_test_matrix(mat)
        ! Do NOT call commit_to_mkl — use fallback manual SpMV

        call xv%initialize(4)
        call yv%initialize(4)
        call xv%set(VECTOR_OPS%INS, [1.0d0, 2.0d0, 3.0d0, 4.0d0])
        call yv%set(VECTOR_OPS%INS, 0.0d0)

        call matvec(mat, xv, yv, ierr)
        call check(error, ierr == MATRIX_STATUS%SUCCESS%ID); if (allocated(error)) return

        y_data => yv%get_data()
        do i = 1, 4
            call check(error, abs(y_data(i) - y_ref(i)) < tol)
            if (allocated(error)) return
        end do

        call xv%destroy()
        call yv%destroy()
        call mat%destroy()
    end subroutine test_spmv_reference

end program test_mkl_bsr
