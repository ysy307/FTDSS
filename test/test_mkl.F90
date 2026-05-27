! program test_example
!     use, intrinsic :: iso_fortran_env
!     use mkl_spblas
!     use :: testdrive, only : &
!         new_unittest, unittest_type, &
!         run_testsuite, &
!         error_type, check
!     implicit none

!     integer :: stat

!     stat = 0

!     call run_testsuite( &
!         collect_tests, &
!         error_unit, &
!         stat &
!     )

!     if (stat /= 0) then
!         error stop 1
!     end if

! contains

!     subroutine collect_tests(tests)
!         implicit none
!         type(unittest_type), allocatable, intent(out) :: tests(:)

!         tests = [ &
!             new_unittest("square_test", test_square) &
!         ]
!     end subroutine collect_tests

!     subroutine test_square(error)
!         implicit none
!         type(error_type), allocatable, intent(out) :: error

!         call check(error, 3 * 3, 9)
!     end subroutine test_square

! end program test_example


program inspector_executor_example
    use, intrinsic :: iso_fortran_env
    use :: mkl_spblas
    implicit none
    
    ! integer, parameter :: real64 = kind(1.0d0)
    integer(int32) :: rows, cols, nnz
    integer(int32), allocatable :: row_ptr(:), col_ind(:)
    real(real64), allocatable :: values(:), x(:), y(:)
    
    type(sparse_matrix_t) :: handle
    type(matrix_descr) :: descr
    integer(int32) :: info, iter
    real(real64) :: alpha, beta
    
    ! Initialize matrix dimensions
    rows = 3
    cols = 3
    nnz = 3
    
    allocate(row_ptr(rows + 1))
    allocate(col_ind(nnz))
    allocate(values(nnz))
    allocate(x(cols))
    allocate(y(rows))
    
    ! Setup 3x3 identity matrix in CSR format
    row_ptr = [1, 2, 3, 4]
    col_ind = [1, 2, 3]
    values = [1.0_real64, 1.0_real64, 1.0_real64]
    
    descr%type = SPARSE_MATRIX_TYPE_GENERAL
    alpha = 1.0_real64
    beta = 0.0_real64
    
    ! Step 1: Create handle (Data registration)
    info = mkl_sparse_d_create_csr(handle, SPARSE_INDEX_BASE_ONE, rows, cols, &
                                   row_ptr(1:rows), row_ptr(2:rows+1), col_ind, values)
    
    ! Step 2: Analyze and optimize (Inspector phase - called once)
    info = mkl_sparse_optimize(handle)
    
    ! Initial vector x
    x = [1.0_real64, 2.0_real64, 3.0_real64]
    
    ! Step 3: Execution loop (Executor phase - called multiple times)
    do iter = 1, 10
        ! Perform SpMV: y = alpha * A * x + beta * y
        info = mkl_sparse_d_mv(SPARSE_OPERATION_NON_TRANSPOSE, alpha, handle, descr, x, beta, y)
        
        ! In a real solver, update x based on y here
        ! For this example, we simply copy y to x for the next iteration
        x = y
    end do
    
    print *, 'Final result after 10 iterations:'
    print *, y
    
    ! Step 4: Release resources
    info = mkl_sparse_destroy(handle)
    
    deallocate(row_ptr, col_ind, values, x, y)
end program inspector_executor_example