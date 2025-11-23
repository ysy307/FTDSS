program test_solver
    use, intrinsic :: iso_fortran_env
#ifdef _MPI
    use :: mpi_f08
#endif
    use :: module_core
    use :: module_linalg
    use :: module_solver
    implicit none
    integer(int32) :: unit
#ifdef _MPI

    integer(int32) :: ierr
    call MPI_Init(ierr)
#endif
    open (newunit=unit, file="log/test/solver.log", status="replace", action="write", iostat=ierr)
    write (unit, *) "========================================"
    write (unit, *) "   Starting Solver tests..."
    write (unit, *) "========================================"
    ! Initialize the linear algebra backend (MKL or Native)
    call initialize_linalg()
    call run_test_solver_dense()
    ! write (unit, *) ""

    write (unit, *) "========================================"
    write (unit, *) "   Solver tests completed."
    write (unit, *) "========================================"

#ifdef _MPI
    call MPI_Finalize(ierr)
#endif

contains
    subroutine run_test_solver_dense()
        implicit none
        type(type_matrix_dense) :: A
        type(type_vector_dp) :: b, x

        class(abst_solver), allocatable :: solver
        integer(int32) :: ierr

        integer(int32), parameter :: n = 10
        real(real64), parameter :: h = 1.0d0 / dble(n + 1)
        real(real64) :: x_exact(n)
        integer(int32) :: i
        real(real64) :: xi
        real(real64), dimension(:), pointer :: x_ptr

        type(type_solver_settings) :: matrix_info
        type(type_preconditioner_settings) :: pc_info

        matrix_info%id = SOLVER_BICGSTAB
        matrix_info%num_nodes = n
        matrix_info%tolerance = 1.0d-12
        matrix_info%max_iterations = 10000
        pc_info%id = SOLVER_PRECONDITION_NONE
        call create_solver(solver, matrix_info, pc_info, ierr)
        if (ierr /= SOLVER_STATUS_SUCCESS) then
            write (unit, *) "Error in creating solver: ", ierr
            return
        end if

        call A%initialize(n)
        call b%initialize(n)
        call x%initialize(n)
        call A%zero()
        call b%zero()
        call x%zero()

        ! Set up the matrix A (1D Poisson problem)
        do i = 1, n
            call A%set(OP_ADD, i, i, 2.0d0)
            if (i > 1) call A%set(OP_ADD, i, i - 1, -1.0d0)
            if (i < n) call A%set(OP_ADD, i, i + 1, -1.0d0)

            call b%set(OP_INS, i, h * h)
            xi = dble(i) * h
            x_exact(i) = 0.5d0 * xi * (1.0d0 - xi)
        end do

        ! Solve Ax = b
        call solver%solve(A, b, x)
        call solver%check()

        ! Output the results
        x_ptr => x%get_data()
        if (any(abs(x_ptr - x_exact) > 1.0d-10)) then
            write (unit, *) "[FAIL]: BiCGSTAB methods failed in dense matrix."
        else
            write (unit, *) "[PASS]: BiCGSTAB methods succeeded in dense matrix."
        end if

        call A%destroy()
        call b%destroy()
        call x%destroy()
        call solver%destroy()

    end subroutine run_test_solver_dense

end program test_solver
