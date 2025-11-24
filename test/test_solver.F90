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
    integer(int32), parameter :: num_solvers = 1
    integer(int32), parameter :: solver_id_lists(num_solvers) = [SOLVER_BICGSTAB]
    character(len=32), parameter :: solver_name_lists(num_solvers) = ["BiCGSTAB"]
    integer(int32), parameter :: num_preconditioners = 2
    integer(int32), parameter :: pc_id_lists(num_preconditioners) = [SOLVER_PRECONDITION_NONE, SOLVER_PRECONDITION_JACOBI]
    character(len=32), parameter :: pc_name_lists(num_preconditioners) = ["None", "Jacobi"]
#ifdef _MPI

    integer(int32) :: ierr
    call MPI_Init(ierr)
#endif
    open (newunit=unit, file="log/test/solver.log", status="replace", action="write", iostat=ierr)
    write (unit, '(a)') "============================================================"
    write (unit, '(a)') "   Starting Solver tests..."
    write (unit, '(a)') "============================================================"
    ! Initialize the linear algebra backend (MKL or Native)
    call initialize_linalg()
    call run_test_solver_dense()
    ! write (unit, '(a)') ""

    write (unit, '(a)') "============================================================"
    write (unit, '(a)') "   Solver tests completed."
    write (unit, '(a)') "============================================================"

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
        integer(int32) :: i, is, ip
        real(real64) :: xi
        real(real64), dimension(:), pointer :: x_ptr

        type(type_solver_settings) :: matrix_info
        type(type_preconditioner_settings) :: pc_info

        do is = 1, num_solvers
            do ip = 1, num_preconditioners
                write (unit, '(4a)') " Testing solver: ", trim(solver_name_lists(is)), &
                    " with preconditioner: ", trim(pc_name_lists(ip))
                select case (solver_id_lists(is))
                case (SOLVER_BICGSTAB)
                    call matrix_info%set(solver_id_lists(is), n, 1.0d-12, 10000)
                end select
                select case (pc_id_lists(ip))
                case (SOLVER_PRECONDITION_NONE)
                    call pc_info%set(pc_id_lists(ip))
                case (SOLVER_PRECONDITION_JACOBI)
                    call pc_info%set(pc_id_lists(ip), n)
                end select
                call create_solver(solver, matrix_info, pc_info, ierr)
                if (ierr /= SOLVER_STATUS_SUCCESS) then
                    write (unit, '(a,i0)') "Error in creating solver: ", ierr
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
                call solver%display_rhistory(unit)

                ! Output the results
                x_ptr => x%get_data()
                if (any(abs(x_ptr - x_exact) > 1.0d-10)) then
                    write (unit, '(3a)') "[FAIL]: ", trim(solver_name_lists(is)), " methods failed in dense matrix."
                else
                    write (unit, '(3a)') "[PASS]: ", trim(solver_name_lists(is)), " methods succeeded in dense matrix."
                end if

                call A%destroy()
                call b%destroy()
                call x%destroy()
                call solver%destroy()
            end do
        end do

    end subroutine run_test_solver_dense

end program test_solver
