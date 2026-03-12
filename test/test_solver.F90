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
    integer(int32), parameter :: num_matrix_sizes = 3
    integer(int32), parameter :: matrix_size(num_matrix_sizes) = [100, 500, 1000]
    integer(int32), parameter :: num_solvers = 2
    integer(int32), parameter :: solver_id_lists(num_solvers) = [SOLVER_BICGSTAB, SOLVER_GMRES_M]
    character(len=32), parameter :: solver_name_lists(num_solvers) = ["BiCGSTAB", "GMRES"]
    integer(int32), parameter :: num_preconditioners = 3
    integer(int32), parameter :: pc_id_lists(num_preconditioners) = [ &
                                 SOLVER_PRECONDITION_NONE, SOLVER_PRECONDITION_JACOBI, SOLVER_PRECONDITION_ILU]
    character(len=32), parameter :: pc_name_lists(num_preconditioners) = ["None", "Jacobi", "ILU(k)"]

    integer(int32), parameter :: max_iterations = 1000000
    real(real64), parameter :: tolerance = 1.0d-14
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
    write (unit, '(a)') "Dense Matrix Testing:"
    call run_test_solver_dense()
    write (unit, '(a)') ""
    write (unit, '(a)') "COO Matrix Testing:"
    call run_test_solver_coo()
    write (unit, '(a)') ""
    write (unit, '(a)') "CSR Matrix Testing:"
    call run_test_solver_csr()
    write (unit, '(a)') ""
    write (unit, '(a)') "BSR Matrix Testing:"
    call run_test_solver_bsr()
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

        integer(int32) :: n
        real(real64) :: h
        real(real64), allocatable :: x_exact(:)
        integer(int32) :: i, is, ip, im
        real(real64) :: xi
        real(real64), dimension(:), pointer :: x_ptr

        type(type_solver_settings) :: matrix_info
        type(type_preconditioner_settings) :: pc_info

        do is = 1, num_solvers
            do ip = 1, num_preconditioners
                do im = 1, num_matrix_sizes
                    n = matrix_size(im)
                    write (unit, '(5a,i0)') " - Testing solver: ", trim(solver_name_lists(is)), &
                        " with pc: ", trim(pc_name_lists(ip)), " and matrix size: ", n
                    select case (solver_id_lists(is))
                    case (SOLVER_BICGSTAB)
                        call matrix_info%set(solver_id_lists(is), n, tolerance, max_iterations)
                    case (SOLVER_GMRES_M)
                        call matrix_info%set(solver_id_lists(is), n, tolerance, max_iterations, n)
                    end select
                    select case (pc_id_lists(ip))
                    case (SOLVER_PRECONDITION_NONE)
                        call pc_info%set(pc_id_lists(ip))
                    case (SOLVER_PRECONDITION_JACOBI)
                        call pc_info%set(pc_id_lists(ip), n)
                    case (SOLVER_PRECONDITION_ILU)
                        call pc_info%set(pc_id_lists(ip))
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
                    call allocate_array(x_exact, n)

                    ! Set up the matrix A (1D Poisson problem)
                    h = 1.0d0 / dble(n + 1)
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
                    ! call solver%display_rhistory(unit)

                    ! Output the results
                    x_ptr => x%get_data()
                    if (any(abs(x_ptr - x_exact) > 1.0d-10)) then
                        write (unit, '(3a)') "  [FAIL]: ", trim(solver_name_lists(is)), " methods failed in dense matrix."
                    else
                        write (unit, '(3a)') "  [PASS]: ", trim(solver_name_lists(is)), " methods succeeded in dense matrix."
                    end if

                    call A%destroy()
                    call b%destroy()
                    call x%destroy()
                    call solver%destroy()
                    call deallocate_array(x_exact)
                end do
            end do
        end do

    end subroutine run_test_solver_dense

    subroutine run_test_solver_coo()
        implicit none
        type(type_matrix_coo) :: A ! COO format
        type(type_vector_dp) :: b, x

        class(abst_solver), allocatable :: solver
        integer(int32) :: ierr

        integer(int32) :: n
        real(real64) :: h
        real(real64), allocatable :: x_exact(:)
        integer(int32) :: i, is, ip, im
        real(real64) :: xi
        real(real64), dimension(:), pointer :: x_ptr

        type(type_solver_settings) :: matrix_info
        type(type_preconditioner_settings) :: pc_info

        ! Work arrays for COO initialization
        ! row_idx: row indices (size nnz)
        ! col_idx: column indices (size nnz)
        integer(int32), allocatable :: row_idx(:), col_idx(:)
        integer(int32) :: nnz_est, k

        do is = 1, num_solvers
            do ip = 1, num_preconditioners
                do im = 1, num_matrix_sizes
                    n = matrix_size(im)
                    write (unit, '(5a,i0)') " - Testing solver: ", trim(solver_name_lists(is)), &
                        " with pc: ", trim(pc_name_lists(ip)), " and matrix size: ", n
                    select case (solver_id_lists(is))
                    case (SOLVER_BICGSTAB)
                        call matrix_info%set(solver_id_lists(is), n, tolerance, max_iterations)
                    case (SOLVER_GMRES_M)
                        call matrix_info%set(solver_id_lists(is), n, tolerance, max_iterations, n)
                    end select
                    select case (pc_id_lists(ip))
                    case (SOLVER_PRECONDITION_NONE)
                        call pc_info%set(pc_id_lists(ip))
                    case (SOLVER_PRECONDITION_JACOBI)
                        call pc_info%set(pc_id_lists(ip), n)
                    case (SOLVER_PRECONDITION_ILU)
                        call pc_info%set(pc_id_lists(ip))
                    end select
                    call create_solver(solver, matrix_info, pc_info, ierr)
                    if (ierr /= SOLVER_STATUS_SUCCESS) then
                        write (unit, '(a,i0)') "Error in creating solver: ", ierr
                        return
                    end if

                    ! --- Build sparsity pattern (COO format) ---
                    ! 1D Poisson (tridiagonal): nnz ~ 3*N - 2
                    nnz_est = 3 * n - 2

                    ! COO requires both row_idx and col_idx of size nnz
                    call allocate_array(row_idx, nnz_est)
                    call allocate_array(col_idx, nnz_est)

                    k = 1
                    do i = 1, n
                        ! 1. Lower neighbor (i, i-1)
                        if (i > 1) then
                            row_idx(k) = i
                            col_idx(k) = i - 1
                            k = k + 1
                        end if

                        ! 2. Diagonal (i, i)
                        row_idx(k) = i
                        col_idx(k) = i
                        k = k + 1

                        ! 3. Upper neighbor (i, i+1)
                        if (i < n) then
                            row_idx(k) = i
                            col_idx(k) = i + 1
                            k = k + 1
                        end if
                    end do

                    ! Initialize COO matrix with sparsity pattern
                    call A%initialize(n, row_idx, col_idx)

                    ! Free work arrays
                    call deallocate_array(row_idx)
                    call deallocate_array(col_idx)
                    ! ----------------------------------------------

                    call b%initialize(n)
                    call x%initialize(n)
                    call A%zero()
                    call b%zero()
                    call x%zero()
                    call allocate_array(x_exact, n)

                    ! Set up the matrix A (1D Poisson problem)
                    h = 1.0d0 / dble(n + 1)

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
                    ! call solver%display_rhistory(unit)

                    ! Output the results
                    x_ptr => x%get_data()
                    if (any(abs(x_ptr - x_exact) > 1.0d-10)) then
                        write (unit, '(3a)') "  [FAIL]: ", trim(solver_name_lists(is)), " methods failed in COO matrix."
                    else
                        write (unit, '(3a)') "  [PASS]: ", trim(solver_name_lists(is)), " methods succeeded in COO matrix."
                    end if

                    call A%destroy()
                    call b%destroy()
                    call x%destroy()
                    call solver%destroy()
                    call deallocate_array(x_exact)
                end do
            end do
        end do

    end subroutine run_test_solver_coo

    subroutine run_test_solver_csr()
        implicit none
        type(type_matrix_csr) :: A ! CSR format
        type(type_vector_dp) :: b, x

        class(abst_solver), allocatable :: solver
        integer(int32) :: ierr

        integer(int32) :: n
        real(real64) :: h
        real(real64), allocatable :: x_exact(:)
        integer(int32) :: i, is, ip, im
        real(real64) :: xi
        real(real64), dimension(:), pointer :: x_ptr

        type(type_solver_settings) :: matrix_info
        type(type_preconditioner_settings) :: pc_info

        ! Work arrays for CSR initialization
        integer(int32), allocatable :: row_ptr(:), col_idx(:)
        integer(int32) :: nnz_est, k

        do is = 1, num_solvers
            do ip = 1, num_preconditioners
                do im = 1, num_matrix_sizes
                    n = matrix_size(im)
                    write (unit, '(5a,i0)') " - Testing solver: ", trim(solver_name_lists(is)), &
                        " with pc: ", trim(pc_name_lists(ip)), " and matrix size: ", n
                    select case (solver_id_lists(is))
                    case (SOLVER_BICGSTAB)
                        call matrix_info%set(solver_id_lists(is), n, tolerance, max_iterations)
                    case (SOLVER_GMRES_M)
                        call matrix_info%set(solver_id_lists(is), n, tolerance, max_iterations, n)
                    end select
                    select case (pc_id_lists(ip))
                    case (SOLVER_PRECONDITION_NONE)
                        call pc_info%set(pc_id_lists(ip))
                    case (SOLVER_PRECONDITION_JACOBI)
                        call pc_info%set(pc_id_lists(ip), n)
                    case (SOLVER_PRECONDITION_ILU)
                        call pc_info%set(pc_id_lists(ip))
                    end select
                    call create_solver(solver, matrix_info, pc_info, ierr)
                    if (ierr /= SOLVER_STATUS_SUCCESS) then
                        write (unit, '(a,i0)') "Error in creating solver: ", ierr
                        return
                    end if

                    ! --- Build sparsity pattern (CSR format) ---
                    ! 1D Poisson (tridiagonal): nnz ~ 3*N
                    nnz_est = 3 * n - 2

                    ! Allocate arrays (using row_ptr instead of row_idx)
                    call allocate_array(row_ptr, n + 1)
                    call allocate_array(col_idx, nnz_est)

                    row_ptr(1) = 1
                    k = 1

                    do i = 1, n
                        ! 1. Lower neighbor (i, i-1)
                        if (i > 1) then
                            col_idx(k) = i - 1
                            k = k + 1
                        end if

                        ! 2. Diagonal (i, i)
                        col_idx(k) = i
                        k = k + 1

                        ! 3. Upper neighbor (i, i+1)
                        if (i < n) then
                            col_idx(k) = i + 1
                            k = k + 1
                        end if

                        ! Next row starts at current counter value
                        row_ptr(i + 1) = k
                    end do
                    ! Initialize CSR matrix with sparsity pattern
                    call A%initialize(n, row_ptr, col_idx)

                    ! Free work arrays
                    call deallocate_array(row_ptr)
                    call deallocate_array(col_idx)
                    ! ----------------------------------------------

                    call b%initialize(n)
                    call x%initialize(n)
                    call A%zero()
                    call b%zero()
                    call x%zero()
                    call allocate_array(x_exact, n)

                    ! Set up the matrix A (1D Poisson problem)
                    h = 1.0d0 / dble(n + 1)

                    do i = 1, n
                        ! Structure is already defined; set values same as Dense
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
                    ! call solver%display_rhistory(unit)

                    ! Output the results
                    x_ptr => x%get_data()
                    if (any(abs(x_ptr - x_exact) > 1.0d-10)) then
                        write (unit, '(3a)') "  [FAIL]: ", trim(solver_name_lists(is)), " methods failed in CSR matrix."
                    else
                        write (unit, '(3a)') "  [PASS]: ", trim(solver_name_lists(is)), " methods succeeded in CSR matrix."
                    end if

                    call A%destroy()
                    call b%destroy()
                    call x%destroy()
                    call solver%destroy()
                    call deallocate_array(x_exact)
                end do
            end do
        end do

    end subroutine run_test_solver_csr

    subroutine run_test_solver_bsr()
        implicit none
        type(type_matrix_bsr) :: A ! BSR format
        type(type_vector_dp) :: b, x

        class(abst_solver), allocatable :: solver
        integer(int32) :: ierr

        integer(int32) :: n ! Number of nodes
        integer(int32) :: n_dof ! Total DOFs (n * nb)
        integer(int32) :: nb ! Block size
        real(real64) :: h
        real(real64), allocatable :: x_exact(:)
        integer(int32) :: i, is, ip, im, k
        integer(int32) :: idx
        real(real64) :: xi, exact_u, exact_v
        real(real64), dimension(:), pointer :: x_ptr

        type(type_solver_settings) :: matrix_info
        type(type_preconditioner_settings) :: pc_info

        ! Work arrays for BSR pattern (same structure as CSR: row_ptr, col_idx)
        integer(int32), allocatable :: row_ptr(:), col_idx(:)
        integer(int32) :: nnz_blocks_est

        ! Block size (e.g., 2-variable problem)
        nb = 2

        do is = 1, num_solvers
            do ip = 1, num_preconditioners
                do im = 1, num_matrix_sizes
                    n = matrix_size(im)
                    n_dof = n * nb

                    write (unit, '(5a,i0,a,i0,a)') " - Testing solver: ", trim(solver_name_lists(is)), &
                        " with pc: ", trim(pc_name_lists(ip)), &
                        " (Nodes: ", n, ", BlockSize: ", nb, ")"

                    ! Pass total DOF count (n_dof) to solver
                    select case (solver_id_lists(is))
                    case (SOLVER_BICGSTAB)
                        call matrix_info%set(solver_id_lists(is), n_dof, tolerance, max_iterations)
                    case (SOLVER_GMRES_M)
                        call matrix_info%set(solver_id_lists(is), n_dof, tolerance, max_iterations, n_dof)
                    end select

                    select case (pc_id_lists(ip))
                    case (SOLVER_PRECONDITION_NONE)
                        call pc_info%set(pc_id_lists(ip))
                    case (SOLVER_PRECONDITION_JACOBI)
                        call pc_info%set(pc_id_lists(ip), n_dof)
                    case (SOLVER_PRECONDITION_ILU)
                        call pc_info%set(pc_id_lists(ip))
                    end select
                    call create_solver(solver, matrix_info, pc_info, ierr)
                    if (ierr /= SOLVER_STATUS_SUCCESS) return

                    ! --- Build sparsity pattern (same logic as CSR) ---
                    ! Define block-level connectivity
                    nnz_blocks_est = 3 * n - 2
                    call allocate_array(row_ptr, n + 1)
                    call allocate_array(col_idx, nnz_blocks_est)

                    row_ptr(1) = 1
                    k = 1

                    do i = 1, n
                        ! 1. Left neighbor block (i, i-1)
                        if (i > 1) then
                            col_idx(k) = i - 1
                            k = k + 1
                        end if

                        ! 2. Diagonal block (i, i)
                        col_idx(k) = i
                        k = k + 1

                        ! 3. Right neighbor block (i, i+1)
                        if (i < n) then
                            col_idx(k) = i + 1
                            k = k + 1
                        end if

                        row_ptr(i + 1) = k
                    end do

                    ! Initialize BSR matrix
                    ! Args: (num_nodes, row_ptr, col_idx, row_block_size, col_block_size)
                    ! Internally allocates val(:, :, :)
                    call A%initialize(n, row_ptr, col_idx, nb, nb)

                    call deallocate_array(row_ptr)
                    call deallocate_array(col_idx)
                    ! -----------------------------------------------------------

                    call b%initialize(n_dof)
                    call x%initialize(n_dof)
                    call A%zero()
                    call b%zero()
                    call x%zero()
                    call allocate_array(x_exact, n_dof)

                    h = 1.0d0 / dble(n + 1)

                    ! Assemble matrix and vectors
                    do i = 1, n
                        ! --- Set block matrix values ---
                        ! set_value_block(op, node_row, node_col, local_row, local_col, val)

                        ! Diagonal block (i, i) -> [[2, 0], [0, 2]]
                        call A%set_value_block(OP_ADD, i, i, 1, 1, 2.0d0)
                        call A%set_value_block(OP_ADD, i, i, 2, 2, 2.0d0)

                        ! Left neighbor block (i, i-1) -> [[-1, 0], [0, -1]]
                        if (i > 1) then
                            call A%set_value_block(OP_ADD, i, i - 1, 1, 1, -1.0d0)
                            call A%set_value_block(OP_ADD, i, i - 1, 2, 2, -1.0d0)
                        end if

                        ! Right neighbor block (i, i+1) -> [[-1, 0], [0, -1]]
                        if (i < n) then
                            call A%set_value_block(OP_ADD, i, i + 1, 1, 1, -1.0d0)
                            call A%set_value_block(OP_ADD, i, i + 1, 2, 2, -1.0d0)
                        end if

                        ! --- Vector setup (compute global indices) ---
                        xi = dble(i) * h

                        ! Variable 1 (u): -u'' = h^2
                        idx = (i - 1) * nb + 1
                        call b%set(OP_INS, idx, h * h)
                        x_exact(idx) = 0.5d0 * xi * (1.0d0 - xi)

                        ! Variable 2 (v): -v'' = 2h^2
                        idx = (i - 1) * nb + 2
                        call b%set(OP_INS, idx, 2.0d0 * h * h)
                        x_exact(idx) = 1.0d0 * xi * (1.0d0 - xi)
                    end do

                    ! Solve Ax = b
                    call solver%solve(A, b, x)
                    call solver%check()

                    ! Check results
                    x_ptr => x%get_data()
                    if (any(abs(x_ptr - x_exact) > 1.0d-10)) then
                        write (unit, '(3a)') "  [FAIL]: ", trim(solver_name_lists(is)), " methods failed in BSR matrix."
                    else
                        write (unit, '(3a)') "  [PASS]: ", trim(solver_name_lists(is)), " methods succeeded in BSR matrix."
                    end if

                    call A%destroy()
                    call b%destroy()
                    call x%destroy()
                    call solver%destroy()
                    call deallocate_array(x_exact)
                end do
            end do
        end do

    end subroutine run_test_solver_bsr

end program test_solver
