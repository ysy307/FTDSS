submodule(numerical_solver_interface) impl_solve_type_solver_pardiso
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use, intrinsic :: iso_c_binding, only: c_int, c_double
    use, intrinsic :: ieee_arithmetic
#ifdef _MKL
    use :: linalg_mkl_interface, only: pardiso, pardisoinit
#endif
    implicit none
contains

    module subroutine initialize_type_solver_pardiso(self, solver_settings, preconditioner_settings)
        implicit none
        class(type_solver_pardiso), intent(inout) :: self
        type(type_solver_settings), intent(in) :: solver_settings
        type(type_preconditioner_settings), intent(in) :: preconditioner_settings

        self%ID = solver_settings%ID
        self%name = "PARDISO"
        self%num_nodes = solver_settings%num_nodes
        self%tolerance = solver_settings%tolerance
        self%relative_tolerance = solver_settings%relative_tolerance
        self%max_iterations = solver_settings%max_iterations
        self%projection_enabled = .false.
        self%projection_offset = 0
        self%projection_stride = 0

        call self%residual_history%initialize(1)
        self%current_iteration = 0

        self%maxfct = 1
        self%mnum   = 1
        self%nrhs   = 1
        self%msglvl = 0
        self%last_error = 0
        self%status = SOLVER_STATUS%SUCCESS%ID
        self%mtype = 11

#ifdef _MKL
        self%pt(:)%dummy = 0
        self%iparm = 0
        call pardisoinit(self%pt, int(self%mtype, c_int), self%iparm)
        self%iparm(1)  = 1
        self%iparm(2)  = 2
        self%iparm(5)  = 0
        self%iparm(7)  = 2
        self%iparm(10) = 8
        self%iparm(11) = 1
        self%iparm(13) = 1
        self%iparm(34) = 0
#else
        self%status = SOLVER_STATUS%NOT_IMPLEMENTED%ID
#endif

    end subroutine initialize_type_solver_pardiso

    module subroutine solve_type_solver_pardiso(self, A, b, x)
        implicit none
        class(type_solver_pardiso), intent(inout) :: self
        class(abst_matrix), intent(in) :: A
        type(type_vector_dp), intent(in) :: b
        type(type_vector_dp), intent(inout) :: x

#ifdef _MKL
        type(type_matrix_info) :: info
        integer(c_int) :: n
        integer(c_int) :: phase
        integer(c_int) :: maxfct
        integer(c_int) :: mnum
        integer(c_int) :: mtype
        integer(c_int) :: nrhs
        integer(c_int) :: msglvl
        integer(c_int) :: error

        integer(int32), pointer :: ia_src(:) => null()
        integer(int32), pointer :: ja_src(:) => null()
        real(real64), pointer :: a_src(:) => null()
        integer(int32), pointer :: ptr_src(:) => null()
        integer(int32), pointer :: ind_src(:) => null()
        real(real64), pointer :: val_blk(:, :, :) => null()
        real(real64), pointer :: b_src(:) => null()
        real(real64), pointer :: x_dst(:) => null()

        integer(c_int), allocatable :: ia(:)
        integer(c_int), allocatable :: ja(:)
        integer(c_int), allocatable :: perm(:)
        real(c_double), allocatable :: a_val(:)
        real(c_double), allocatable :: b_val(:)
        real(c_double), allocatable :: x_val(:)

        integer(int32) :: i, j, k, r, c, idx, row, col, bs_row, bs_col, nnz_total, n_rows
        integer(c_int) :: tmp_col
        real(c_double) :: tmp_val
        integer(int32) :: row_start, row_end

        self%status = SOLVER_STATUS%SUCCESS%ID
        self%last_error = 0

        maxfct = int(self%maxfct, c_int)
        mnum   = int(self%mnum,   c_int)
        mtype  = 11
        nrhs   = int(self%nrhs,   c_int)
        msglvl = 1 ! Enable logging to diagnose solve-phase crash

        select type (A)
        type is (type_matrix_csr)
            ! (Already handled)
        type is (type_matrix_bsr)
            ! (Already handled, but ensure b_val and x_val are zero-initialized)
            b_val = real(b_src, c_double)
            x_val = 0.0d0
            perm  = 0
        end select

        ! Phase 11: Analysis
        phase = 11
        call pardiso(self%pt, maxfct, mnum, mtype, phase, n, a_val, ia, ja, perm, nrhs, &
                     self%iparm, msglvl, b_val, x_val, error)
        if (error /= 0) then
            write (*, '(A,I0)') '[ERR-PARDISO] Phase 11 failed with error=', error
            self%status = SOLVER_STATUS%BREAKDOWN%ID
            self%last_error = int(error, int32)
            return
        end if

        ! Phase 22: Numerical factorization
        phase = 22
        call pardiso(self%pt, maxfct, mnum, mtype, phase, n, a_val, ia, ja, perm, nrhs, &
                     self%iparm, msglvl, b_val, x_val, error)
        if (error /= 0) then
            write (*, '(A,I0)') '[ERR-PARDISO] Phase 22 failed with error=', error
            self%status = SOLVER_STATUS%BREAKDOWN%ID
            self%last_error = int(error, int32)
            return
        end if

        ! Phase 33: Solve
        write (*, '(A)') '[DBG-PARDISO] entering phase 33'
        phase = 33
        call pardiso(self%pt, maxfct, mnum, mtype, phase, n, a_val, ia, ja, perm, nrhs, &
                     self%iparm, msglvl, b_val, x_val, error)
        write (*, '(A,I0)') '[DBG-PARDISO] phase 33 finished with error=', error

        if (error /= 0) then
            self%status = SOLVER_STATUS%BREAKDOWN%ID
            self%last_error = int(error, int32)
            return
        end if

        x_dst = real(x_val, real64)
        self%current_iteration = 1

        ! Phase -1: Memory release
        phase = -1
        call pardiso(self%pt, maxfct, mnum, mtype, phase, n, a_val, ia, ja, perm, nrhs, &
                     self%iparm, msglvl, b_val, x_val, error)

        deallocate (ia, ja, a_val, b_val, x_val, perm)

#else
        self%status = SOLVER_STATUS%NOT_IMPLEMENTED%ID
#endif

    end subroutine solve_type_solver_pardiso

    module subroutine destroy_type_solver_pardiso(self)
        implicit none
        class(type_solver_pardiso), intent(inout) :: self

        if (allocated(self%name)) deallocate (self%name)
        self%ID = -1
        self%num_nodes = -1
        self%status = SOLVER_STATUS%SUCCESS%ID

    end subroutine destroy_type_solver_pardiso

end submodule impl_solve_type_solver_pardiso
