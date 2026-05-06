submodule(numerical_solver_interface) impl_solve_type_solver_pardiso
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use, intrinsic :: iso_c_binding, only: c_int, c_double
    use, intrinsic :: ieee_arithmetic
    use, intrinsic :: ieee_exceptions
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
        logical :: fpe_halt

        self%status = SOLVER_STATUS%SUCCESS%ID
        self%last_error = 0

        maxfct = int(self%maxfct, c_int)
        mnum   = int(self%mnum,   c_int)
        mtype  = int(self%mtype,  c_int)
        nrhs   = int(self%nrhs,   c_int)
        msglvl = int(self%msglvl, c_int)

        select type (A)
        type is (type_matrix_csr)
            call A%get_info(info)
            n = int(info%num_rows, c_int)

            ia_src => A%get_ptr()
            ja_src => A%get_ind()
            a_src  => A%get_val()
            b_src  => b%get_data()
            x_dst  => x%get_data()

            if (.not. associated(ia_src) .or. .not. associated(ja_src) .or. .not. associated(a_src)) then
                self%status = SOLVER_STATUS%ILL_OPTIONS%ID
                return
            end if
            if (.not. associated(b_src) .or. .not. associated(x_dst)) then
                self%status = SOLVER_STATUS%ILL_OPTIONS%ID
                return
            end if
            if (size(b_src) /= size(x_dst) .or. size(b_src) /= int(n, int32)) then
                self%status = SOLVER_STATUS%ILL_OPTIONS%ID
                return
            end if

            allocate (ia(size(ia_src)))
            allocate (ja(size(ja_src)))
            allocate (a_val(size(a_src)))
            allocate (b_val(size(b_src)))
            allocate (x_val(size(x_dst)))
            allocate (perm(size(x_dst)))

            ia    = int(ia_src, c_int)
            ja    = int(ja_src, c_int)
            a_val = real(a_src, c_double)
            b_val = real(b_src, c_double)
            x_val = 0.0d0
            perm  = 0

            if (any(.not. ieee_is_finite(a_val))) then
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                deallocate (ia, ja, a_val, b_val, x_val, perm)
                return
            end if
            if (any(.not. ieee_is_finite(b_val))) then
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                deallocate (ia, ja, a_val, b_val, x_val, perm)
                return
            end if

            ! MKL PARDISO generates FP_INVALID internally (documented behavior).
            ! Save and disable to prevent spurious SIGFPE from -init=snan.
            call ieee_get_halting_mode(ieee_invalid, fpe_halt)
            call ieee_set_halting_mode(ieee_invalid, .false.)

            phase = 11
            call pardiso(self%pt, maxfct, mnum, mtype, phase, n, a_val, ia, ja, perm, nrhs, &
                         self%iparm, msglvl, b_val, x_val, error)
            self%last_error = int(error, int32)
            if (error /= 0) then
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                call ieee_set_halting_mode(ieee_invalid, fpe_halt)
                deallocate (ia, ja, a_val, b_val, x_val, perm)
                return
            end if

            phase = 22
            call pardiso(self%pt, maxfct, mnum, mtype, phase, n, a_val, ia, ja, perm, nrhs, &
                         self%iparm, msglvl, b_val, x_val, error)
            self%last_error = int(error, int32)
            if (error /= 0) then
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                call ieee_set_halting_mode(ieee_invalid, fpe_halt)
                deallocate (ia, ja, a_val, b_val, x_val, perm)
                return
            end if

            phase = 33
            call pardiso(self%pt, maxfct, mnum, mtype, phase, n, a_val, ia, ja, perm, nrhs, &
                         self%iparm, msglvl, b_val, x_val, error)
            self%last_error = int(error, int32)
            if (error /= 0) then
                self%status = SOLVER_STATUS%BREAKDOWN%ID
            else
                x_dst = real(x_val, real64)
                self%current_iteration = 1
            end if

            call ieee_set_halting_mode(ieee_invalid, fpe_halt)
            deallocate (ia, ja, a_val, b_val, x_val, perm)

        type is (type_matrix_bsr)
            block
                integer(int32) :: bs_row, bs_col, n_rows
                integer(int32) :: i, j, k, r, c
                integer(int32) :: row, col
                integer(int32) :: blocks_in_row
                integer(int32) :: nnz_total
                integer(int32) :: idx
                integer(int32) :: row_start, row_end
                integer(c_int) :: tmp_col
                real(c_double) :: tmp_val

                call A%get_info(info)
                bs_row = info%num_block_rows
                bs_col = info%num_block_cols
                n_rows = info%num_nodes * bs_row
                n      = int(n_rows, c_int)

                ptr_src => A%get_ptr()
                ind_src => A%get_ind()
                val_blk => A%get_val()
                b_src   => b%get_data()
                x_dst   => x%get_data()

                if (.not. associated(ptr_src) .or. .not. associated(ind_src) .or. .not. associated(val_blk)) then
                    self%status = SOLVER_STATUS%ILL_OPTIONS%ID
                    return
                end if
                if (.not. associated(b_src) .or. .not. associated(x_dst)) then
                    self%status = SOLVER_STATUS%ILL_OPTIONS%ID
                    return
                end if
                if (size(b_src) /= size(x_dst) .or. size(b_src) /= n_rows) then
                    self%status = SOLVER_STATUS%ILL_OPTIONS%ID
                    return
                end if

                nnz_total = 0
                do i = 1, info%num_nodes
                    blocks_in_row = ptr_src(i + 1) - ptr_src(i)
                    nnz_total = nnz_total + blocks_in_row * bs_row * bs_col
                end do

                allocate (ia(n_rows + 1))
                allocate (ja(nnz_total))
                allocate (a_val(nnz_total))
                allocate (b_val(size(b_src)))
                allocate (x_val(size(x_dst)))
                allocate (perm(size(x_dst)))

                ia(1) = 1
                do i = 1, info%num_nodes
                    blocks_in_row = ptr_src(i + 1) - ptr_src(i)
                    do r = 1, bs_row
                        row = (i - 1) * bs_row + r
                        ia(row + 1) = ia(row) + blocks_in_row * bs_col
                    end do
                end do

                idx = 1
                do i = 1, info%num_nodes
                    do r = 1, bs_row
                        do k = ptr_src(i), ptr_src(i + 1) - 1
                            col = ind_src(k)
                            do c = 1, bs_col
                                ja(idx)    = int((col - 1) * bs_col + c, c_int)
                                a_val(idx) = real(val_blk(r, c, k), c_double)
                                idx = idx + 1
                            end do
                        end do
                    end do
                end do

                ! Sort ja within each CSR row (insertion sort) and track a_val
                do i = 1, n_rows
                    row_start = ia(i)
                    row_end   = ia(i + 1) - 1
                    do j = row_start + 1, row_end
                        tmp_col = ja(j)
                        tmp_val = a_val(j)
                        k = j - 1
                        do while (k >= row_start .and. ja(k) > tmp_col)
                            ja(k + 1)    = ja(k)
                            a_val(k + 1) = a_val(k)
                            k = k - 1
                        end do
                        ja(k + 1)    = tmp_col
                        a_val(k + 1) = tmp_val
                    end do
                end do

                ! Validate CSR structure
                do i = 1, n_rows
                    if (ia(i + 1) < ia(i)) then
                        self%status = SOLVER_STATUS%ILL_OPTIONS%ID
                        deallocate (ia, ja, a_val, b_val, x_val, perm)
                        return
                    end if
                end do
                do i = 1, nnz_total
                    if (ja(i) < 1 .or. ja(i) > n) then
                        self%status = SOLVER_STATUS%ILL_OPTIONS%ID
                        deallocate (ia, ja, a_val, b_val, x_val, perm)
                        return
                    end if
                end do

                b_val = real(b_src, c_double)
                x_val = 0.0d0
                perm  = 0

                if (any(.not. ieee_is_finite(a_val))) then
                    self%status = SOLVER_STATUS%BREAKDOWN%ID
                    deallocate (ia, ja, a_val, b_val, x_val, perm)
                    return
                end if
                if (any(.not. ieee_is_finite(b_val))) then
                    self%status = SOLVER_STATUS%BREAKDOWN%ID
                    deallocate (ia, ja, a_val, b_val, x_val, perm)
                    return
                end if

                ! MKL PARDISO generates FP_INVALID internally (documented behavior).
                ! Save and disable to prevent spurious SIGFPE from -init=snan.
                call ieee_get_halting_mode(ieee_invalid, fpe_halt)
                call ieee_set_halting_mode(ieee_invalid, .false.)

                phase = 11
                call pardiso(self%pt, maxfct, mnum, mtype, phase, n, a_val, ia, ja, perm, nrhs, &
                             self%iparm, msglvl, b_val, x_val, error)
                self%last_error = int(error, int32)
                if (error /= 0) then
                    self%status = SOLVER_STATUS%BREAKDOWN%ID
                    call ieee_set_halting_mode(ieee_invalid, fpe_halt)
                    deallocate (ia, ja, a_val, b_val, x_val, perm)
                    return
                end if

                phase = 22
                call pardiso(self%pt, maxfct, mnum, mtype, phase, n, a_val, ia, ja, perm, nrhs, &
                             self%iparm, msglvl, b_val, x_val, error)
                self%last_error = int(error, int32)
                if (error /= 0) then
                    self%status = SOLVER_STATUS%BREAKDOWN%ID
                    call ieee_set_halting_mode(ieee_invalid, fpe_halt)
                    deallocate (ia, ja, a_val, b_val, x_val, perm)
                    return
                end if

                phase = 33
                call pardiso(self%pt, maxfct, mnum, mtype, phase, n, a_val, ia, ja, perm, nrhs, &
                             self%iparm, msglvl, b_val, x_val, error)
                self%last_error = int(error, int32)
                if (error /= 0) then
                    self%status = SOLVER_STATUS%BREAKDOWN%ID
                else
                    x_dst = real(x_val, real64)
                    self%current_iteration = 1
                end if

                call ieee_set_halting_mode(ieee_invalid, fpe_halt)
                deallocate (ia, ja, a_val, b_val, x_val, perm)
            end block

        class default
            self%status = SOLVER_STATUS%NOT_IMPLEMENTED%ID
        end select
#else
        self%status = SOLVER_STATUS%NOT_IMPLEMENTED%ID
#endif

    end subroutine solve_type_solver_pardiso

    module subroutine destroy_type_solver_pardiso(self)
        implicit none
        class(type_solver_pardiso), intent(inout) :: self
#ifdef _MKL
        integer(c_int) :: phase, maxfct, mnum, mtype, nrhs, msglvl, error, n
        real(c_double) :: dummy_a(1) = 0.0d0
        real(c_double) :: dummy_b(1) = 0.0d0
        real(c_double) :: dummy_x(1) = 0.0d0
        integer(c_int) :: dummy_ia(2) = [1, 1]
        integer(c_int) :: dummy_ja(1) = 0
        integer(c_int) :: dummy_perm(1) = 0
        logical :: fpe_halt

        call ieee_get_halting_mode(ieee_invalid, fpe_halt)
        call ieee_set_halting_mode(ieee_invalid, .false.)

        phase  = -1
        maxfct = int(self%maxfct, c_int)
        mnum   = int(self%mnum,   c_int)
        mtype  = int(self%mtype,  c_int)
        nrhs   = 1
        msglvl = 0
        error  = 0
        n      = 1
        call pardiso(self%pt, maxfct, mnum, mtype, phase, n, dummy_a, dummy_ia, dummy_ja, &
                     dummy_perm, nrhs, self%iparm, msglvl, dummy_b, dummy_x, error)

        call ieee_set_halting_mode(ieee_invalid, fpe_halt)
#endif
        if (allocated(self%name)) deallocate (self%name)
        self%ID = -1
        self%num_nodes = -1
        self%status = SOLVER_STATUS%SUCCESS%ID

    end subroutine destroy_type_solver_pardiso

end submodule impl_solve_type_solver_pardiso
