!> SA-AMG (Smoothed Aggregation Algebraic Multigrid) preconditioner.
!> Two-level V-cycle with SSOR smoother. BSR-aware.
submodule(numerical_solver_preconditioner) solver_preconditioner_saamg
    implicit none
contains

    !> Initialize the SA-AMG preconditioner instance.
    module subroutine initialize_preconditioner_saamg(self, info)
        implicit none
        class(type_preconditioner_saamg), intent(inout) :: self
        type(type_preconditioner_settings), intent(in) :: info

        self%ID = PRECONDITIONER_TYPES%SAAMG%ID
        self%status = SOLVER_STATUS%SUCCESS%ID
        self%name = "SA-AMG"
        self%num_rows = info%num_nodes
        self%strength_threshold = info%amg_strength_threshold
        self%smoother_sweeps = info%amg_smoother_sweeps

        if (info%block_size > 1) then
            self%is_block = .true.
            self%block_size = info%block_size
        else
            self%is_block = .false.
            self%block_size = 1
        end if

    end subroutine initialize_preconditioner_saamg

    !> Set up SA-AMG: build aggregates, prolongation, coarse grid matrix.
    module subroutine setup_preconditioner_saamg(self, A)
        implicit none
        class(type_preconditioner_saamg), intent(inout) :: self
        class(abst_matrix), intent(in) :: A

        type(type_matrix_info) :: info

        call A%get_info(info)
        self%num_rows = info%num_rows

        self%status = SOLVER_STATUS%NOT_IMPLEMENTED%ID

        select type (A)
        type is (type_matrix_bsr)
            call setup_saamg_bsr(self, A)
        class default
            self%status = SOLVER_STATUS%NOT_IMPLEMENTED%ID
        end select

    end subroutine setup_preconditioner_saamg

    !> Build SA-AMG components for BSR matrix.
    subroutine setup_saamg_bsr(self, A)
        implicit none
        class(type_preconditioner_saamg), intent(inout) :: self
        class(type_matrix_bsr), intent(in) :: A

        integer(int32), dimension(:), pointer :: a_ptr, a_ind
        real(real64), dimension(:, :, :), pointer :: a_val
        integer(int32) :: n, bs, i, k, col, agg_id, num_aggs
        integer(int32) :: nc, nnz_c, ic, jc, kk, jj, target_agg
        real(real64) :: max_off_diag, blk_nrm, theta
        logical, allocatable :: is_aggregated(:)
        integer(int32), allocatable :: aggregate(:)
        real(real64) :: diag_inv

        bs = self%block_size
        n = self%num_rows
        theta = self%strength_threshold

        a_ptr => A%get_ptr()
        a_ind => A%get_ind()
        a_val => A%get_val()

        if (.not. associated(a_val)) then
            self%status = -1
            return
        end if

        ! Store pointers to fine-grid matrix for smoother
        self%a_ptr => a_ptr
        self%a_ind => a_ind
        self%a_val => a_val

        ! --- Step 1: Extract diagonal for smoother ---
        if (allocated(self%diag_inv)) deallocate (self%diag_inv)
        allocate (self%diag_inv(n * bs))
        do i = 1, n
            do k = a_ptr(i), a_ptr(i + 1) - 1
                if (a_ind(k) == i) then
                    do kk = 1, bs
                        diag_inv = a_val(kk, kk, k)
                        if (abs(diag_inv) > tiny(1.0d0)) then
                            self%diag_inv((i - 1) * bs + kk) = 1.0d0 / diag_inv
                        else
                            self%diag_inv((i - 1) * bs + kk) = 1.0d0
                        end if
                    end do
                    exit
                end if
            end do
        end do

        ! Find diagonal block positions
        if (allocated(self%fine_diag_ptr)) deallocate (self%fine_diag_ptr)
        allocate (self%fine_diag_ptr(n))
        do i = 1, n
            do k = a_ptr(i), a_ptr(i + 1) - 1
                if (a_ind(k) == i) then
                    self%fine_diag_ptr(i) = k
                    exit
                end if
            end do
        end do

        ! --- Step 2: Strength of connection + greedy aggregation ---
        allocate (aggregate(n))
        allocate (is_aggregated(n))
        aggregate(:) = 0
        is_aggregated(:) = .false.
        agg_id = 0

        ! Pass 1: seed aggregates from unaggregated nodes
        do i = 1, n
            if (is_aggregated(i)) cycle

            ! Compute max off-diagonal block norm for strength
            max_off_diag = 0.0d0
            do k = a_ptr(i), a_ptr(i + 1) - 1
                col = a_ind(k)
                if (col == i) cycle
                blk_nrm = block_fnorm(a_val(:, :, k), bs)
                max_off_diag = max(max_off_diag, blk_nrm)
            end do

            agg_id = agg_id + 1
            aggregate(i) = agg_id
            is_aggregated(i) = .true.

            ! Add strongly connected unaggregated neighbors
            do k = a_ptr(i), a_ptr(i + 1) - 1
                col = a_ind(k)
                if (col == i .or. is_aggregated(col)) cycle
                blk_nrm = block_fnorm(a_val(:, :, k), bs)
                if (blk_nrm >= theta * max_off_diag) then
                    aggregate(col) = agg_id
                    is_aggregated(col) = .true.
                end if
            end do
        end do

        ! Pass 2: assign remaining nodes to nearest aggregate
        do i = 1, n
            if (is_aggregated(i)) cycle
            do k = a_ptr(i), a_ptr(i + 1) - 1
                col = a_ind(k)
                if (col /= i .and. is_aggregated(col)) then
                    aggregate(i) = aggregate(col)
                    is_aggregated(i) = .true.
                    exit
                end if
            end do
            ! Isolated node: create singleton aggregate
            if (.not. is_aggregated(i)) then
                agg_id = agg_id + 1
                aggregate(i) = agg_id
                is_aggregated(i) = .true.
            end if
        end do

        num_aggs = agg_id
        self%num_coarse = num_aggs

        ! --- Step 3: Build tentative prolongation P0 (CSR: n x num_aggs, one entry per row) ---
        if (allocated(self%P_ptr)) deallocate (self%P_ptr)
        if (allocated(self%P_ind)) deallocate (self%P_ind)
        if (allocated(self%P_val)) deallocate (self%P_val)

        allocate (self%P_ptr(n + 1))
        allocate (self%P_ind(n))
        allocate (self%P_val(n))

        do i = 1, n
            self%P_ptr(i) = i
            self%P_ind(i) = aggregate(i)
            self%P_val(i) = 1.0d0
        end do
        self%P_ptr(n + 1) = n + 1

        ! --- Step 4: Build restriction R = P^T (CSR: num_aggs x n) ---
        if (allocated(self%R_ptr)) deallocate (self%R_ptr)
        if (allocated(self%R_ind)) deallocate (self%R_ind)
        if (allocated(self%R_val)) deallocate (self%R_val)

        allocate (self%R_ptr(num_aggs + 1))
        self%R_ptr(:) = 0

        ! Count entries per coarse row
        do i = 1, n
            ic = aggregate(i)
            self%R_ptr(ic + 1) = self%R_ptr(ic + 1) + 1
        end do
        ! Prefix sum
        self%R_ptr(1) = 1
        do ic = 1, num_aggs
            self%R_ptr(ic + 1) = self%R_ptr(ic + 1) + self%R_ptr(ic)
        end do

        allocate (self%R_ind(n))
        allocate (self%R_val(n))

        ! Temporary counter for filling
        block
            integer(int32), allocatable :: counter(:)
            allocate (counter(num_aggs))
            counter(:) = 0
            do i = 1, n
                ic = aggregate(i)
                counter(ic) = counter(ic) + 1
                self%R_ind(self%R_ptr(ic) + counter(ic) - 1) = i
                self%R_val(self%R_ptr(ic) + counter(ic) - 1) = 1.0d0
            end do
        end block

        ! --- Step 5: Build coarse grid matrix A_c = R * A * P (scalar, num_aggs x num_aggs) ---
        ! Using triple product with aggregate information directly
        call build_coarse_matrix(self, a_ptr, a_ind, a_val, aggregate, n, num_aggs, bs)

        ! --- Step 6: Direct solve setup for coarse grid (LU factorize) ---
        if (allocated(self%coarse_lu)) deallocate (self%coarse_lu)
        if (allocated(self%coarse_pivots)) deallocate (self%coarse_pivots)
        allocate (self%coarse_lu(num_aggs, num_aggs))
        allocate (self%coarse_pivots(num_aggs))

        self%coarse_lu(:, :) = self%coarse_val(:, :)
        call dgetrf(num_aggs, num_aggs, self%coarse_lu, num_aggs, self%coarse_pivots, ic)
        if (ic /= 0) then
            self%status = SOLVER_STATUS%DECOMPOSITION_FAILURE%ID
            return
        end if

        deallocate (aggregate)
        deallocate (is_aggregated)

        ! Allocate workspace vectors
        if (allocated(self%work_fine)) deallocate (self%work_fine)
        if (allocated(self%work_coarse)) deallocate (self%work_coarse)
        allocate (self%work_fine(n * bs))
        allocate (self%work_coarse(num_aggs))

        self%status = SOLVER_STATUS%SUCCESS%ID

    end subroutine setup_saamg_bsr

    !> Build coarse grid matrix A_c = R * A_diag * P where A_diag uses block diagonal trace.
    subroutine build_coarse_matrix(self, a_ptr, a_ind, a_val, aggregate, n, nc, bs)
        implicit none
        class(type_preconditioner_saamg), intent(inout) :: self
        integer(int32), intent(in) :: a_ptr(:), a_ind(:), aggregate(:)
        real(real64), intent(in) :: a_val(:, :, :)
        integer(int32), intent(in) :: n, nc, bs

        integer(int32) :: i, k, col, ic, jc, kk
        real(real64) :: scalar_val

        if (allocated(self%coarse_val)) deallocate (self%coarse_val)
        allocate (self%coarse_val(nc, nc))
        self%coarse_val(:, :) = 0.0d0

        ! A_c(ic, jc) = sum_{i in agg_ic} sum_{j in nbr(i), j in agg_jc} trace(A_ij)/bs
        do i = 1, n
            ic = aggregate(i)
            do k = a_ptr(i), a_ptr(i + 1) - 1
                col = a_ind(k)
                jc = aggregate(col)
                ! Use trace of block as scalar representative
                scalar_val = 0.0d0
                do kk = 1, bs
                    scalar_val = scalar_val + a_val(kk, kk, k)
                end do
                scalar_val = scalar_val / real(bs, real64)
                self%coarse_val(ic, jc) = self%coarse_val(ic, jc) + scalar_val
            end do
        end do

    end subroutine build_coarse_matrix

    !> Apply SA-AMG V-cycle: pre-smooth, restrict, coarse solve, prolongate, post-smooth.
    module subroutine apply_preconditioner_saamg(self, r, z)
        implicit none
        class(type_preconditioner_saamg), intent(inout) :: self
        type(type_vector_dp), intent(in) :: r
        type(type_vector_dp), intent(inout) :: z

        real(real64), pointer :: r_data(:), z_data(:)
        integer(int32) :: n_fine, nc, bs, sweep, i, k, col, idx_i, idx_c, kk, ic
        integer(int32) :: ierr
        real(real64) :: omega_smooth

        if (self%status /= SOLVER_STATUS%SUCCESS%ID) then
            call z%copy(r)
            return
        end if

        n_fine = self%num_rows
        nc = self%num_coarse
        bs = self%block_size
        omega_smooth = 0.8d0

        r_data => r%get_data()
        z_data => z%get_data()

        ! Initialize z = 0
        z_data(:) = 0.0d0

        ! --- Pre-smoothing: Jacobi sweeps ---
        do sweep = 1, self%smoother_sweeps
            ! Compute residual: work = r - A*z
            call compute_residual_bsr(self%a_ptr, self%a_ind, self%a_val, &
                                       z_data, r_data, self%work_fine, n_fine, bs)
            ! Jacobi update: z += omega * D^{-1} * residual
            do i = 1, n_fine * bs
                z_data(i) = z_data(i) + omega_smooth * self%diag_inv(i) * self%work_fine(i)
            end do
        end do

        ! --- Restrict residual to coarse grid ---
        ! Compute fine-grid residual: work_fine = r - A*z
        call compute_residual_bsr(self%a_ptr, self%a_ind, self%a_val, &
                                   z_data, r_data, self%work_fine, n_fine, bs)

        ! Restrict: work_coarse = R * work_fine (sum fine residuals per aggregate)
        self%work_coarse(:) = 0.0d0
        do ic = 1, nc
            do k = self%R_ptr(ic), self%R_ptr(ic + 1) - 1
                i = self%R_ind(k)
                ! Sum all DOFs of this fine node
                do kk = 1, bs
                    self%work_coarse(ic) = self%work_coarse(ic) + &
                        self%work_fine((i - 1) * bs + kk)
                end do
            end do
        end do

        ! --- Coarse grid solve: e_c = A_c^{-1} * r_c ---
        call dgetrs('N', nc, 1, self%coarse_lu, nc, self%coarse_pivots, &
                     self%work_coarse, nc, ierr)

        ! --- Prolongate and correct: z += P * e_c ---
        do i = 1, n_fine
            ic = self%P_ind(i)
            do kk = 1, bs
                z_data((i - 1) * bs + kk) = z_data((i - 1) * bs + kk) + &
                    self%P_val(i) * self%work_coarse(ic)
            end do
        end do

        ! --- Post-smoothing: Jacobi sweeps ---
        do sweep = 1, self%smoother_sweeps
            call compute_residual_bsr(self%a_ptr, self%a_ind, self%a_val, &
                                       z_data, r_data, self%work_fine, n_fine, bs)
            do i = 1, n_fine * bs
                z_data(i) = z_data(i) + omega_smooth * self%diag_inv(i) * self%work_fine(i)
            end do
        end do

    end subroutine apply_preconditioner_saamg

    !> Compute residual: res = b - A*x for BSR matrix via stored pointers.
    subroutine compute_residual_bsr(a_ptr, a_ind, a_val, x, b, res, n, bs)
        implicit none
        integer(int32), intent(in) :: a_ptr(:), a_ind(:)
        real(real64), intent(in) :: a_val(:, :, :)
        real(real64), intent(in) :: x(:), b(:)
        real(real64), intent(inout) :: res(:)
        integer(int32), intent(in) :: n, bs

        integer(int32) :: i, k, col, idx_i, idx_c, ii, jj

        ! res = b
        res(1:n * bs) = b(1:n * bs)

        ! res -= A*x (block SpMV)
        do i = 1, n
            idx_i = (i - 1) * bs
            do k = a_ptr(i), a_ptr(i + 1) - 1
                col = a_ind(k)
                idx_c = (col - 1) * bs
                do jj = 1, bs
                    do ii = 1, bs
                        res(idx_i + ii) = res(idx_i + ii) - a_val(ii, jj, k) * x(idx_c + jj)
                    end do
                end do
            end do
        end do

    end subroutine compute_residual_bsr

    !> Deallocate all resources.
    module subroutine destroy_preconditioner_saamg(self)
        implicit none
        class(type_preconditioner_saamg), intent(inout) :: self

        if (allocated(self%name)) deallocate (self%name)
        if (allocated(self%diag_inv)) deallocate (self%diag_inv)
        if (allocated(self%fine_diag_ptr)) deallocate (self%fine_diag_ptr)
        if (allocated(self%P_ptr)) deallocate (self%P_ptr)
        if (allocated(self%P_ind)) deallocate (self%P_ind)
        if (allocated(self%P_val)) deallocate (self%P_val)
        if (allocated(self%R_ptr)) deallocate (self%R_ptr)
        if (allocated(self%R_ind)) deallocate (self%R_ind)
        if (allocated(self%R_val)) deallocate (self%R_val)
        if (allocated(self%coarse_val)) deallocate (self%coarse_val)
        if (allocated(self%coarse_lu)) deallocate (self%coarse_lu)
        if (allocated(self%coarse_pivots)) deallocate (self%coarse_pivots)
        if (allocated(self%work_fine)) deallocate (self%work_fine)
        if (allocated(self%work_coarse)) deallocate (self%work_coarse)
        nullify (self%a_ptr)
        nullify (self%a_ind)
        nullify (self%a_val)

        self%ID = -1
        self%num_rows = -1
        self%num_coarse = -1
        self%is_block = .false.

    end subroutine destroy_preconditioner_saamg

    !> Frobenius norm of a block.
    pure function block_fnorm(blk, bs) result(nrm)
        implicit none
        integer(int32), intent(in) :: bs
        real(real64), intent(in) :: blk(bs, bs)
        real(real64) :: nrm
        integer(int32) :: ii, jj

        nrm = 0.0d0
        do jj = 1, bs
            do ii = 1, bs
                nrm = nrm + blk(ii, jj)**2
            end do
        end do
        nrm = sqrt(nrm)

    end function block_fnorm

end submodule solver_preconditioner_saamg
