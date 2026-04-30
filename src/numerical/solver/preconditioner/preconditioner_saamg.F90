submodule(numerical_solver_preconditioner) solver_preconditioner_saamg
    implicit none
contains

    module subroutine initialize_preconditioner_saamg(self, info)
        implicit none
        class(type_preconditioner_saamg), intent(inout) :: self
        type(type_preconditioner_settings), intent(in) :: info

        self%ID = PRECONDITIONER_TYPES%SAAMG%ID
        self%status = SOLVER_STATUS%SUCCESS%ID
        if (allocated(self%name)) deallocate (self%name)
        self%name = "SA-AMG"
        self%num_rows = info%num_nodes
        self%block_size = max(1, info%block_size)
        self%strength_threshold = 0.25d0

    end subroutine initialize_preconditioner_saamg

    module subroutine apply_preconditioner_saamg(self, r, z)
        implicit none
        class(type_preconditioner_saamg), intent(inout) :: self
        type(type_vector_dp), intent(in) :: r
        type(type_vector_dp), intent(inout) :: z

        real(real64), dimension(:), pointer :: xz, xr
        integer(int32) :: i, k, col, bs, idx_i, idx_c, ierr, nc_dofs

        bs = self%block_size
        nc_dofs = self%num_coarse * bs

        xr => r%get_data()
        call z%copy(r)
        xz => z%get_data()

        if (.not. allocated(self%work_fine)) then
            self%status = SOLVER_STATUS%NOT_IMPLEMENTED%ID
            return
        end if

        ! Initialize correction vector to zero
        self%work_fine = 0.0d0

        ! Restrict residual to coarse grid: r_c = P^T * r
        self%work_coarse = 0.0d0
        do i = 1, self%num_rows
            idx_i = (i - 1)*bs + 1
            do k = self%P_ptr(i), self%P_ptr(i + 1) - 1
                col = self%P_ind(k)
                idx_c = (col - 1)*bs + 1
                call dgemv('T', bs, bs, 1.0d0, self%P_val(:, :, k), bs, &
                           xr(idx_i), 1, 1.0d0, self%work_coarse(idx_c), 1)
            end do
        end do

        ! Coarse-grid direct solve via LU: x_c = A_c^{-1} * r_c
        self%work_coarse_block(1:nc_dofs, 1) = self%work_coarse(1:nc_dofs)
        call dgetrs('N', nc_dofs, 1, self%coarse_lu, nc_dofs, self%coarse_pivots, &
                    self%work_coarse_block(1, 1), nc_dofs, ierr)
        if (ierr /= 0) then
            self%status = SOLVER_STATUS%DECOMPOSITION_FAILURE%ID
            return
        end if

        ! Prolongate coarse correction: x = P * x_c
        do i = 1, self%num_rows
            idx_i = (i - 1)*bs + 1
            do k = self%P_ptr(i), self%P_ptr(i + 1) - 1
                col = self%P_ind(k)
                idx_c = (col - 1)*bs + 1
                call dgemv('N', bs, bs, 1.0d0, self%P_val(:, :, k), bs, &
                           self%work_coarse_block(idx_c, 1), 1, 1.0d0, self%work_fine(idx_i), 1)
            end do
        end do

        ! Return correction: z = x
        xz = self%work_fine

    end subroutine apply_preconditioner_saamg

    module subroutine destroy_preconditioner_saamg(self)
        implicit none
        class(type_preconditioner_saamg), intent(inout) :: self

        if (allocated(self%diag_inv)) deallocate (self%diag_inv)
        if (allocated(self%P_ptr)) deallocate (self%P_ptr)
        if (allocated(self%P_ind)) deallocate (self%P_ind)
        if (allocated(self%P_val)) deallocate (self%P_val)
        if (allocated(self%R_ptr)) deallocate (self%R_ptr)
        if (allocated(self%R_ind)) deallocate (self%R_ind)
        if (allocated(self%R_val)) deallocate (self%R_val)
        if (allocated(self%Ac_ptr)) deallocate (self%Ac_ptr)
        if (allocated(self%Ac_ind)) deallocate (self%Ac_ind)
        if (allocated(self%Ac_val)) deallocate (self%Ac_val)
        if (allocated(self%coarse_lu)) deallocate (self%coarse_lu)
        if (allocated(self%coarse_pivots)) deallocate (self%coarse_pivots)
        if (allocated(self%work_fine)) deallocate (self%work_fine)
        if (allocated(self%work_coarse)) deallocate (self%work_coarse)
        if (allocated(self%work_coarse_block)) deallocate (self%work_coarse_block)
        if (associated(self%a_ptr)) nullify (self%a_ptr)
        if (associated(self%a_ind)) nullify (self%a_ind)
        if (associated(self%a_val)) nullify (self%a_val)
        if (allocated(self%name)) deallocate (self%name)
        self%ID = -1
        self%num_rows = -1
        self%status = SOLVER_STATUS%SUCCESS%ID

    end subroutine destroy_preconditioner_saamg

    module subroutine setup_preconditioner_saamg(self, A)
        implicit none
        class(type_preconditioner_saamg), intent(inout) :: self
        class(abst_matrix), intent(in) :: A

        integer(int32), pointer :: a_ptr(:), a_ind(:)
        real(real64), pointer :: a_val(:, :, :)
        integer(int32) :: n, bs
        integer(int32), allocatable :: aggregate(:)

        select type (A)
        type is (type_matrix_bsr)
            a_ptr => A%get_ptr()
            a_ind => A%get_ind()
            a_val => A%get_val()

            if (.not. associated(a_val)) then
                self%status = -1
                return
            end if

            n = self%num_rows
            bs = self%block_size

            self%a_ptr => a_ptr
            self%a_ind => a_ind
            self%a_val => a_val

            allocate (aggregate(n))

            call build_block_diag_inverse(self, n, bs)
            if (self%status /= SOLVER_STATUS%SUCCESS%ID) return

            call build_aggregation(self, aggregate, n, bs)
            if (self%status /= SOLVER_STATUS%SUCCESS%ID) return

            call build_prolongation(self, aggregate, n, bs)
            if (self%status /= SOLVER_STATUS%SUCCESS%ID) return

            call build_restriction(self, n, self%num_coarse, bs)
            if (self%status /= SOLVER_STATUS%SUCCESS%ID) return

            call build_coarse_matrix_galerkin(self, n, self%num_coarse, bs)
            if (self%status /= SOLVER_STATUS%SUCCESS%ID) return

            deallocate (aggregate)

            ! Allocate workspace vectors
            if (allocated(self%work_fine)) deallocate (self%work_fine)
            if (allocated(self%work_coarse)) deallocate (self%work_coarse)
            if (allocated(self%work_coarse_block)) deallocate (self%work_coarse_block)
            allocate (self%work_fine(n * bs))
            allocate (self%work_coarse(self%num_coarse * bs))
            allocate (self%work_coarse_block(self%num_coarse * bs, 1))
            self%work_fine = 0.0d0
            self%work_coarse = 0.0d0
            self%work_coarse_block = 0.0d0

            ! Factor coarse matrix for direct solve
            call factor_coarse_matrix(self, self%num_coarse, bs)

            self%status = SOLVER_STATUS%SUCCESS%ID

        end select
    end subroutine setup_preconditioner_saamg

    subroutine factor_coarse_matrix(self, nc, bs)
        implicit none
        class(type_preconditioner_saamg), intent(inout) :: self
        integer(int32), intent(in) :: nc, bs

        integer(int32) :: nc_dofs, i, k, row, col, ierr
        nc_dofs = nc * bs

        if (allocated(self%coarse_lu)) deallocate (self%coarse_lu)
        if (allocated(self%coarse_pivots)) deallocate (self%coarse_pivots)
        allocate (self%coarse_lu(nc_dofs, nc_dofs))
        allocate (self%coarse_pivots(nc_dofs))
        self%coarse_lu = 0.0d0

        do i = 1, nc
            do k = self%Ac_ptr(i), self%Ac_ptr(i + 1) - 1
                col = self%Ac_ind(k)
                self%coarse_lu((i - 1)*bs + 1:i*bs, (col - 1)*bs + 1:col*bs) = self%Ac_val(:, :, k)
            end do
        end do

        call dgetrf(nc_dofs, nc_dofs, self%coarse_lu, nc_dofs, self%coarse_pivots, ierr)
        if (ierr /= 0) then
            self%status = SOLVER_STATUS%DECOMPOSITION_FAILURE%ID
        end if
    end subroutine factor_coarse_matrix

    subroutine build_block_diag_inverse(self, n, bs)
        implicit none
        class(type_preconditioner_saamg), intent(inout) :: self
        integer(int32), intent(in) :: n, bs

        integer(int32) :: i, k, diag_idx
        real(real64) :: D(bs, bs)
        real(real64), parameter :: tol = 1.0d-12

        if (allocated(self%diag_inv)) deallocate (self%diag_inv)
        allocate (self%diag_inv(bs, bs, n))

        do i = 1, n
            D = 0.0d0
            diag_idx = -1

            do k = self%a_ptr(i), self%a_ptr(i + 1) - 1
                if (self%a_ind(k) == i) then
                    D = self%a_val(:, :, k)
                    diag_idx = k
                    exit
                end if
            end do

            if (diag_idx == -1) then
                self%status = SOLVER_STATUS%NOT_IMPLEMENTED%ID
                return
            end if

            call matrix_inverse(D, self%status)

            if (self%status /= SOLVER_STATUS%SUCCESS%ID) then
                D = 0.0d0
                do k = 1, bs
                    if (abs(self%a_val(k, k, diag_idx)) > tol) then
                        D(k, k) = 1.0d0 / self%a_val(k, k, diag_idx)
                    else
                        D(k, k) = 1.0d0
                    end if
                end do
                self%status = SOLVER_STATUS%SUCCESS%ID
            end if

            self%diag_inv(:, :, i) = D
        end do
    end subroutine build_block_diag_inverse

    subroutine build_aggregation(self, aggregate, n, bs)
        implicit none
        class(type_preconditioner_saamg), intent(inout) :: self
        integer(int32), intent(inout) :: aggregate(:)
        integer(int32), intent(in) :: n, bs

        integer(int32) :: i, k, kk, col, agg_id, agg_count
        real(real64) :: theta, s_ij, s_ji, s
        logical, allocatable :: is_aggregated(:)
        real(real64) :: blk(bs, bs), blk2(bs, bs)

        theta = self%strength_threshold
        allocate (is_aggregated(n))
        aggregate = 0
        is_aggregated = .false.
        agg_id = 0

        do i = 1, n
            if (is_aggregated(i)) cycle

            agg_id = agg_id + 1
            aggregate(i) = agg_id
            is_aggregated(i) = .true.
            agg_count = 1

            do k = self%a_ptr(i), self%a_ptr(i + 1) - 1
                col = self%a_ind(k)
                if (col == i .or. is_aggregated(col)) cycle
                if (agg_count >= 3) exit

                call matrix_gemm(self%diag_inv(:, :, i), self%a_val(:, :, k), blk, self%status)
                if (self%status /= SOLVER_STATUS%SUCCESS%ID) return
                s_ij = block_fnorm(blk, bs)

                s_ji = 0.0d0
                do kk = self%a_ptr(col), self%a_ptr(col + 1) - 1
                    if (self%a_ind(kk) == i) then
                        call matrix_gemm(self%diag_inv(:, :, col), self%a_val(:, :, kk), blk2, self%status)
                        if (self%status /= SOLVER_STATUS%SUCCESS%ID) return
                        s_ji = block_fnorm(blk2, bs)
                        exit
                    end if
                end do

                s = max(s_ij, s_ji)

                if (s >= theta) then
                    aggregate(col) = agg_id
                    is_aggregated(col) = .true.
                    agg_count = agg_count + 1
                end if
            end do
        end do

        do i = 1, n
            if (is_aggregated(i)) cycle
            agg_id = agg_id + 1
            aggregate(i) = agg_id
            is_aggregated(i) = .true.
        end do

        self%num_coarse = agg_id
        deallocate (is_aggregated)
    end subroutine build_aggregation

    subroutine build_prolongation(self, aggregate, n, bs)
        implicit none
        class(type_preconditioner_saamg), intent(inout) :: self
        integer(int32), intent(in) :: aggregate(:)
        integer(int32), intent(in) :: n, bs

        integer(int32) :: i, k, j, ic, jc, count, nnz_row, c1, c2, tmp_col
        real(real64) :: blk(bs, bs), I_mat(bs, bs)
        real(real64), parameter :: omega = 0.6666666666666666d0
        integer(int32), allocatable :: marker(:), p_cols(:)
        real(real64), allocatable :: spa_val(:, :, :)

        call set_identity_block(I_mat, bs)

        allocate (marker(self%num_coarse))
        allocate (spa_val(bs, bs, self%num_coarse))
        allocate (p_cols(self%num_coarse))
        marker = -1

        if (allocated(self%P_ptr)) deallocate (self%P_ptr)
        allocate (self%P_ptr(n + 1))
        self%P_ptr(1) = 1
        count = 1

        do i = 1, n
            nnz_row = 0
            ic = aggregate(i)
            marker(ic) = i
            nnz_row = 1

            do k = self%a_ptr(i), self%a_ptr(i + 1) - 1
                j = self%a_ind(k)
                jc = aggregate(j)
                if (marker(jc) /= i) then
                    marker(jc) = i
                    nnz_row = nnz_row + 1
                end if
            end do
            count = count + nnz_row
            self%P_ptr(i + 1) = count
        end do

        if (allocated(self%P_ind)) deallocate (self%P_ind)
        if (allocated(self%P_val)) deallocate (self%P_val)
        allocate (self%P_ind(count - 1))
        allocate (self%P_val(bs, bs, count - 1))

        marker = -1
        count = 1

        do i = 1, n
            nnz_row = 0

            do k = self%a_ptr(i), self%a_ptr(i + 1) - 1
                j = self%a_ind(k)
                jc = aggregate(j)

                call matrix_gemm(self%diag_inv(:, :, i), self%a_val(:, :, k), blk, self%status)
                if (self%status /= SOLVER_STATUS%SUCCESS%ID) return
                blk = -omega * blk

                if (marker(jc) /= i) then
                    marker(jc) = i
                    nnz_row = nnz_row + 1
                    p_cols(nnz_row) = jc
                    spa_val(:, :, jc) = blk
                else
                    spa_val(:, :, jc) = spa_val(:, :, jc) + blk
                end if
            end do

            ic = aggregate(i)
            if (marker(ic) /= i) then
                marker(ic) = i
                nnz_row = nnz_row + 1
                p_cols(nnz_row) = ic
                spa_val(:, :, ic) = I_mat
            else
                spa_val(:, :, ic) = spa_val(:, :, ic) + I_mat
            end if

            do c1 = 2, nnz_row
                tmp_col = p_cols(c1)
                c2 = c1 - 1
                do while (c2 >= 1)
                    if (p_cols(c2) <= tmp_col) exit
                    p_cols(c2 + 1) = p_cols(c2)
                    c2 = c2 - 1
                end do
                p_cols(c2 + 1) = tmp_col
            end do

            do c1 = 1, nnz_row
                jc = p_cols(c1)
                self%P_ind(count) = jc
                self%P_val(:, :, count) = spa_val(:, :, jc)
                count = count + 1
            end do
        end do

        deallocate (marker, spa_val, p_cols)
    end subroutine build_prolongation

    subroutine build_restriction(self, n, nc, bs)
        implicit none
        class(type_preconditioner_saamg), intent(inout) :: self
        integer(int32), intent(in) :: n, nc, bs

        integer(int32) :: i, k, ic, idx, ii, jj
        integer(int32), allocatable :: work_ptr(:)
        real(real64) :: tmp_block(bs, bs)

        if (allocated(self%R_ptr)) deallocate (self%R_ptr)
        allocate (self%R_ptr(nc + 1))
        self%R_ptr = 0

        do i = 1, n
            do k = self%P_ptr(i), self%P_ptr(i + 1) - 1
                ic = self%P_ind(k)
                self%R_ptr(ic + 1) = self%R_ptr(ic + 1) + 1
            end do
        end do

        self%R_ptr(1) = 1
        do ic = 1, nc
            self%R_ptr(ic + 1) = self%R_ptr(ic + 1) + self%R_ptr(ic)
        end do

        if (allocated(self%R_ind)) deallocate (self%R_ind)
        if (allocated(self%R_val)) deallocate (self%R_val)
        allocate (self%R_ind(self%R_ptr(nc + 1) - 1))
        allocate (self%R_val(bs, bs, self%R_ptr(nc + 1) - 1))

        allocate (work_ptr(nc))
        work_ptr = self%R_ptr(1:nc)

        do i = 1, n
            do k = self%P_ptr(i), self%P_ptr(i + 1) - 1
                ic = self%P_ind(k)
                idx = work_ptr(ic)
                self%R_ind(idx) = i
                tmp_block = transpose(self%P_val(:, :, k))
                do jj = 1, bs
                    do ii = 1, bs
                        self%R_val(ii, jj, idx) = tmp_block(ii, jj)
                    end do
                end do
                work_ptr(ic) = work_ptr(ic) + 1
            end do
        end do

        deallocate (work_ptr)
    end subroutine build_restriction

    subroutine build_coarse_matrix_galerkin(self, n, nc, bs)
        implicit none
        class(type_preconditioner_saamg), intent(inout) :: self
        integer(int32), intent(in) :: n, nc, bs

        integer(int32) :: ic, k_R, i, k_A, j, k_P, jc, idx, c1, c2, tmp_col, nnz_row
        integer(int32), allocatable :: marker(:), ac_cols(:)
        real(real64), allocatable :: spa_val(:, :, :)
        real(real64) :: tmp(bs, bs), contrib(bs, bs)
        real(real64) :: tau, row_norm

        allocate (marker(nc))
        allocate (spa_val(bs, bs, nc))
        allocate (ac_cols(nc))
        marker = -1

        if (allocated(self%Ac_ptr)) deallocate (self%Ac_ptr)
        if (allocated(self%Ac_ind)) deallocate (self%Ac_ind)
        if (allocated(self%Ac_val)) deallocate (self%Ac_val)

        allocate (self%Ac_ptr(nc + 1))
        self%Ac_ptr(1) = 1

        do ic = 1, nc
            nnz_row = 0
            do k_R = self%R_ptr(ic), self%R_ptr(ic + 1) - 1
                i = self%R_ind(k_R)
                do k_A = self%a_ptr(i), self%a_ptr(i + 1) - 1
                    j = self%a_ind(k_A)
                    do k_P = self%P_ptr(j), self%P_ptr(j + 1) - 1
                        jc = self%P_ind(k_P)
                        if (marker(jc) /= ic) then
                            marker(jc) = ic
                            nnz_row = nnz_row + 1
                        end if
                    end do
                end do
            end do
            self%Ac_ptr(ic + 1) = self%Ac_ptr(ic) + nnz_row
        end do

        allocate (self%Ac_ind(self%Ac_ptr(nc + 1) - 1))
        allocate (self%Ac_val(bs, bs, self%Ac_ptr(nc + 1) - 1))

        marker = -1
        idx = 1

        do ic = 1, nc
            nnz_row = 0
            row_norm = 0.0d0

            do k_R = self%R_ptr(ic), self%R_ptr(ic + 1) - 1
                i = self%R_ind(k_R)
                do k_A = self%a_ptr(i), self%a_ptr(i + 1) - 1
                    j = self%a_ind(k_A)

                    call matrix_gemm(self%R_val(:, :, k_R), self%a_val(:, :, k_A), tmp, self%status)
                    if (self%status /= SOLVER_STATUS%SUCCESS%ID) return

                    do k_P = self%P_ptr(j), self%P_ptr(j + 1) - 1
                        jc = self%P_ind(k_P)
                        call matrix_gemm(tmp, self%P_val(:, :, k_P), contrib, self%status)
                        if (self%status /= SOLVER_STATUS%SUCCESS%ID) return

                        if (marker(jc) /= ic) then
                            marker(jc) = ic
                            nnz_row = nnz_row + 1
                            ac_cols(nnz_row) = jc
                            spa_val(:, :, jc) = contrib
                        else
                            spa_val(:, :, jc) = spa_val(:, :, jc) + contrib
                        end if
                    end do
                end do
            end do

            do c1 = 1, nnz_row
                jc = ac_cols(c1)
                row_norm = max(row_norm, block_fnorm(spa_val(:, :, jc), bs))
            end do

            tau = 1.0d-10 * row_norm

            do c1 = 2, nnz_row
                tmp_col = ac_cols(c1)
                c2 = c1 - 1
                do while (c2 >= 1)
                    if (ac_cols(c2) <= tmp_col) exit
                    ac_cols(c2 + 1) = ac_cols(c2)
                    c2 = c2 - 1
                end do
                ac_cols(c2 + 1) = tmp_col
            end do

            do c1 = 1, nnz_row
                jc = ac_cols(c1)
                if (block_fnorm(spa_val(:, :, jc), bs) >= tau) then
                    self%Ac_ind(idx) = jc
                    self%Ac_val(:, :, idx) = spa_val(:, :, jc)
                    idx = idx + 1
                end if
            end do

            self%Ac_ptr(ic + 1) = idx
        end do

        deallocate (marker, spa_val, ac_cols)
    end subroutine build_coarse_matrix_galerkin

    subroutine set_identity_block(B, bs)
        implicit none
        integer(int32), intent(in) :: bs
        real(real64), intent(inout) :: B(bs, bs)
        integer(int32) :: i

        B = 0.0d0
        do i = 1, bs
            B(i, i) = 1.0d0
        end do
    end subroutine set_identity_block

    pure function block_fnorm(blk, bs) result(nrm)
        implicit none
        integer(int32), intent(in) :: bs
        real(real64), intent(in) :: blk(bs, bs)
        real(real64) :: nrm
        integer(int32) :: i, j

        nrm = 0.0d0
        do j = 1, bs
            do i = 1, bs
                nrm = nrm + blk(i, j)**2
            end do
        end do
        nrm = sqrt(nrm)
    end function block_fnorm

end submodule solver_preconditioner_saamg
