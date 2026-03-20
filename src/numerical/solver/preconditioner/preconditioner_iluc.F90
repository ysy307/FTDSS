!> Crout ILU preconditioner: column-based ILU factorization for BSR matrices.
!> Uses left-looking Crout variant for improved numerical stability.
submodule(numerical_solver_preconditioner) solver_preconditioner_iluc
    implicit none
contains

    !> Initialize the Crout ILU preconditioner instance.
    module subroutine initialize_preconditioner_iluc(self, info)
        implicit none
        class(type_preconditioner_iluc), intent(inout) :: self
        type(type_preconditioner_settings), intent(in) :: info

        self%ID = PRECONDITIONER_TYPES%ILUC%ID
        self%status = SOLVER_STATUS%SUCCESS%ID
        self%name = "Crout ILU"
        self%num_rows = info%num_nodes
        self%drop_tolerance = info%ilut_drop_tolerance

        if (info%block_size > 1) then
            self%is_block = .true.
            self%block_size = info%block_size
        else
            self%is_block = .false.
            self%block_size = 1
        end if

    end subroutine initialize_preconditioner_iluc

    !> Set up Crout ILU factorization.
    !> Left-looking column Crout: for each column k, compute L(:,k) and U(k,:)
    !> simultaneously, dropping small entries.
    module subroutine setup_preconditioner_iluc(self, A)
        implicit none
        class(type_preconditioner_iluc), intent(inout) :: self
        class(abst_matrix), intent(in) :: A

        type(type_matrix_info) :: info

        call A%get_info(info)
        self%num_rows = info%num_rows

        self%status = SOLVER_STATUS%NOT_IMPLEMENTED%ID

        select type (A)
        type is (type_matrix_bsr)
            if (self%is_block) then
                call self%setup_bsr(A)
            else
                self%status = SOLVER_STATUS%NOT_IMPLEMENTED%ID
            end if
        class default
            self%status = SOLVER_STATUS%NOT_IMPLEMENTED%ID
        end select

    end subroutine setup_preconditioner_iluc

    !> Block Crout ILU for BSR matrix.
    !> Uses same sparsity pattern as original matrix (ILU(0) pattern)
    !> with Crout ordering and threshold dropping on values.
    module subroutine setup_bsr_iluc(self, A)
        implicit none
        class(type_preconditioner_iluc), intent(inout) :: self
        class(type_matrix_bsr), intent(in) :: A

        integer(int32), dimension(:), pointer :: mat_ptr, mat_ind
        real(real64), dimension(:, :, :), pointer :: mat_val
        integer(int32) :: n, bs, nnz, i, k, jj, j, col, target_idx
        integer(int32) :: info_lapack, kb
        real(real64) :: row_norm, blk_norm, tau
        real(real64), parameter :: PIVOT_TOL = 1.0d-12
        integer(int32), allocatable :: work_pos(:)

        bs = self%block_size
        n = self%num_rows
        tau = self%drop_tolerance

        mat_ptr => A%get_ptr()
        mat_ind => A%get_ind()
        mat_val => A%get_val()

        if (.not. associated(mat_val)) then
            self%status = -1
            return
        end if
        nnz = size(mat_val, 3)

        if (allocated(self%val_blocks)) deallocate (self%val_blocks)
        if (allocated(self%ptr)) deallocate (self%ptr)
        if (allocated(self%ind)) deallocate (self%ind)
        if (allocated(self%diag_ptr)) deallocate (self%diag_ptr)
        if (allocated(self%diag_pivots)) deallocate (self%diag_pivots)

        allocate (self%val_blocks(bs, bs, nnz))
        allocate (self%ptr(n + 1))
        allocate (self%ind(nnz))
        allocate (self%diag_ptr(n))
        allocate (self%diag_pivots(bs, n))
        allocate (work_pos(n))

        self%ptr = mat_ptr
        self%ind = mat_ind
        self%val_blocks = mat_val
        work_pos = 0

        ! Find diagonal positions
        do i = 1, n
            self%diag_ptr(i) = -1
            do k = self%ptr(i), self%ptr(i + 1) - 1
                if (self%ind(k) == i) then
                    self%diag_ptr(i) = k
                    exit
                end if
            end do
            if (self%diag_ptr(i) == -1) then
                self%status = SOLVER_STATUS%DECOMPOSITION_FAILURE%ID
                return
            end if
        end do

        ! Stabilize diagonal
        do i = 1, n
            do kb = 1, bs
                if (self%val_blocks(kb, kb, self%diag_ptr(i)) < 0.0d0) then
                    self%val_blocks(kb, kb, self%diag_ptr(i)) = &
                        abs(self%val_blocks(kb, kb, self%diag_ptr(i)))
                else if (abs(self%val_blocks(kb, kb, self%diag_ptr(i))) < PIVOT_TOL) then
                    self%val_blocks(kb, kb, self%diag_ptr(i)) = PIVOT_TOL
                end if
            end do
        end do

        ! Crout-style ILU: process columns left-to-right
        ! For column k: update L(i,k) for i>k and U(k,j) for j>k
        do k = 1, n
            ! Build work_pos for row k
            do j = self%ptr(k), self%ptr(k + 1) - 1
                work_pos(self%ind(j)) = j
            end do

            ! Compute row norm for drop tolerance
            row_norm = 0.0d0
            do j = self%ptr(k), self%ptr(k + 1) - 1
                row_norm = row_norm + block_fnorm_iluc(self%val_blocks(:, :, j), bs)
            end do
            row_norm = row_norm / max(1, self%ptr(k + 1) - self%ptr(k))

            ! Update L part: for each column j < k in row k
            do j = self%ptr(k), self%diag_ptr(k) - 1
                col = self%ind(j)
                ! L_kj = A_kj * U_jj^{-1}
                call dtrsm('R', 'U', 'N', 'N', bs, bs, 1.0d0, &
                           self%val_blocks(:, :, self%diag_ptr(col)), bs, &
                           self%val_blocks(:, :, j), bs)

                ! Drop small L blocks
                blk_norm = block_fnorm_iluc(self%val_blocks(:, :, j), bs)
                if (blk_norm < tau * row_norm) then
                    self%val_blocks(:, :, j) = 0.0d0
                    cycle
                end if

                ! Update: A_kp -= L_kj * U_jp for p > j
                do jj = self%diag_ptr(col) + 1, self%ptr(col + 1) - 1
                    target_idx = work_pos(self%ind(jj))
                    if (target_idx > 0) then
                        call dgemm('N', 'N', bs, bs, bs, -1.0d0, &
                                   self%val_blocks(:, :, j), bs, &
                                   self%val_blocks(:, :, jj), bs, 1.0d0, &
                                   self%val_blocks(:, :, target_idx), bs)
                    end if
                end do
            end do

            ! Drop small U blocks
            do j = self%diag_ptr(k) + 1, self%ptr(k + 1) - 1
                blk_norm = block_fnorm_iluc(self%val_blocks(:, :, j), bs)
                if (blk_norm < tau * row_norm) then
                    self%val_blocks(:, :, j) = 0.0d0
                end if
            end do

            ! Stabilize diagonal after elimination
            do kb = 1, bs
                if (abs(self%val_blocks(kb, kb, self%diag_ptr(k))) < PIVOT_TOL) then
                    self%val_blocks(kb, kb, self%diag_ptr(k)) = PIVOT_TOL
                end if
            end do

            ! LU factorize diagonal block
            call dgetrf(bs, bs, self%val_blocks(:, :, self%diag_ptr(k)), bs, &
                        self%diag_pivots(:, k), info_lapack)
            if (info_lapack /= 0) then
                self%status = SOLVER_STATUS%DECOMPOSITION_FAILURE%ID
                return
            end if

            ! Clear work_pos
            do j = self%ptr(k), self%ptr(k + 1) - 1
                work_pos(self%ind(j)) = 0
            end do
        end do

        self%status = SOLVER_STATUS%SUCCESS%ID

    end subroutine setup_bsr_iluc

    !> Apply Crout ILU solve. Same forward/backward as standard block ILU.
    module subroutine apply_preconditioner_iluc(self, r, z)
        implicit none
        class(type_preconditioner_iluc), intent(inout) :: self
        type(type_vector_dp), intent(in) :: r
        type(type_vector_dp), intent(inout) :: z

        real(real64), dimension(:), pointer :: x
        integer(int32) :: i, k, col, bs, idx_i, idx_c, ierr

        if (.not. allocated(self%ptr)) then
            call z%copy(r)
            return
        end if

        bs = self%block_size
        call z%copy(r)
        x => z%get_data()

        ! Forward substitution: Lz = r
        do i = 1, self%num_rows
            idx_i = (i - 1) * bs + 1
            do k = self%ptr(i), self%diag_ptr(i) - 1
                col = self%ind(k)
                idx_c = (col - 1) * bs + 1
                call dgemv('N', bs, bs, -1.0d0, &
                           self%val_blocks(:, :, k), bs, &
                           x(idx_c), 1, 1.0d0, &
                           x(idx_i), 1)
            end do
        end do

        ! Back substitution: Uz = z
        do i = self%num_rows, 1, -1
            idx_i = (i - 1) * bs + 1
            do k = self%diag_ptr(i) + 1, self%ptr(i + 1) - 1
                col = self%ind(k)
                idx_c = (col - 1) * bs + 1
                call dgemv('N', bs, bs, -1.0d0, &
                           self%val_blocks(:, :, k), bs, &
                           x(idx_c), 1, 1.0d0, &
                           x(idx_i), 1)
            end do
            call dgetrs('N', bs, 1, &
                        self%val_blocks(:, :, self%diag_ptr(i)), bs, &
                        self%diag_pivots(:, i), &
                        x(idx_i), bs, ierr)
        end do

    end subroutine apply_preconditioner_iluc

    !> Deallocate all resources.
    module subroutine destroy_preconditioner_iluc(self)
        implicit none
        class(type_preconditioner_iluc), intent(inout) :: self

        if (allocated(self%val_blocks)) deallocate (self%val_blocks)
        if (allocated(self%ptr)) deallocate (self%ptr)
        if (allocated(self%ind)) deallocate (self%ind)
        if (allocated(self%diag_ptr)) deallocate (self%diag_ptr)
        if (allocated(self%diag_pivots)) deallocate (self%diag_pivots)
        if (allocated(self%name)) deallocate (self%name)
        self%ID = -1
        self%num_rows = -1
        self%is_block = .false.

    end subroutine destroy_preconditioner_iluc

    pure function block_fnorm_iluc(blk, bs) result(nrm)
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
    end function block_fnorm_iluc

end submodule solver_preconditioner_iluc
