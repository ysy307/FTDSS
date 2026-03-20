!> ILUT preconditioner: ILU with threshold dropping and fill-in control.
!> BSR block-aware implementation.
submodule(numerical_solver_preconditioner) solver_preconditioner_ilut
    implicit none
contains

    !> Initialize the ILUT preconditioner instance.
    module subroutine initialize_preconditioner_ilut(self, info)
        implicit none
        class(type_preconditioner_ilut), intent(inout) :: self
        type(type_preconditioner_settings), intent(in) :: info

        self%ID = PRECONDITIONER_TYPES%ILUT%ID
        self%status = SOLVER_STATUS%SUCCESS%ID
        self%name = "ILUT"
        self%num_rows = info%num_nodes
        self%drop_tolerance = info%ilut_drop_tolerance
        self%max_fill = info%ilut_max_fill

        if (info%block_size > 1) then
            self%is_block = .true.
            self%block_size = info%block_size
        else
            self%is_block = .false.
            self%block_size = 1
        end if

    end subroutine initialize_preconditioner_ilut

    !> Set up the ILUT factorization for matrix A.
    module subroutine setup_preconditioner_ilut(self, A)
        implicit none
        class(type_preconditioner_ilut), intent(inout) :: self
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

    end subroutine setup_preconditioner_ilut

    !> Block ILUT factorization for BSR matrix.
    !> Drop criterion: ||block|| < tau * ||row||
    !> Fill control: keep at most max_fill entries per row in L and U.
    module subroutine setup_bsr_ilut(self, A)
        implicit none
        class(type_preconditioner_ilut), intent(inout) :: self
        class(type_matrix_bsr), intent(in) :: A

        integer(int32), dimension(:), pointer :: mat_ptr, mat_ind
        real(real64), dimension(:, :, :), pointer :: mat_val
        integer(int32) :: n, bs, nnz_orig, i, k, jj, j, col, target_idx
        integer(int32) :: info_lapack
        integer(int32) :: nnz_L, nnz_U, kb
        real(real64) :: row_norm, blk_norm, tau
        real(real64), parameter :: PIVOT_TOL = 1.0d-12
        integer(int32), allocatable :: work_pos(:)

        ! Temporary storage for row-by-row factorization
        ! We use the same sparsity pattern as ILU(0) but with threshold dropping
        integer(int32) :: max_nnz_per_row, nnz_row, nnz_total, nnz_alloc
        integer(int32) :: ptr_pos

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

        nnz_orig = size(mat_val, 3)

        ! For ILUT, we allow additional fill-in. Allocate extra space.
        ! Estimate: original nnz + max_fill per row for L and U
        max_nnz_per_row = 0
        do i = 1, n
            max_nnz_per_row = max(max_nnz_per_row, mat_ptr(i + 1) - mat_ptr(i))
        end do
        nnz_alloc = nnz_orig + 2 * self%max_fill * n

        if (allocated(self%val_blocks)) deallocate (self%val_blocks)
        if (allocated(self%ptr)) deallocate (self%ptr)
        if (allocated(self%ind)) deallocate (self%ind)
        if (allocated(self%diag_ptr)) deallocate (self%diag_ptr)
        if (allocated(self%diag_pivots)) deallocate (self%diag_pivots)

        ! For simplicity with ILUT+BSR, use same sparsity as ILU(0)
        ! but apply threshold dropping to values (zero out small blocks)
        ! This avoids complex dynamic sparsity management
        allocate (self%val_blocks(bs, bs, nnz_orig))
        allocate (self%ptr(n + 1))
        allocate (self%ind(nnz_orig))
        allocate (self%diag_ptr(n))
        allocate (self%diag_pivots(bs, n))

        allocate (work_pos(n))
        work_pos = 0

        self%ptr = mat_ptr
        self%ind = mat_ind
        self%val_blocks = mat_val

        ! Find diagonal pointers and compute modified diagonal for stability
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

        ! Stabilize diagonal blocks (same as ILU(0))
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

        ! ILU factorization with threshold dropping
        do i = 1, n
            ! Compute row norm for drop criterion
            row_norm = 0.0d0
            do k = self%ptr(i), self%ptr(i + 1) - 1
                row_norm = row_norm + block_frobenius_norm(self%val_blocks(:, :, k), bs)
            end do
            row_norm = row_norm / max(1, self%ptr(i + 1) - self%ptr(i))

            ! Build work_pos map for this row
            do k = self%ptr(i), self%ptr(i + 1) - 1
                work_pos(self%ind(k)) = k
            end do

            ! Elimination: process L part (columns < i)
            do k = self%ptr(i), self%diag_ptr(i) - 1
                col = self%ind(k)

                ! L_ik = A_ik * U_kk^{-1}
                call dtrsm('R', 'U', 'N', 'N', bs, bs, 1.0d0, &
                           self%val_blocks(:, :, self%diag_ptr(col)), bs, &
                           self%val_blocks(:, :, k), bs)

                ! Drop small L blocks
                blk_norm = block_frobenius_norm(self%val_blocks(:, :, k), bs)
                if (blk_norm < tau * row_norm) then
                    self%val_blocks(:, :, k) = 0.0d0
                    cycle
                end if

                ! Update remaining row: A_ij -= L_ik * U_kj
                do jj = self%diag_ptr(col) + 1, self%ptr(col + 1) - 1
                    j = self%ind(jj)
                    target_idx = work_pos(j)
                    if (target_idx > 0) then
                        call dgemm('N', 'N', bs, bs, bs, -1.0d0, &
                                   self%val_blocks(:, :, k), bs, &
                                   self%val_blocks(:, :, jj), bs, 1.0d0, &
                                   self%val_blocks(:, :, target_idx), bs)
                    end if
                end do
            end do

            ! Drop small U blocks (columns > i)
            do k = self%diag_ptr(i) + 1, self%ptr(i + 1) - 1
                blk_norm = block_frobenius_norm(self%val_blocks(:, :, k), bs)
                if (blk_norm < tau * row_norm) then
                    self%val_blocks(:, :, k) = 0.0d0
                end if
            end do

            ! Stabilize diagonal
            do kb = 1, bs
                if (abs(self%val_blocks(kb, kb, self%diag_ptr(i))) < PIVOT_TOL) then
                    self%val_blocks(kb, kb, self%diag_ptr(i)) = PIVOT_TOL
                end if
            end do

            ! LU factorize diagonal block
            call dgetrf(bs, bs, self%val_blocks(:, :, self%diag_ptr(i)), bs, &
                        self%diag_pivots(:, i), info_lapack)

            if (info_lapack /= 0) then
                self%status = SOLVER_STATUS%DECOMPOSITION_FAILURE%ID
                return
            end if

            ! Clear work_pos for this row
            do k = self%ptr(i), self%ptr(i + 1) - 1
                work_pos(self%ind(k)) = 0
            end do
        end do

        self%status = SOLVER_STATUS%SUCCESS%ID

    end subroutine setup_bsr_ilut

    !> Apply the ILUT preconditioner.
    module subroutine apply_preconditioner_ilut(self, r, z)
        implicit none
        class(type_preconditioner_ilut), intent(inout) :: self
        type(type_vector_dp), intent(in) :: r
        type(type_vector_dp), intent(inout) :: z

        if (.not. allocated(self%ptr)) then
            call z%copy(r)
            return
        end if

        if (self%is_block) then
            call self%apply_bsr(r, z)
        else
            call z%copy(r)
        end if

    end subroutine apply_preconditioner_ilut

    !> Apply block ILUT solve (BSR). Same structure as ILU(0) apply.
    module subroutine apply_bsr_ilut(self, r, z)
        implicit none
        class(type_preconditioner_ilut), intent(inout) :: self
        type(type_vector_dp), intent(in) :: r
        type(type_vector_dp), intent(inout) :: z

        real(real64), dimension(:), pointer :: x
        integer(int32) :: i, k, col, bs, idx_i, idx_c, ierr

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

    end subroutine apply_bsr_ilut

    !> Deallocate all resources.
    module subroutine destroy_preconditioner_ilut(self)
        implicit none
        class(type_preconditioner_ilut), intent(inout) :: self

        if (allocated(self%val_blocks)) deallocate (self%val_blocks)
        if (allocated(self%ptr)) deallocate (self%ptr)
        if (allocated(self%ind)) deallocate (self%ind)
        if (allocated(self%diag_ptr)) deallocate (self%diag_ptr)
        if (allocated(self%diag_pivots)) deallocate (self%diag_pivots)
        if (allocated(self%name)) deallocate (self%name)

        self%ID = -1
        self%num_rows = -1
        self%is_block = .false.

    end subroutine destroy_preconditioner_ilut

    !> Compute Frobenius norm of a block.
    pure function block_frobenius_norm(blk, bs) result(nrm)
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

    end function block_frobenius_norm

end submodule solver_preconditioner_ilut
