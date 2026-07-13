!> SAINV (Sparse Approximate Inverse) preconditioner for BSR matrices.
!> Computes M^{-1} ≈ Z D_z^{-1} Z^T via biconjugation (stabilized Frobenius norm).
!> Simplified implementation using diagonal scaling + sparse approximate inverse.
submodule(numerical_solver_preconditioner) solver_preconditioner_sainv
    implicit none
contains

    !> Initialize the SAINV preconditioner.
    module subroutine initialize_preconditioner_sainv(self, info)
        implicit none
        class(type_preconditioner_sainv), intent(inout) :: self
        type(type_preconditioner_settings), intent(in) :: info

        self%ID = PRECONDITIONER_TYPES%SAINV%ID
        self%status = SOLVER_STATUS%SUCCESS%ID
        self%name = "SAINV"
        self%num_rows = info%num_nodes
        self%drop_tolerance = info%ilut_drop_tolerance

        if (info%block_size > 1) then
            self%is_block = .true.
            self%block_size = info%block_size
        else
            self%is_block = .false.
            self%block_size = 1
        end if

    end subroutine initialize_preconditioner_sainv

    !> Setup SAINV: build sparse approximate inverse via stabilized
    !> left-looking biconjugation. For BSR, operates on block diagonal scaling.
    !>
    !> Simplified version: M^{-1} = W D_w^{-1} W^T where W starts as D^{-1/2}
    !> and is refined by dropping small entries.
    module subroutine setup_preconditioner_sainv(self, A)
        implicit none
        class(type_preconditioner_sainv), intent(inout) :: self
        class(abst_matrix), intent(in) :: A

        type(type_matrix_info) :: info
        integer(int32) :: n, bs, i, k, col, kb, ierr
        integer(int32) :: idx_i, idx_c, ii, jj
        real(real64) :: diag_val, tau

        call A%get_info(info)
        self%num_rows = info%num_rows

        self%status = SOLVER_STATUS%NOT_IMPLEMENTED%ID

        select type (A)
        type is (type_matrix_bsr)
            bs = info%num_block_rows
            n = self%num_rows
            tau = self%drop_tolerance

            self%a_ptr => A%get_ptr()
            self%a_ind => A%get_ind()
            self%a_val => A%get_val()

            ! Build approximate inverse: M^{-1} ≈ Z * diag_z^{-1} * Z^T
            ! Simplified: use column of (I - L*D^{-1}) as Z columns
            ! For practical purposes, store as diagonal block inverse + off-diagonal corrections

            ! Extract diagonal blocks and LU factorize
            if (allocated(self%diag_blocks)) deallocate (self%diag_blocks)
            if (allocated(self%diag_pivots)) deallocate (self%diag_pivots)
            if (allocated(self%diag_ptr)) deallocate (self%diag_ptr)
            allocate (self%diag_blocks(bs, bs, n))
            allocate (self%diag_pivots(bs, n))
            allocate (self%diag_ptr(n))

            ! Build W matrix: approximate inverse stored in same sparsity as A
            if (allocated(self%w_val)) deallocate (self%w_val)
            allocate (self%w_val(bs, bs, size(self%a_val, 3)))
            self%w_val(:, :, :) = 0.0d0

            ! Find diagonals
            do i = 1, n
                do k = self%a_ptr(i), self%a_ptr(i + 1) - 1
                    if (self%a_ind(k) == i) then
                        self%diag_ptr(i) = k
                        self%diag_blocks(:, :, i) = self%a_val(:, :, k)
                        exit
                    end if
                end do
                ! LU factorize diagonal
                call dgetrf(bs, bs, self%diag_blocks(:, :, i), bs, &
                            self%diag_pivots(:, i), ierr)
                if (ierr /= 0) then
                    do kb = 1, bs
                        self%diag_blocks(:, :, i) = 0.0d0
                        self%diag_blocks(kb, kb, i) = 1.0d0
                        self%diag_pivots(kb, i) = kb
                    end do
                end if
            end do

            ! W = D^{-1} * (-off_diag) for off-diagonal, I for diagonal
            ! This gives a first-order approximate inverse: M^{-1} ≈ D^{-1}(2I - AD^{-1})
            do i = 1, n
                ! Set diagonal block of W to identity
                do kb = 1, bs
                    self%w_val(kb, kb, self%diag_ptr(i)) = 1.0d0
                end do

                ! For off-diagonal blocks: W_ij = -D_i^{-1} * A_ij * D_j^{-1}
                do k = self%a_ptr(i), self%a_ptr(i + 1) - 1
                    col = self%a_ind(k)
                    if (col == i) cycle

                    ! w_ij = -A_ij
                    self%w_val(:, :, k) = -self%a_val(:, :, k)

                    ! w_ij = D_i^{-1} * w_ij
                    call dgetrs('N', bs, bs, self%diag_blocks(:, :, i), bs, &
                                self%diag_pivots(:, i), self%w_val(:, :, k), bs, ierr)

                    ! Drop small blocks
                    diag_val = 0.0d0
                    do jj = 1, bs
                        do ii = 1, bs
                            diag_val = diag_val + self%w_val(ii, jj, k)**2
                        end do
                    end do
                    if (sqrt(diag_val) < tau) then
                        self%w_val(:, :, k) = 0.0d0
                    end if
                end do
            end do

            ! Allocate workspace
            if (allocated(self%work)) deallocate (self%work)
            allocate (self%work(n * bs))

            self%status = SOLVER_STATUS%SUCCESS%ID

        class default
            self%status = SOLVER_STATUS%NOT_IMPLEMENTED%ID
        end select

    end subroutine setup_preconditioner_sainv

    !> Apply SAINV: z = W * (D^{-1} * (W^T * r))
    !> Simplified: z = W * r (since W already includes D^{-1} scaling)
    module subroutine apply_preconditioner_sainv(self, r, z)
        implicit none
        class(type_preconditioner_sainv), intent(inout) :: self
        type(type_vector_dp), intent(in) :: r
        type(type_vector_dp), intent(inout) :: z

        real(real64), pointer :: r_data(:), z_data(:)
        integer(int32) :: n, bs, i, k, col, idx_i, idx_c, ii, jj, ierr

        if (self%status /= SOLVER_STATUS%SUCCESS%ID) then
            call z%copy(r)
            return
        end if

        n = self%num_rows
        bs = self%block_size
        r_data => r%get_data()
        z_data => z%get_data()

        ! z = W * r (block SpMV with approximate inverse)
        z_data(:) = 0.0d0
        do i = 1, n
            idx_i = (i - 1) * bs
            do k = self%a_ptr(i), self%a_ptr(i + 1) - 1
                col = self%a_ind(k)
                idx_c = (col - 1) * bs
                do jj = 1, bs
                    do ii = 1, bs
                        z_data(idx_i + ii) = z_data(idx_i + ii) + &
                            self%w_val(ii, jj, k) * r_data(idx_c + jj)
                    end do
                end do
            end do
        end do

        ! Apply the left diagonal inverse represented by the factored blocks.
        do i = 1, n
            idx_i = (i - 1) * bs + 1
            call dgetrs('N', bs, 1, self%diag_blocks(:, :, i), bs, &
                        self%diag_pivots(:, i), z_data(idx_i), bs, ierr)
        end do

    end subroutine apply_preconditioner_sainv

    !> Deallocate all resources.
    module subroutine destroy_preconditioner_sainv(self)
        implicit none
        class(type_preconditioner_sainv), intent(inout) :: self

        if (allocated(self%diag_blocks)) deallocate (self%diag_blocks)
        if (allocated(self%diag_pivots)) deallocate (self%diag_pivots)
        if (allocated(self%diag_ptr)) deallocate (self%diag_ptr)
        if (allocated(self%w_val)) deallocate (self%w_val)
        if (allocated(self%work)) deallocate (self%work)
        if (allocated(self%name)) deallocate (self%name)
        nullify (self%a_ptr)
        nullify (self%a_ind)
        nullify (self%a_val)
        self%ID = -1
        self%num_rows = -1
        self%is_block = .false.

    end subroutine destroy_preconditioner_sainv

end submodule solver_preconditioner_sainv
