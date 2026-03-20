!> I+S approximate inverse preconditioner for BSR matrices.
!> Computes M^{-1} ≈ I + S where S ≈ I - A_scaled.
!> A_scaled = D^{-1}A so that diagonal is identity, then S = I - A_scaled.
!> Apply: z = (I + S) r = (2I - D^{-1}A) r = 2r - D^{-1}(Ar)
submodule(numerical_solver_preconditioner) solver_preconditioner_is
    implicit none
contains

    !> Initialize the I+S preconditioner.
    module subroutine initialize_preconditioner_is(self, info)
        implicit none
        class(type_preconditioner_is), intent(inout) :: self
        type(type_preconditioner_settings), intent(in) :: info

        self%ID = PRECONDITIONER_TYPES%IS%ID
        self%status = SOLVER_STATUS%SUCCESS%ID
        self%name = "I+S"
        self%num_rows = info%num_nodes

        if (info%block_size > 1) then
            self%is_block = .true.
            self%block_size = info%block_size
        else
            self%is_block = .false.
            self%block_size = 1
        end if

    end subroutine initialize_preconditioner_is

    !> Setup: extract and invert diagonal blocks.
    module subroutine setup_preconditioner_is(self, A)
        implicit none
        class(type_preconditioner_is), intent(inout) :: self
        class(abst_matrix), intent(in) :: A

        type(type_matrix_info) :: info
        integer(int32) :: n, bs, i, k, kb, ierr

        call A%get_info(info)
        self%num_rows = info%num_rows

        self%status = SOLVER_STATUS%NOT_IMPLEMENTED%ID

        select type (A)
        type is (type_matrix_bsr)
            bs = info%num_block_rows
            n = self%num_rows

            ! Store matrix pointers for apply
            self%a_ptr => A%get_ptr()
            self%a_ind => A%get_ind()
            self%a_val => A%get_val()

            ! Extract and LU-factorize diagonal blocks
            if (allocated(self%diag_blocks)) deallocate (self%diag_blocks)
            if (allocated(self%diag_pivots)) deallocate (self%diag_pivots)
            if (allocated(self%diag_ptr)) deallocate (self%diag_ptr)
            allocate (self%diag_blocks(bs, bs, n))
            allocate (self%diag_pivots(bs, n))
            allocate (self%diag_ptr(n))

            do i = 1, n
                do k = self%a_ptr(i), self%a_ptr(i + 1) - 1
                    if (self%a_ind(k) == i) then
                        self%diag_ptr(i) = k
                        self%diag_blocks(:, :, i) = self%a_val(:, :, k)
                        exit
                    end if
                end do
                call dgetrf(bs, bs, self%diag_blocks(:, :, i), bs, &
                            self%diag_pivots(:, i), ierr)
                if (ierr /= 0) then
                    self%diag_blocks(:, :, i) = 0.0d0
                    do kb = 1, bs
                        self%diag_blocks(kb, kb, i) = 1.0d0
                        self%diag_pivots(kb, i) = kb
                    end do
                end if
            end do

            ! Allocate workspace
            if (allocated(self%work)) deallocate (self%work)
            allocate (self%work(n * bs))

            self%status = SOLVER_STATUS%SUCCESS%ID

        class default
            self%status = SOLVER_STATUS%NOT_IMPLEMENTED%ID
        end select

    end subroutine setup_preconditioner_is

    !> Apply I+S: z = (2I - D^{-1}A) r
    !> Step 1: work = A*r (SpMV)
    !> Step 2: work = D^{-1} * work (block solves)
    !> Step 3: z = 2*r - work
    module subroutine apply_preconditioner_is(self, r, z)
        implicit none
        class(type_preconditioner_is), intent(inout) :: self
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

        ! Step 1: work = A * r (block SpMV)
        self%work(:) = 0.0d0
        do i = 1, n
            idx_i = (i - 1) * bs
            do k = self%a_ptr(i), self%a_ptr(i + 1) - 1
                col = self%a_ind(k)
                idx_c = (col - 1) * bs
                do jj = 1, bs
                    do ii = 1, bs
                        self%work(idx_i + ii) = self%work(idx_i + ii) + &
                            self%a_val(ii, jj, k) * r_data(idx_c + jj)
                    end do
                end do
            end do
        end do

        ! Step 2: work = D^{-1} * work (solve diagonal blocks)
        do i = 1, n
            idx_i = (i - 1) * bs + 1
            call dgetrs('N', bs, 1, self%diag_blocks(:, :, i), bs, &
                        self%diag_pivots(:, i), self%work(idx_i), bs, ierr)
        end do

        ! Step 3: z = 2*r - work
        do i = 1, n * bs
            z_data(i) = 2.0d0 * r_data(i) - self%work(i)
        end do

    end subroutine apply_preconditioner_is

    !> Deallocate all resources.
    module subroutine destroy_preconditioner_is(self)
        implicit none
        class(type_preconditioner_is), intent(inout) :: self

        if (allocated(self%diag_blocks)) deallocate (self%diag_blocks)
        if (allocated(self%diag_pivots)) deallocate (self%diag_pivots)
        if (allocated(self%diag_ptr)) deallocate (self%diag_ptr)
        if (allocated(self%work)) deallocate (self%work)
        if (allocated(self%name)) deallocate (self%name)
        nullify (self%a_ptr)
        nullify (self%a_ind)
        nullify (self%a_val)
        self%ID = -1
        self%num_rows = -1
        self%is_block = .false.

    end subroutine destroy_preconditioner_is

end submodule solver_preconditioner_is
