submodule(numerical_solver_preconditioner) solver_preconditioner_ssor
    implicit none
contains

    !> Initialize the SSOR preconditioner instance.
    module subroutine initialize_preconditioner_ssor(self, info)
        implicit none
        class(type_preconditioner_ssor), intent(inout) :: self
        type(type_preconditioner_settings), intent(in) :: info

        self%name = "SSOR"
        self%ID = PRECONDITIONER_TYPES%SSOR%ID
        self%status = SOLVER_STATUS%SUCCESS%ID
        self%num_nodes = info%num_nodes

        if (info%block_size > 1) then
            self%is_block = .true.
            self%block_size = info%block_size
        else
            self%is_block = .false.
            self%block_size = 1
        end if

        self%omega = info%ssor_omega

    end subroutine initialize_preconditioner_ssor

    !> Set up the SSOR preconditioner for matrix A.
    !> Extracts and LU-factorizes diagonal blocks for block SSOR,
    !> or extracts diagonal values for point SSOR.
    module subroutine setup_preconditioner_ssor(self, A)
        implicit none
        class(type_preconditioner_ssor), intent(inout) :: self
        class(abst_matrix), intent(in) :: A

        type(type_matrix_info) :: mat_info
        integer(int32) :: i, k, bs, ierr

        self%status = SOLVER_STATUS%NOT_IMPLEMENTED%ID

        select type (A)
        type is (type_matrix_bsr)
            call A%get_info(mat_info)
            self%num_rows = mat_info%num_nodes
            bs = mat_info%num_block_rows

            ! Store sparsity structure pointers
            self%a_ptr => A%get_ptr()
            self%a_ind => A%get_ind()
            self%a_val => A%get_val()

            if (self%is_block .and. bs > 1) then
                ! Block SSOR: extract and LU-factorize diagonal blocks
                if (allocated(self%diag_blocks)) deallocate (self%diag_blocks)
                if (allocated(self%diag_pivots)) deallocate (self%diag_pivots)
                if (allocated(self%diag_ptr)) deallocate (self%diag_ptr)

                allocate (self%diag_blocks(bs, bs, self%num_rows))
                allocate (self%diag_pivots(bs, self%num_rows))
                allocate (self%diag_ptr(self%num_rows))

                do i = 1, self%num_rows
                    ! Find diagonal block position
                    do k = self%a_ptr(i), self%a_ptr(i + 1) - 1
                        if (self%a_ind(k) == i) then
                            self%diag_ptr(i) = k
                            exit
                        end if
                    end do

                    ! Copy and scale diagonal block: D/omega
                    self%diag_blocks(:, :, i) = self%a_val(:, :, self%diag_ptr(i)) / self%omega
                    call dgetrf(bs, bs, self%diag_blocks(:, :, i), bs, &
                                self%diag_pivots(:, i), ierr)
                    if (ierr /= 0) then
                        self%diag_blocks(:, :, i) = 0.0d0
                        do concurrent(integer(int32) :: kb=1:bs)
                            self%diag_blocks(kb, kb, i) = 1.0d0
                            self%diag_pivots(kb, i) = kb
                        end do
                    end if
                end do
            else
                ! Point SSOR: extract diagonal values
                self%is_block = .false.
                if (self%diag_vals%get_size() /= self%num_rows * bs) then
                    call self%diag_vals%initialize(self%num_rows * bs)
                end if
                call A%get_diagonal(self%diag_vals)

                ! Find diagonal block positions
                if (allocated(self%diag_ptr)) deallocate (self%diag_ptr)
                allocate (self%diag_ptr(self%num_rows))
                do i = 1, self%num_rows
                    do k = self%a_ptr(i), self%a_ptr(i + 1) - 1
                        if (self%a_ind(k) == i) then
                            self%diag_ptr(i) = k
                            exit
                        end if
                    end do
                end do
            end if

            self%status = SOLVER_STATUS%SUCCESS%ID

        class default
            self%status = SOLVER_STATUS%NOT_IMPLEMENTED%ID
        end select

    end subroutine setup_preconditioner_ssor

    !> Apply the SSOR preconditioner: z = M_SSOR^{-1} * r
    !>
    !> SSOR consists of a forward SOR sweep followed by a backward SOR sweep:
    !> Forward:  (D/ω + L) y = r
    !> Backward: (D/ω + U) z = (D/ω) y
    module subroutine apply_preconditioner_ssor(self, r, z)
        implicit none
        class(type_preconditioner_ssor), intent(inout) :: self
        type(type_vector_dp), intent(in) :: r
        type(type_vector_dp), intent(inout) :: z

        if (self%status /= SOLVER_STATUS%SUCCESS%ID) then
            call z%copy(r)
            return
        end if

        if (self%is_block) then
            call self%apply_block(r, z)
        else
            call self%apply_point(r, z)
        end if

    end subroutine apply_preconditioner_ssor

    !> Apply point SSOR for scalar or BSR with block_size=1.
    module subroutine apply_point_ssor(self, r, z)
        implicit none
        class(type_preconditioner_ssor), intent(inout) :: self
        type(type_vector_dp), intent(in) :: r
        type(type_vector_dp), intent(inout) :: z

        real(real64), dimension(:), pointer :: x, d
        real(real64) :: omega, sum_val, diag_val
        integer(int32) :: i, k, col, n

        omega = self%omega
        n = self%num_rows
        call z%copy(r)
        x => z%get_data()
        d => self%diag_vals%get_data()

        ! Forward sweep: (D/ω + L) y = r
        do i = 1, n
            sum_val = 0.0d0
            do k = self%a_ptr(i), self%diag_ptr(i) - 1
                col = self%a_ind(k)
                sum_val = sum_val + self%a_val(1, 1, k) * x(col)
            end do
            diag_val = d(i)
            if (abs(diag_val) > tiny(1.0d0)) then
                x(i) = omega * (x(i) - sum_val) / diag_val
            end if
        end do

        ! Multiply by D/ω: y <- (D/ω) * y
        do i = 1, n
            x(i) = d(i) / omega * x(i)
        end do

        ! Backward sweep: (D/ω + U) z = y
        do i = n, 1, -1
            sum_val = 0.0d0
            do k = self%diag_ptr(i) + 1, self%a_ptr(i + 1) - 1
                col = self%a_ind(k)
                sum_val = sum_val + self%a_val(1, 1, k) * x(col)
            end do
            diag_val = d(i)
            if (abs(diag_val) > tiny(1.0d0)) then
                x(i) = omega * (x(i) - sum_val) / diag_val
            end if
        end do

    end subroutine apply_point_ssor

    !> Apply block SSOR for BSR matrices.
    !>
    !> Forward:  (D/ω + L) y = r   where L is strictly lower block triangular
    !> Middle:   y <- (D/ω) y
    !> Backward: (D/ω + U) z = y   where U is strictly upper block triangular
    module subroutine apply_block_ssor(self, r, z)
        implicit none
        class(type_preconditioner_ssor), intent(inout) :: self
        type(type_vector_dp), intent(in) :: r
        type(type_vector_dp), intent(inout) :: z

        real(real64), dimension(:), pointer :: x
        real(real64) :: tmp(self%block_size)
        integer(int32) :: i, k, col, bs, idx_i, idx_c, ierr

        bs = self%block_size
        call z%copy(r)
        x => z%get_data()

        ! Forward sweep: solve (D/ω + L) y = r
        do i = 1, self%num_rows
            idx_i = (i - 1) * bs + 1
            ! Subtract L * y (lower triangular blocks)
            do k = self%a_ptr(i), self%diag_ptr(i) - 1
                col = self%a_ind(k)
                idx_c = (col - 1) * bs + 1
                call dgemv('N', bs, bs, -1.0d0, &
                           self%a_val(:, :, k), bs, &
                           x(idx_c), 1, 1.0d0, &
                           x(idx_i), 1)
            end do
            ! Solve D/ω block: (D/ω) y_i = rhs_i
            call dgetrs('N', bs, 1, &
                        self%diag_blocks(:, :, i), bs, &
                        self%diag_pivots(:, i), &
                        x(idx_i), bs, ierr)
        end do

        ! Middle: multiply by D/ω
        ! After forward sweep: y = (D/ω + L)^{-1} r
        ! We need: y' = (D/ω) y
        ! Use tmp buffer because dgemv does not support in-place operation
        do i = 1, self%num_rows
            idx_i = (i - 1) * bs + 1
            call dgemv('N', bs, bs, 1.0d0 / self%omega, &
                       self%a_val(:, :, self%diag_ptr(i)), bs, &
                       x(idx_i), 1, 0.0d0, &
                       tmp, 1)
            x(idx_i:idx_i + bs - 1) = tmp(1:bs)
        end do

        ! Backward sweep: solve (D/ω + U) z = y'
        do i = self%num_rows, 1, -1
            idx_i = (i - 1) * bs + 1
            ! Subtract U * z (upper triangular blocks)
            do k = self%diag_ptr(i) + 1, self%a_ptr(i + 1) - 1
                col = self%a_ind(k)
                idx_c = (col - 1) * bs + 1
                call dgemv('N', bs, bs, -1.0d0, &
                           self%a_val(:, :, k), bs, &
                           x(idx_c), 1, 1.0d0, &
                           x(idx_i), 1)
            end do
            ! Solve D/ω block
            call dgetrs('N', bs, 1, &
                        self%diag_blocks(:, :, i), bs, &
                        self%diag_pivots(:, i), &
                        x(idx_i), bs, ierr)
        end do

    end subroutine apply_block_ssor

    !> Deallocate all resources.
    module subroutine destroy_preconditioner_ssor(self)
        implicit none
        class(type_preconditioner_ssor), intent(inout) :: self

        self%ID = -1
        if (allocated(self%name)) deallocate (self%name)
        if (allocated(self%diag_blocks)) deallocate (self%diag_blocks)
        if (allocated(self%diag_pivots)) deallocate (self%diag_pivots)
        if (allocated(self%diag_ptr)) deallocate (self%diag_ptr)
        call self%diag_vals%destroy()
        nullify (self%a_ptr)
        nullify (self%a_ind)
        nullify (self%a_val)
        self%num_nodes = -1
        self%num_rows = -1
        self%block_size = -1
        self%is_block = .false.
        self%omega = 1.0d0
        self%status = SOLVER_STATUS%SUCCESS%ID

    end subroutine destroy_preconditioner_ssor

end submodule solver_preconditioner_ssor
