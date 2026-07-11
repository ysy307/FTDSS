submodule(numerical_solver_preconditioner) solver_preconditioner_jacobi
    implicit none

contains

    !> Initialize the Jacobi preconditioner instance.
    module subroutine initialize_preconditioner_jacobi(self, info)
        implicit none
        class(type_preconditioner_jacobi), intent(inout) :: self
        type(type_preconditioner_settings), intent(in) :: info

        self%name = "Jacobi"
        self%ID = PRECONDITIONER_TYPES%JACOBI%ID
        self%status = SOLVER_STATUS%SUCCESS%ID

        ! Use the node count set at initialization as authoritative.
        ! The setup routine does not modify this value.
        self%num_nodes = info%num_nodes

        if (info%block_size > 1) then
            self%is_block = .true.
            self%block_size = info%block_size
        else
            self%is_block = .false.
            self%block_size = 1
        end if

    end subroutine initialize_preconditioner_jacobi

    !> Set up the preconditioner for matrix A.
    module subroutine setup_preconditioner_jacobi(self, A)
        implicit none
        class(type_preconditioner_jacobi), intent(inout) :: self
        class(abst_matrix), intent(in) :: A

        ! Removed resize logic; trust num_nodes set in initialize

        self%status = SOLVER_STATUS%NOT_IMPLEMENTED%ID

        select type (A)
        type is (type_matrix_dense)
            self%is_block = .false.
            call self%setup_point(A)

        type is (type_matrix_csr)
            self%is_block = .false.
            call self%setup_point(A)

        type is (type_matrix_coo)
            self%is_block = .false.
            call self%setup_point(A)

        type is (type_matrix_bsr)
            if (self%is_block .and. self%block_size > 1) then
                call self%setup_block(A)
            else
                self%is_block = .false.
                call self%setup_point(A)
            end if

        class default
            write (*, *) "Error: Jacobi preconditioner does not support this matrix type."
            self%status = SOLVER_STATUS%NOT_IMPLEMENTED%ID
        end select

    end subroutine setup_preconditioner_jacobi

    !> Set up Point Jacobi preconditioner.
    module subroutine setup_preconditioner_jacobi_point(self, A)
        implicit none
        class(type_preconditioner_jacobi), intent(inout) :: self
        class(abst_matrix), intent(in) :: A

        ! Allocate memory using the size set at initialization
        if (self%M_inv%get_size() /= self%num_nodes) then
            call self%M_inv%initialize(self%num_nodes)
        end if

        call self%M_inv%zero()

        ! Extract diagonal entries
        call A%get_diagonal(self%M_inv)

        ! Compute reciprocals (division-by-zero protection handled in linalg)
        call vector_reciprocal(self%M_inv)

        self%status = SOLVER_STATUS%SUCCESS%ID
    end subroutine setup_preconditioner_jacobi_point

    !> Set up Block Jacobi preconditioner.
    module subroutine setup_preconditioner_jacobi_block(self, A)
        implicit none
        class(type_preconditioner_jacobi), intent(inout) :: self
        class(abst_matrix), intent(in) :: A

        integer(int32) :: i, ierr, bs, num_blocks, kb

        bs = self%block_size
        ! Number of blocks = total DOFs / block_size
        num_blocks = self%num_nodes / bs

        select type (A)
        type is (type_matrix_bsr)
            if (allocated(self%M_inv_blocks)) then
                if (any(shape(self%M_inv_blocks) /= [bs, bs, num_blocks])) deallocate (self%M_inv_blocks)
            end if
            if (allocated(self%ipiv_blocks)) then
                if (any(shape(self%ipiv_blocks) /= [bs, num_blocks])) deallocate (self%ipiv_blocks)
            end if
            if (.not. allocated(self%M_inv_blocks)) allocate (self%M_inv_blocks(bs, bs, num_blocks))
            if (.not. allocated(self%ipiv_blocks)) allocate (self%ipiv_blocks(bs, num_blocks))

            !$omp parallel do private(i, ierr)
            do i = 1, num_blocks
                call A%get_diagonal_block(i, self%M_inv_blocks(:, :, i))
                call dgetrf(bs, bs, self%M_inv_blocks(:, :, i), bs, &
                            self%ipiv_blocks(:, i), ierr)
                if (ierr /= 0) then
                    ! Fallback to identity for singular blocks
                    self%M_inv_blocks(:, :, i) = 0.0d0
                    do kb = 1, bs
                        self%M_inv_blocks(kb, kb, i) = 1.0d0
                        self%ipiv_blocks(kb, i) = kb
                    end do
                end if
            end do
            !$omp end parallel do

            self%status = SOLVER_STATUS%SUCCESS%ID

        class default
            self%status = SOLVER_STATUS%NOT_IMPLEMENTED%ID
        end select

    end subroutine setup_preconditioner_jacobi_block

    !> Apply the preconditioner: z = M^{-1} * r
    module subroutine apply_preconditioner_jacobi(self, r, z)
        implicit none
        class(type_preconditioner_jacobi), intent(inout) :: self
        type(type_vector_dp), intent(in) :: r
        type(type_vector_dp), intent(inout) :: z

        real(real64), dimension(:), pointer :: z_ptr
        integer(int32) :: i, idx_s, idx_e, ierr, bs, num_blocks

        if (self%status /= SOLVER_STATUS%SUCCESS%ID) then
            call z%copy(r)
            return
        end if

        if (self%is_block) then
            ! ==========================================================
            ! Block Jacobi (LU solve)
            ! ==========================================================
            bs = self%block_size
            num_blocks = self%num_nodes / bs

            call z%copy(r)
            z_ptr => z%get_data()

            !$omp parallel do private(i, idx_s, idx_e, ierr)
            do i = 1, num_blocks
                idx_s = (i - 1) * bs + 1
                idx_e = i * bs

                call dgetrs('N', bs, 1, &
                            self%M_inv_blocks(:, :, i), bs, &
                            self%ipiv_blocks(:, i), &
                            z_ptr(idx_s:idx_e), bs, ierr)
            end do
            !$omp end parallel do

        else
            ! ==========================================================
            ! Point Jacobi (Scalar scaling)
            ! ==========================================================
            call multiply(self%M_inv, r, z)

        end if

    end subroutine apply_preconditioner_jacobi

    !> Deallocate all resources.
    module subroutine destroy_preconditioner_jacobi(self)
        implicit none
        class(type_preconditioner_jacobi), intent(inout) :: self

        self%ID = -1
        if (allocated(self%name)) deallocate (self%name)

        if (allocated(self%M_inv_blocks)) deallocate (self%M_inv_blocks)
        if (allocated(self%ipiv_blocks)) deallocate (self%ipiv_blocks)

        call self%M_inv%destroy()

        self%num_nodes = -1
        self%block_size = -1
        self%is_block = .false.
        self%status = SOLVER_STATUS%SUCCESS%ID

    end subroutine destroy_preconditioner_jacobi

end submodule solver_preconditioner_jacobi
