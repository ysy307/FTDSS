submodule(solver_preconditioner) solver_preconditioner_jacobi

contains

    !> Initialize the Jacobi preconditioner instance.
    !> It determines whether to use Point Jacobi or Block Jacobi based on the provided settings.
    module subroutine initialize_preconditioner_jacobi(self, info)
        implicit none
        !> Preconditioner instance to be initialized
        class(type_preconditioner_jacobi), intent(inout) :: self
        !> Settings defining the structure of the system (e.g., block size)
        type(type_preconditioner_settings), intent(in) :: info

        self%name = "Jacobi"
        self%id = SOLVER_PRECONDITION_JACOBI

        if (info%num_nodes <= 0) then
            self%status = SOLVER_STATUS_ILL_OPTIONS
            return
        else
            ! Stored as the number of blocks (rows)
            self%num_nodes = info%num_nodes
        end if

        ! Assumed to be info%block_size (or dofs per node).
        if (info%block_size > 1) then
            self%is_block = .true.
            self%block_size = info%block_size

            ! (block_size, block_size, num_nodes)
            call allocate_array(self%M_inv_blocks, self%block_size, self%block_size, self%num_nodes)
            ! (block_size, num_nodes)
            call allocate_array(self%ipiv_blocks, self%block_size, self%num_nodes)
        else
            self%is_block = .false.
            self%block_size = 1
            call self%M_inv%initialize(self%num_nodes)
            call self%M_inv%zero()
        end if

        self%status = SOLVER_STATUS_SUCCESS
    end subroutine initialize_preconditioner_jacobi

    !> Setup the preconditioner for the specific matrix \( A \).
    !> It dispatches the setup routine to either point or block implementations.
    module subroutine setup_preconditioner_jacobi(self, A)
        implicit none
        !> Preconditioner instance
        class(type_preconditioner_jacobi), intent(inout) :: self
        !> System matrix \( A \)
        class(abst_matrix), intent(in) :: A

        select type (A)
        type is (type_matrix_dense)
            call self%setup_point(A)
        type is (type_matrix_coo)
            call self%setup_point(A)
        type is (type_matrix_csr)
            call self%setup_point(A)
        type is (type_matrix_bsr)
            ! Block processing only if BSR and block size > 1
            if (self%is_block) then
                call self%setup_block(A)
            else
                ! Treat as scalar even if BSR (rare case)
                call self%setup_point(A)
            end if
        class default
            self%status = SOLVER_STATUS_NOT_IMPLEMENTED
        end select
    end subroutine setup_preconditioner_jacobi

    !> Setup the Point Jacobi preconditioner.
    !> It computes and stores the reciprocal of the diagonal elements of \( A \).
    module subroutine setup_preconditioner_jacobi_point(self, A)
        implicit none
        !> Preconditioner instance
        class(type_preconditioner_jacobi), intent(inout) :: self
        !> System matrix
        class(abst_matrix), intent(in) :: A

        ! Point Jacobi: Reciprocal of diagonal elements
        call A%get_diagonal(self%M_inv)
        call vector_reciprocal(self%M_inv)
        self%status = SOLVER_STATUS_SUCCESS
    end subroutine setup_preconditioner_jacobi_point

    !> Setup the Block Jacobi preconditioner.
    !> It extracts diagonal blocks and performs LU factorization for each block.
    module subroutine setup_preconditioner_jacobi_block(self, A)
        implicit none
        !> Preconditioner instance
        class(type_preconditioner_jacobi), intent(inout) :: self
        !> System matrix (Must be BSR)
        class(abst_matrix), intent(in) :: A

        integer(int32) :: i, ierr, bs

        bs = self%block_size

        select type (A)
        type is (type_matrix_bsr)
            !$omp parallel do private(i, ierr)
            do i = 1, self%num_nodes
                ! 1. Extract diagonal block A(i,i) -> M_inv_blocks(:,:,i)
                call A%get_diagonal_block(i, self%M_inv_blocks(:, :, i))

                ! 2. LU decomposition (dgetrf)
                ! M = P * L * U
                call dgetrf(bs, bs, self%M_inv_blocks(:, :, i), bs, &
                            self%ipiv_blocks(:, i), ierr)

                ! Error handling in parallel loops requires care.
                ! Here we check locally and set the status flag.
                if (ierr /= 0) then
                    ! Singular matrix found
                    self%status = SOLVER_STATUS_DECOMPOSITION_FAILURE
                end if
            end do
            !$omp end parallel do
        end select

        if (self%status /= SOLVER_STATUS_DECOMPOSITION_FAILURE) then
            self%status = SOLVER_STATUS_SUCCESS
        end if
    end subroutine setup_preconditioner_jacobi_block

    !> Apply the preconditioner to the vector \( r \), producing \( z \).
    !> Solves \( Mz = r \), where \( M \) is the diagonal (or block-diagonal) of \( A \).
    module subroutine apply_preconditioner_jacobi(self, r, z)
        implicit none
        !> Preconditioner instance
        class(type_preconditioner_jacobi), intent(inout) :: self
        !> Input vector \( r \)
        type(type_vector_dp), intent(in) :: r
        !> Output vector \( z \) (result of \( M^{-1}r \))
        type(type_vector_dp), intent(inout) :: z

        real(real64), dimension(:), pointer :: z_ptr
        integer(int32) :: i, idx_s, idx_e, ierr, bs

        if (self%is_block) then
            ! ==========================================================
            ! Block Jacobi (LU solve)
            ! ==========================================================
            bs = self%block_size
            z_ptr => z%get_data()

            ! First copy r to z (initialize z as RHS b)
            call z%copy(r)

            !$omp parallel do private(i, idx_s, idx_e, ierr)
            do i = 1, self%num_nodes
                idx_s = (i - 1) * bs + 1
                idx_e = i * bs

                ! 3. Solve (dgetrs)
                ! z = M^-1 * z  (M is LU decomposed)
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

        self%status = SOLVER_STATUS_SUCCESS
    end subroutine apply_preconditioner_jacobi

    !> Finalize the preconditioner instance and release memory.
    module subroutine destroy_preconditioner_jacobi(self)
        implicit none
        !> Preconditioner instance to be destroyed
        class(type_preconditioner_jacobi), intent(inout) :: self

        self%id = -1
        if (allocated(self%name)) deallocate (self%name)

        if (self%is_block) then
            call deallocate_array(self%M_inv_blocks)
            call deallocate_array(self%ipiv_blocks)
        else
            call self%M_inv%destroy()
        end if

        self%num_nodes = -1
        self%block_size = -1
        self%is_block = .false.

        self%status = SOLVER_STATUS_SUCCESS
    end subroutine destroy_preconditioner_jacobi

end submodule solver_preconditioner_jacobi
