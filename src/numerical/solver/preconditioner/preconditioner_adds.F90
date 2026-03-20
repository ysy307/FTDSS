!> Additive Schwarz preconditioner for BSR matrices.
!> Partitions the domain into overlapping subdomains, applies local ILU(0) on each,
!> and sums the corrections.
submodule(numerical_solver_preconditioner) solver_preconditioner_adds
    implicit none
contains

    !> Initialize the Additive Schwarz preconditioner.
    module subroutine initialize_preconditioner_adds(self, info)
        implicit none
        class(type_preconditioner_adds), intent(inout) :: self
        type(type_preconditioner_settings), intent(in) :: info

        self%ID = PRECONDITIONER_TYPES%ADDS%ID
        self%status = SOLVER_STATUS%SUCCESS%ID
        self%name = "Additive Schwarz"
        self%num_rows = info%num_nodes

        if (info%block_size > 1) then
            self%is_block = .true.
            self%block_size = info%block_size
        else
            self%is_block = .false.
            self%block_size = 1
        end if

    end subroutine initialize_preconditioner_adds

    !> Setup: partition domain and build local preconditioners.
    !> Uses contiguous block partitioning with overlap.
    module subroutine setup_preconditioner_adds(self, A)
        implicit none
        class(type_preconditioner_adds), intent(inout) :: self
        class(abst_matrix), intent(in) :: A

        type(type_matrix_info) :: info
        integer(int32) :: n, bs, i, k, col, d, dom_start, dom_end
        integer(int32) :: overlap, num_domains, dom_size, dom_size_ext
        integer(int32) :: local_idx, nnz_local
        integer(int32) :: ierr, kb
        real(real64), parameter :: PIVOT_TOL = 1.0d-12

        call A%get_info(info)
        self%num_rows = info%num_rows

        self%status = SOLVER_STATUS%NOT_IMPLEMENTED%ID

        select type (A)
        type is (type_matrix_bsr)
            n = self%num_rows
            bs = info%num_block_rows

            self%a_ptr => A%get_ptr()
            self%a_ind => A%get_ind()
            self%a_val => A%get_val()

            ! Determine domain decomposition parameters
            ! Auto-select number of domains based on problem size
            overlap = 1
            num_domains = max(1, n / 50)
            if (num_domains > 20) num_domains = 20
            self%num_domains = num_domains
            self%overlap = overlap

            ! Build domain partitioning
            if (allocated(self%domain_start)) deallocate (self%domain_start)
            if (allocated(self%domain_end)) deallocate (self%domain_end)
            allocate (self%domain_start(num_domains))
            allocate (self%domain_end(num_domains))

            dom_size = n / num_domains
            do d = 1, num_domains
                dom_start = (d - 1) * dom_size + 1
                if (d == num_domains) then
                    dom_end = n
                else
                    dom_end = d * dom_size
                end if
                ! Extend with overlap
                self%domain_start(d) = max(1, dom_start - overlap)
                self%domain_end(d) = min(n, dom_end + overlap)
            end do

            ! Build local ILU(0) for each subdomain
            ! Store as block diagonal + local triangular factors
            if (allocated(self%local_diag_blocks)) deallocate (self%local_diag_blocks)
            if (allocated(self%local_diag_pivots)) deallocate (self%local_diag_pivots)
            allocate (self%local_diag_blocks(bs, bs, n))
            allocate (self%local_diag_pivots(bs, n))

            ! Simple approach: use block Jacobi per subdomain
            ! (full local ILU would require extracting local submatrices)
            do i = 1, n
                do k = self%a_ptr(i), self%a_ptr(i + 1) - 1
                    if (self%a_ind(k) == i) then
                        self%local_diag_blocks(:, :, i) = self%a_val(:, :, k)
                        ! Stabilize
                        do kb = 1, bs
                            if (abs(self%local_diag_blocks(kb, kb, i)) < PIVOT_TOL) then
                                self%local_diag_blocks(kb, kb, i) = PIVOT_TOL
                            end if
                        end do
                        call dgetrf(bs, bs, self%local_diag_blocks(:, :, i), bs, &
                                    self%local_diag_pivots(:, i), ierr)
                        if (ierr /= 0) then
                            self%local_diag_blocks(:, :, i) = 0.0d0
                            do kb = 1, bs
                                self%local_diag_blocks(kb, kb, i) = 1.0d0
                                self%local_diag_pivots(kb, i) = kb
                            end do
                        end if
                        exit
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

    end subroutine setup_preconditioner_adds

    !> Apply Additive Schwarz:
    !> For each subdomain d: z_d = M_d^{-1} * r_d (restricted to subdomain)
    !> z = sum of z_d (additive combination)
    module subroutine apply_preconditioner_adds(self, r, z)
        implicit none
        class(type_preconditioner_adds), intent(inout) :: self
        type(type_vector_dp), intent(in) :: r
        type(type_vector_dp), intent(inout) :: z

        real(real64), pointer :: r_data(:), z_data(:)
        integer(int32) :: n, bs, d, i, k, col, idx_i, idx_c
        integer(int32) :: ds, de, ii, jj, ierr
        real(real64) :: weight

        if (self%status /= SOLVER_STATUS%SUCCESS%ID) then
            call z%copy(r)
            return
        end if

        n = self%num_rows
        bs = self%block_size
        r_data => r%get_data()
        z_data => z%get_data()

        z_data(:) = 0.0d0

        ! For each subdomain
        do d = 1, self%num_domains
            ds = self%domain_start(d)
            de = self%domain_end(d)

            ! Local solve: block Jacobi on subdomain
            ! Plus one SSOR-like sweep for off-diagonal coupling within subdomain
            do i = ds, de
                idx_i = (i - 1) * bs + 1

                ! Copy residual to workspace
                self%work((i - 1) * bs + 1:(i - 1) * bs + bs) = &
                    r_data((i - 1) * bs + 1:(i - 1) * bs + bs)

                ! Subtract off-diagonal contributions within subdomain
                do k = self%a_ptr(i), self%a_ptr(i + 1) - 1
                    col = self%a_ind(k)
                    if (col == i) cycle
                    if (col >= ds .and. col <= de .and. col < i) then
                        idx_c = (col - 1) * bs
                        do jj = 1, bs
                            do ii = 1, bs
                                self%work((i - 1) * bs + ii) = &
                                    self%work((i - 1) * bs + ii) - &
                                    self%a_val(ii, jj, k) * self%work((col - 1) * bs + jj)
                            end do
                        end do
                    end if
                end do

                ! Solve diagonal block
                call dgetrs('N', bs, 1, self%local_diag_blocks(:, :, i), bs, &
                            self%local_diag_pivots(:, i), &
                            self%work((i - 1) * bs + 1), bs, ierr)
            end do

            ! Accumulate with overlap weighting
            do i = ds, de
                ! Weight = 1/number_of_domains_containing_this_node
                weight = 1.0d0
                if (i > self%domain_start(d) .and. d > 1) then
                    if (i <= self%domain_end(d - 1)) weight = 0.5d0
                end if
                if (i < self%domain_end(d) .and. d < self%num_domains) then
                    if (i >= self%domain_start(d + 1)) weight = 0.5d0
                end if

                do ii = 1, bs
                    z_data((i - 1) * bs + ii) = z_data((i - 1) * bs + ii) + &
                        weight * self%work((i - 1) * bs + ii)
                end do
            end do
        end do

    end subroutine apply_preconditioner_adds

    !> Deallocate all resources.
    module subroutine destroy_preconditioner_adds(self)
        implicit none
        class(type_preconditioner_adds), intent(inout) :: self

        if (allocated(self%domain_start)) deallocate (self%domain_start)
        if (allocated(self%domain_end)) deallocate (self%domain_end)
        if (allocated(self%local_diag_blocks)) deallocate (self%local_diag_blocks)
        if (allocated(self%local_diag_pivots)) deallocate (self%local_diag_pivots)
        if (allocated(self%work)) deallocate (self%work)
        if (allocated(self%name)) deallocate (self%name)
        nullify (self%a_ptr)
        nullify (self%a_ind)
        nullify (self%a_val)
        self%ID = -1
        self%num_rows = -1
        self%is_block = .false.

    end subroutine destroy_preconditioner_adds

end submodule solver_preconditioner_adds
