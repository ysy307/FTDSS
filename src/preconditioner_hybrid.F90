!> Hybrid preconditioner: combines two preconditioners.
!> Apply: z = M2^{-1} * r + M1^{-1} * (r - A * M2^{-1} * r)
!> Simplified: applies inner preconditioner, then outer preconditioner on residual.
!> Default: inner = Jacobi, outer = ILU(0).
submodule(numerical_solver_preconditioner) solver_preconditioner_hybrid
    implicit none
contains

    !> Initialize the Hybrid preconditioner.
    module subroutine initialize_preconditioner_hybrid(self, info)
        implicit none
        class(type_preconditioner_hybrid), intent(inout) :: self
        type(type_preconditioner_settings), intent(in) :: info

        type(type_preconditioner_settings) :: inner_info, outer_info

        self%ID = PRECONDITIONER_TYPES%HYBRID%ID
        self%status = SOLVER_STATUS%SUCCESS%ID
        self%name = "Hybrid"
        self%num_rows = info%num_nodes

        if (info%block_size > 1) then
            self%is_block = .true.
            self%block_size = info%block_size
        else
            self%is_block = .false.
            self%block_size = 1
        end if

        ! Create inner preconditioner (SSOR)
        call inner_info%set(PRECONDITIONER_TYPES%SSOR%ID, info%num_nodes, info%block_size)
        call create_preconditioner(self%inner_pc, inner_info, self%status)

        ! Create outer preconditioner (ILU(0))
        call outer_info%set(PRECONDITIONER_TYPES%ILU%ID, info%num_nodes, info%block_size)
        call create_preconditioner(self%outer_pc, outer_info, self%status)

    end subroutine initialize_preconditioner_hybrid

    !> Setup both inner and outer preconditioners.
    module subroutine setup_preconditioner_hybrid(self, A)
        implicit none
        class(type_preconditioner_hybrid), intent(inout) :: self
        class(abst_matrix), intent(in) :: A

        type(type_matrix_info) :: info

        call A%get_info(info)
        self%num_rows = info%num_rows

        self%status = SOLVER_STATUS%NOT_IMPLEMENTED%ID

        select type (A)
        type is (type_matrix_bsr)
            ! Store matrix pointers for residual computation
            self%a_ptr => A%get_ptr()
            self%a_ind => A%get_ind()
            self%a_val => A%get_val()

            if (allocated(self%inner_pc)) call self%inner_pc%setup(A)
            if (allocated(self%outer_pc)) call self%outer_pc%setup(A)

            ! Allocate workspace
            if (self%work1%get_size() /= self%num_rows * self%block_size) then
                call self%work1%initialize(self%num_rows * self%block_size)
            end if
            if (self%work2%get_size() /= self%num_rows * self%block_size) then
                call self%work2%initialize(self%num_rows * self%block_size)
            end if

            self%status = SOLVER_STATUS%SUCCESS%ID

        class default
            self%status = SOLVER_STATUS%NOT_IMPLEMENTED%ID
        end select

    end subroutine setup_preconditioner_hybrid

    !> Apply hybrid: z = M_outer^{-1}(r) + M_inner^{-1}(r - A*M_outer^{-1}(r))
    module subroutine apply_preconditioner_hybrid(self, r, z)
        implicit none
        class(type_preconditioner_hybrid), intent(inout) :: self
        type(type_vector_dp), intent(in) :: r
        type(type_vector_dp), intent(inout) :: z

        real(real64), pointer :: r_data(:), z_data(:), w1(:), w2(:)
        integer(int32) :: n, bs, i, k, col, idx_i, idx_c, ii, jj, ierr

        if (self%status /= SOLVER_STATUS%SUCCESS%ID .or. &
            .not. allocated(self%outer_pc) .or. .not. allocated(self%inner_pc)) then
            call z%copy(r)
            return
        end if

        n = self%num_rows
        bs = self%block_size

        ! Step 1: z = M_outer^{-1} * r
        call self%outer_pc%apply(r, z)

        ! Step 2: work1 = A * z (SpMV)
        w1 => self%work1%get_data()
        w1(:) = 0.0d0
        z_data => z%get_data()
        r_data => r%get_data()

        do i = 1, n
            idx_i = (i - 1) * bs
            do k = self%a_ptr(i), self%a_ptr(i + 1) - 1
                col = self%a_ind(k)
                idx_c = (col - 1) * bs
                do jj = 1, bs
                    do ii = 1, bs
                        w1(idx_i + ii) = w1(idx_i + ii) + &
                            self%a_val(ii, jj, k) * z_data(idx_c + jj)
                    end do
                end do
            end do
        end do

        ! Step 3: work1 = r - A*z (residual)
        do i = 1, n * bs
            w1(i) = r_data(i) - w1(i)
        end do

        ! Step 4: work2 = M_inner^{-1} * work1
        call self%inner_pc%apply(self%work1, self%work2)

        ! Step 5: z = z + work2
        w2 => self%work2%get_data()
        do i = 1, n * bs
            z_data(i) = z_data(i) + w2(i)
        end do

    end subroutine apply_preconditioner_hybrid

    !> Deallocate all resources.
    module subroutine destroy_preconditioner_hybrid(self)
        implicit none
        class(type_preconditioner_hybrid), intent(inout) :: self

        if (allocated(self%inner_pc)) then
            call self%inner_pc%destroy()
            deallocate (self%inner_pc)
        end if
        if (allocated(self%outer_pc)) then
            call self%outer_pc%destroy()
            deallocate (self%outer_pc)
        end if
        call self%work1%destroy()
        call self%work2%destroy()
        if (allocated(self%name)) deallocate (self%name)
        nullify (self%a_ptr)
        nullify (self%a_ind)
        nullify (self%a_val)
        self%ID = -1
        self%num_rows = -1
        self%is_block = .false.

    end subroutine destroy_preconditioner_hybrid

end submodule solver_preconditioner_hybrid
