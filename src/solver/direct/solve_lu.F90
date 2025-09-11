submodule(solver_solve) solve_lu
    implicit none
contains
    module function construct_type_solver_sparse_crs_lu(A, MAXFCT, MNUM, MTYPE, PHASE, NRHS, MSGVLV) result(structure)
        implicit none
        type(type_jacobian_matrix), intent(in), target :: A
        integer(int32), intent(in) :: MAXFCT
        integer(int32), intent(in) :: MNUM
        integer(int32), intent(in) :: MTYPE
        integer(int32), intent(in) :: PHASE
        integer(int32), intent(in) :: NRHS
        integer(int32), intent(in) :: MSGVLV

        class(abst_solver), allocatable :: structure
        integer(int32), dimension(:), pointer :: p_ind => null()
        integer(int32), dimension(:), pointer :: p_ptr => null()
        ! class(abst_matrix), pointer :: matrix

        integer(int32) :: i

        allocate (type_solver_sparse_crs_lu :: structure)
        select type (this => structure)
        type is (type_solver_sparse_crs_lu)
            this%N = transfer(A%get_size(), this%N)
            this%MAXFCT = transfer(MAXFCT, this%MAXFCT)
            this%MNUM = transfer(MNUM, this%MNUM)
            this%MTYPE = transfer(MTYPE, this%MTYPE)
            this%PHASE = transfer(PHASE, this%PHASE)
            this%NRHS = transfer(NRHS, this%NRHS)
            this%MSGLVL = transfer(MSGVLV, this%MSGLVL)
            allocate (this%PERM(A%get_size()))

            this%A => A%get_matrix()
            select type (matrix => this%A)
            type is (type_crs)
                allocate (this%JA(matrix%get_nnz()))
                allocate (this%IA(matrix%get_num_ptr()))
                p_ind => matrix%get_ind()
                p_ptr => matrix%get_ptr()

                this%IPARM(:) = 0
                call PARDISOINIT(this%PT, this%MTYPE, this%IPARM)

                do i = 1, matrix%get_nnz()
                    this%JA(i) = transfer(p_ind(i), this%JA(i))
                end do
                do i = 1, matrix%get_num_ptr()
                    this%IA(i) = transfer(p_ptr(i), this%IA(i))
                end do

                p_ind => null()
                p_ptr => null()
            end select
        end select

    end function construct_type_solver_sparse_crs_lu

    module subroutine solve_sparse_crs_lu(self, b, x, status)
        implicit none
        class(type_solver_sparse_crs_lu), intent(inout) :: self
        real(real64), intent(inout) :: b(:)
        real(real64), intent(inout) :: x(:)
        integer(int32), intent(inout) :: status

        real(real64), dimension(:), pointer :: p_val => null()

        select type (matrix => self%A)
        type is (type_crs)
            p_val => matrix%get_val()
            call PARDISO(self%PT, self%MAXFCT, self%MNUM, self%MTYPE, self%PHASE, self%N, p_val, self%IA, self%JA, &
                         self%PERM, self%NRHS, self%IPARM, self%MSGLVL, b, x, self%ERROR)
            status = transfer(self%ERROR, status)
        end select

        p_val => null()

    end subroutine solve_sparse_crs_lu

    module subroutine check_sparse_crs_lu(self, status, time)
        implicit none
        class(type_solver_sparse_crs_lu), intent(inout) :: self
        integer(int32), intent(in) :: status
        real(real64), intent(in) :: time

        if (status /= 0) then
            print *, 'PARDISO ', status, ' LU 解法エラー'
            print *, 'PARDISO Error Code: ', self%ERROR
            stop
        end if

    end subroutine check_sparse_crs_lu

    module function construct_type_solver_dense_lu(A) result(structure)
        implicit none
        type(type_jacobian_matrix), intent(in), target :: A
        class(abst_solver), allocatable :: structure

        allocate (type_solver_dense_lu :: structure)
        select type (this => structure)
        type is (type_solver_dense_lu)
            this%A => A%get_matrix()
            this%N = transfer(A%get_size(), this%N)
            allocate (this%IPIV(this%N))
        end select

    end function construct_type_solver_dense_lu

    module subroutine solve_dense_lu(self, b, x, status)
        implicit none
        class(type_solver_dense_lu), intent(inout) :: self
        real(real64), intent(inout) :: b(:)
        real(real64), intent(inout) :: x(:)
        integer(int32), intent(inout) :: status

        real(real64), dimension(:, :), pointer :: p_val => null()

        !* LU decomposition
        select type (matrix => self%A)
        type is (type_dense)
            p_val => matrix%get_val()
            call dgetrf(self%N, self%N, p_val, self%N, self%IPIV, self%ERROR)
            if (self%ERROR /= 0) call error_message(942)

            !* solve linear equation
            call dgetrs('N', self%N, 1, p_val, self%N, self%IPIV, b, self%N, self%ERROR)
            if (self%ERROR /= 0) call error_message(943)

            x(:) = b(:)

            status = transfer(self%ERROR, status)
        end select

        p_val => null()
    end subroutine solve_dense_lu

    module subroutine check_dense_lu(self, status, time)
        implicit none
        class(type_solver_dense_lu), intent(inout) :: self
        integer(int32), intent(in) :: status
        real(real64), intent(in) :: time

        if (status /= 0) then
            print *, 'LU 解法エラー'
            stop
        end if

    end subroutine check_dense_lu

end submodule solve_lu
