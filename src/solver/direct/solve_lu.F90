submodule(Solver_Solve) Solve_LU
    implicit none
contains
    module function construct_type_solver_sparse_crs_lu(N, MAXFCT, MNUM, MTYPE, PHASE, NRHS, MSGVLV, A) result(structure)
        implicit none
        integer(int32), intent(in) :: N
        integer(int32), intent(in) :: MAXFCT
        integer(int32), intent(in) :: MNUM
        integer(int32), intent(in) :: MTYPE
        integer(int32), intent(in) :: PHASE
        integer(int32), intent(in) :: NRHS
        integer(int32), intent(in) :: MSGVLV
        type(type_crs), intent(in) :: A

        class(abst_solver), allocatable :: structure

        integer(int32) :: i

        allocate (type_solver_sparse_crs_lu :: structure)
        select type (this => structure)
        type is (type_solver_sparse_crs_lu)
            this%N = transfer(N, this%N)
            this%MAXFCT = transfer(MAXFCT, this%MAXFCT)
            this%MNUM = transfer(MNUM, this%MNUM)
            this%MTYPE = transfer(MTYPE, this%MTYPE)
            this%PHASE = transfer(PHASE, this%PHASE)
            this%NRHS = transfer(NRHS, this%NRHS)
            this%MSGLVL = transfer(MSGVLV, this%MSGLVL)
            allocate (this%PERM(N))
            allocate (this%JA(A%nnz))
            allocate (this%IA(A%num_ptr))

            this%IPARM(:) = 0
            call PARDISOINIT(this%PT, this%MTYPE, this%IPARM)

            do i = 1, A%nnz
                this%JA(i) = transfer(A%Ind(i), this%JA(i))
            end do
            do i = 1, A%num_ptr
                this%IA(i) = transfer(A%Ptr(i), this%IA(i))
            end do
        end select

    end function construct_type_solver_sparse_crs_lu

    module subroutine solve_sparse_crs_lu(self, A, b, x, status)
        implicit none
        class(type_solver_sparse_crs_lu), intent(inout) :: self
        type(type_crs), intent(in) :: A
        real(real64), intent(inout) :: b(:)
        real(real64), intent(inout) :: x(:)
        integer(int32), intent(inout) :: status

        call PARDISO(self%PT, self%MAXFCT, self%MNUM, self%MTYPE, self%PHASE, self%N, A%Val, self%IA, self%JA, self%PERM, self%NRHS, self%IPARM, self%MSGLVL, b, x, self%ERROR)
        status = transfer(self%ERROR, status)

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

    module function construct_type_solver_dense_lu(N) result(structure)
        implicit none
        integer(int32), intent(in) :: N
        class(abst_solver), allocatable :: structure

        allocate (type_solver_dense_lu :: structure)
        select type (this => structure)
        type is (type_solver_dense_lu)
            this%N = transfer(N, this%N)
            allocate (this%IPIV(this%N))
        end select

    end function construct_type_solver_dense_lu

    module subroutine solve_dense_lu(self, A, b, x, status)
        implicit none
        class(type_solver_dense_lu), intent(inout) :: self
        real(real64), intent(in) :: A(:, :)
        real(real64), intent(inout) :: b(:)
        real(real64), intent(inout) :: x(:)
        integer(int32), intent(inout) :: status
        !* LU decomposition
        call dgetrf(self%N, self%N, A, self%N, self%IPIV, self%ERROR)
        if (self%ERROR /= 0) call error_message(942)

        !* solve linear equation
        call dgetrs('N', self%N, 1, A, self%N, self%IPIV, b, self%N, self%ERROR)
        if (self%ERROR /= 0) call error_message(943)

        x(:) = b(:)

        status = transfer(self%ERROR, status)

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

end submodule Solve_LU
