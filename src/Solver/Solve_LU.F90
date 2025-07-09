submodule(Solver_Solve) Solve_LU
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use :: Matrix_CRS
    implicit none

contains
    module function Solver_CRS_LU_Constructor(N, MAXFCT, MNUM, MTYPE, PHASE, NRHS, MSGVLV, A) result(structure)
        implicit none
        integer(int32), intent(in) :: N
        integer(int32), intent(in) :: MAXFCT
        integer(int32), intent(in) :: MNUM
        integer(int32), intent(in) :: MTYPE
        integer(int32), intent(in) :: PHASE
        integer(int32), intent(in) :: NRHS
        integer(int32), intent(in) :: MSGVLV
        type(Type_CRS), intent(in) :: A
        class(Abstract_Solver_CRS), allocatable :: structure

        integer(int32) :: i

        allocate (Solver_CRS_LU :: structure)
        select type (this => structure)
        type is (Solver_CRS_LU)
            this%N = int(N, kind=kind(this%N))
            this%MAXFCT = int(MAXFCT, kind=kind(this%MAXFCT))
            this%MNUM = int(MNUM, kind=kind(this%MNUM))
            this%MTYPE = int(MTYPE, kind=kind(this%MTYPE))
            this%PHASE = int(PHASE, kind=kind(this%PHASE))
            this%NRHS = int(NRHS, kind=kind(this%NRHS))
            this%MSGLVL = int(MSGVLV, kind=kind(this%MSGLVL))
            allocate (this%PERM(N))
            allocate (this%JA(A%nnz))
            allocate (this%IA(A%nptr))

            this%IPARM = 0
            call PARDISOINIT(this%PT, this%MTYPE, this%IPARM)

            do i = 1, A%nnz
                this%JA(i) = int(A%Ind(i), kind=kind(this%JA(i)))
            end do
            do i = 1, A%nptr
                this%IA(i) = int(A%Ptr(i), kind=kind(this%IA(i)))
            end do
        end select

    end function Solver_CRS_LU_Constructor

    module subroutine Solve_CRS_LU(self, A, b, x, status)
        implicit none
        class(Solver_CRS_LU) :: self
        type(Type_CRS), intent(in) :: A
        real(real64), intent(inout) :: b(:)
        real(real64), intent(inout) :: x(:)
        integer(int32), intent(inout) :: status

        call PARDISO(self%PT, self%MAXFCT, self%MNUM, self%MTYPE, self%PHASE, self%N, A%Val, self%IA, self%JA, self%PERM, self%NRHS, self%IPARM, self%MSGLVL, b, x, self%ERROR)
        status = int(self%ERROR, kind=kind(status))

    end subroutine Solve_CRS_LU

    module subroutine Check_CRS_LU(self, status, time)
        implicit none
        class(Solver_CRS_LU) :: self
        integer(int32), intent(in) :: status
        real(real64), intent(in) :: time

        if (status /= 0) then
            print *, 'PARDISO '
            stop
        end if

    end subroutine Check_CRS_LU

    module function Solver_Full_LU_Constructor(N) result(structure)
        implicit none
        integer(int32), intent(in) :: N

        class(Abstract_Solver_Full), allocatable :: structure

        allocate (Solver_Full_LU :: structure)
        select type (this => structure)
        type is (Solver_Full_LU)
            this%N = int(N, kind=kind(this%N))
            allocate (this%IPIV(this%N))
        end select

    end function Solver_Full_LU_Constructor

    module subroutine Solve_Full_LU(self, A, b, x, status)
        implicit none
        class(Solver_Full_LU) :: self
        real(real64), intent(in) :: A(:, :)
        real(real64), intent(inout) :: b(:)
        real(real64), intent(inout) :: x(:)
        integer(int32), intent(inout) :: status

        !* LU decomposition
        call Dgetrf(self%N, self%N, A, self%N, self%IPIV, self%ERROR)
        if (self%ERROR /= 0) call error_message(942)

        !* solve linear equation
        call Dgetrs('N', self%N, 1, A, self%N, self%IPIV, b, self%N, self%ERROR)
        if (self%ERROR /= 0) call error_message(943)

        x(:) = b(:)

        status = int(self%ERROR, kind=kind(status))

    end subroutine Solve_Full_LU

    module subroutine Check_Full_LU(self, status, time)
        implicit none
        class(Solver_Full_LU) :: self
        integer(int32), intent(in) :: status
        real(real64), intent(in) :: time

        if (status /= 0) then
            print *, 'LU 解法エラー'
            stop
        end if

    end subroutine Check_Full_LU

end submodule Solve_LU
