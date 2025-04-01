submodule(Solver_Solve) Solver_Solve_LU_Implementation
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
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
        integer :: converter

        allocate (Solver_CRS_LU :: structure)
        select type (this => structure)
        type is (Solver_CRS_LU)
            this%N = transfer(N, converter)
            this%MAXFCT = transfer(MAXFCT, converter)
            this%MNUM = transfer(MNUM, converter)
            this%MTYPE = transfer(MTYPE, converter)
            this%PHASE = transfer(PHASE, converter)
            this%NRHS = transfer(NRHS, converter)
            this%MSGLVL = transfer(MSGVLV, converter)
            allocate (this%PT(64))
            allocate (this%IPARM(64))
            allocate (this%PERM(N))
            allocate (this%JA(A%nnz))
            allocate (this%IA(N + 1))

            do i = 1, A%nnz
                this%JA(i) = transfer(A%Ind(i), converter)
            end do
            do i = 1, N + 1
                this%IA(i) = transfer(A%Ptr(i), converter)
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
        status = transfer(self%ERROR, status)

    end subroutine Solve_CRS_LU

    module subroutine Check_CRS_LU(self, status, time)
        implicit none
        class(Solver_CRS_LU) :: self
        integer(int32), intent(in) :: status
        real(real64), intent(in) :: time

        if (status /= 0) then
            print *, 'PARDISO 解法エラー'
            stop
        end if

    end subroutine Check_CRS_LU

end submodule Solver_Solve_LU_Implementation
