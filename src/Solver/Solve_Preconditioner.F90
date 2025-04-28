submodule(Solver_Solve) Solver_Solve_Preconditioner_Implementation
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Matrix_CRS
    implicit none

contains
    module subroutine Create_Preconditioner_Jacobi(N, A, M)
        implicit none
        integer(int32), intent(in) :: N
        type(Type_CRS), intent(in) :: A
        real(real64), intent(inout) :: M(:)
        integer(int32) :: i, j

        !$omp parallel do private(i, j)
        do i = 1, N
            do j = A%Ptr(i), A%Ptr(i + 1) - 1
                if (i == A%Ind(j)) then
                    M(i) = 1.0d0 / A%Val(j)
                end if
            end do
        end do
        !$omp end parallel do

    end subroutine Create_Preconditioner_Jacobi

    module subroutine Apply_Preconditioner_Jacobi(N, M, r, z)
        implicit none
        integer(int32), intent(in) :: N
        real(real64), intent(in) :: M(:)
        real(real64), intent(in) :: r(:)
        real(real64), intent(inout) :: z(:)
        integer(int32) :: i

        !$omp parallel do private(i)
        do i = 1, N
            z(i) = M(i) * r(i)
        end do
        !$omp end parallel do
    end subroutine Apply_Preconditioner_Jacobi

    module subroutine Create_Preconditioner_CRS_BiCGSTAB(self, A)
        implicit none
        class(Solver_CRS_BiCGSTAB) :: self
        type(Type_CRS), intent(in) :: A
        integer(int32) :: i, j

        select case (self%Preconditioner)
        case (0)
            !! No preconditioner
            return
        case (1)
            !! Jacobi preconditioner
            call Create_Preconditioner_Jacobi(self%N, A, self%M(:))
        case (2)
            !! ILU preconditioner
        end select

    end subroutine Create_Preconditioner_CRS_BiCGSTAB

    module subroutine Apply_Preconditioner_CRS_BiCGSTAB(self, b, x)
        implicit none
        class(Solver_CRS_BiCGSTAB) :: self
        real(real64), intent(inout) :: b(:)
        real(real64), intent(inout) :: x(:)
        integer(int32) :: i, j

        select case (self%Preconditioner)
        case (0)
            !! No preconditioner
            return
        case (1)
            !! Jacobi preconditioner
            call Apply_Preconditioner_Jacobi(self%N, self%M(:), b(:), x(:))
        case (2)
            !! ILU preconditioner
        end select

    end subroutine Apply_Preconditioner_CRS_BiCGSTAB
end submodule Solver_Solve_Preconditioner_Implementation

