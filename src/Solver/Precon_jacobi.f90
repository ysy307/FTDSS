module Solver_Precon_jacobi
    use omp_lib
    use, intrinsic :: iso_fortran_env, only : int32, real64
    use :: Types
    implicit none

    public :: Create_Precond_Jacobi
    public :: Apply_Precond_Jacobi

    contains
    
    subroutine Create_Precond_Jacobi(N, A, M)
        implicit none
        integer(int32), intent(in)    :: N
        type(CRS),      intent(in)    :: A
        real(real64),   intent(inout) :: M(:)
        integer(int32)                :: i, j

        M(:) = 0.0d0

        !$omp parallel do private(i, j)
        do i = 1, N
            do j = A%Ptr(i-1), A%Ptr(i) - 1
                if ((i-1) == A%Ind(j)) then
                    M(i) = 1.0d0 / A%Val(j)
                end if
            end do
        end do
        !$omp end parallel do

    end subroutine Create_Precond_Jacobi

    subroutine Apply_Precond_Jacobi(N, M, r, z)
        implicit none
        integer(int32), intent(in)    :: N
        real(real64),   intent(in)    :: M(:)
        real(real64),   intent(in)    :: r(:)
        real(real64),   intent(inout) :: z(:)
        integer(int32)                :: i

        ! $omp parallel do private(i)
        do i = 1, N
            z(i) = M(i) * r(i)
        end do
        ! $omp end parallel do

    end subroutine Apply_Precond_Jacobi
    
end module Solver_Precon_jacobi