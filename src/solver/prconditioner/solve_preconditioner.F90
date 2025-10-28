submodule(solver_solve) solve_preconditioner
    implicit none

contains
    module subroutine create_preconditioner_jacobi(N, A, M)
        implicit none
        integer(int32), intent(in) :: N
        class(abst_matrix), intent(in) :: A
        real(real64), intent(inout) :: M(:)
        integer(int32) :: i, j

        integer(int32), dimension(:), pointer :: p_ptr
        integer(int32), dimension(:), pointer :: p_ind
        real(real64), dimension(:), pointer :: p_val

        select type (matrix => A)
        type is (type_crs)
            p_ptr => matrix%get_ptr()
            p_ind => matrix%get_ind()
            p_val => matrix%get_val()

            !$omp parallel do private(i, j)
            do i = 1, N
                do j = p_ptr(i), p_ptr(i + 1) - 1
                    if (i == p_ind(j)) then
                        M(i) = 1.0d0 / p_val(j)
                    end if
                end do
            end do
            !$omp end parallel do
        end select

    end subroutine create_preconditioner_jacobi

    module subroutine apply_preconditioner_jacobi(N, M, r, z)
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
    end subroutine apply_preconditioner_jacobi

    module subroutine create_preconditioner_bicgstab(self, A)
        implicit none
        class(type_solver_bicgstab), intent(inout) :: self
        class(abst_matrix), intent(in) :: A
        integer(int32) :: i, j

        select case (self%Preconditioner)
        case (0)
            !! No preconditioner
            return
        case (1)
            !! Jacobi preconditioner
            call create_preconditioner_jacobi(self%size, A, self%M(:))
        case (2)
            !! ILU preconditioner
        end select

    end subroutine create_preconditioner_bicgstab

    module subroutine apply_preconditioner_bicgstab(self, b, x)
        implicit none
        class(type_solver_bicgstab), intent(inout) :: self
        real(real64), intent(inout) :: b(:)
        real(real64), intent(inout) :: x(:)
        integer(int32) :: i, j

        select case (self%Preconditioner)
        case (0)
            !! No preconditioner
            return
        case (1)
            !! Jacobi preconditioner
            call apply_preconditioner_jacobi(self%size, self%M(:), b(:), x(:))
        case (2)
            !! ILU preconditioner
        end select

    end subroutine apply_preconditioner_bicgstab
end submodule solve_preconditioner

