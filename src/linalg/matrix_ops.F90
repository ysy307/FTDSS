! src/linalg/matrix_operations.F90
module matrix_operations
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core
    implicit none

    interface
        !>
        !> Performs a general matrix-vector multiplication: y = alpha*A*x + beta*y.
        !>
        subroutine abst_gemv(self, alpha, x, beta, y)
            import :: abst_matrix, real64
            implicit none
            !> The matrix object (A).
            class(abst_matrix), intent(in) :: self
            !> The scalar multiplier alpha.
            real(real64), intent(in) :: alpha
            !> The input vector x.
            real(real64), intent(in) :: x(:)
            !> The scalar multiplier beta.
            real(real64), intent(in) :: beta
            !> The input/output vector y.
            real(real64), intent(inout) :: y(:)
        end subroutine abst_gemv
        ! Define interfaces for matrix operations here
    end interface

contains

    ! subroutine gemv(matrix, vector_in, vector_out)
    !     implicit none
    !     class(abst_matrix), intent(in) :: matrix
    !     class(abst_vector), intent(in) :: vector_in
    !     class(abst_vector), intent(out) :: vector_out
    !     ! ...
    ! end subroutine

    ! subroutine add(matrix_a, matrix_b, matrix_out)
    !     implicit none
    !     class(abst_matrix), intent(in) :: matrix_a, matrix_b
    !     class(abst_matrix), intent(out) :: matrix_out
    !     ! ...
    ! end subroutine

end module
