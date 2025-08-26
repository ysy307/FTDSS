module calculate_linalg_matrix_ops
!$  use omp_lib
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core, only:type_coo, type_crs, type_dense
    implicit none
    private

    public :: add
    interface add
        module procedure :: type_coo_add
        module procedure :: type_crs_add
        module procedure :: type_dense_add
    end interface

contains

    subroutine type_coo_add(alpha, A, B, C)
        ! C := alpha*A + B
        !
        ! [ATTENTION] Assumes A, B, and C have the exact same sparsity pattern.
        !
        implicit none
        real(real64), intent(in) :: alpha
        type(type_coo), intent(in) :: A
        type(type_coo), intent(in) :: B
        type(type_coo), intent(inout) :: C

        integer(int32) :: i

        !$omp parallel do
        do i = 1, A%nnz
            C%val(i) = alpha * A%val(i) + B%val(i)
        end do
        !$omp end parallel do
    end subroutine type_coo_add

    subroutine type_crs_add(alpha, A, B, C)
        ! C := alpha*A + B
        !
        ! [ATTENTION] Assumes A, B, and C have the exact same sparsity pattern.
        !
        implicit none
        real(real64), intent(in) :: alpha
        type(type_crs), intent(in) :: A
        type(type_crs), intent(in) :: B
        type(type_crs), intent(inout) :: C

        integer(int32) :: i

        !$omp parallel do
        do i = 1, A%nnz
            C%val(i) = alpha * A%val(i) + B%val(i)
        end do
        !$omp end parallel do

    end subroutine type_crs_add

    subroutine type_dense_add(alpha, A, B, C)
        ! C := alpha*A + B
        !
        ! [ATTENTION] Assumes A, B, and C have the exact same sparsity pattern.
        !
        implicit none
        real(real64), intent(in) :: alpha
        type(type_dense), intent(in) :: A
        type(type_dense), intent(in) :: B
        type(type_dense), intent(inout) :: C

        integer(int32) :: i, j

        !$omp parallel do private(i, j) collapse(2)
        do i = 1, size(A%val, 1)
            do j = 1, size(A%val, 2)
                C%val(i, j) = alpha * A%val(i, j) + B%val(i, j)
            end do
        end do
        !$omp end parallel do

    end subroutine type_dense_add

end module calculate_linalg_matrix_ops
