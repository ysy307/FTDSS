module linalg_matrix_operations
    use, intrinsic :: iso_fortran_env
    use :: module_core
    implicit none
    private

    public :: matrix_axpy
    public :: matrix_xpay
    public :: matrix_axpyz
    public :: matrix_scale

contains

    !>
    !> Matrix AXPY operation: Y := a*X + Y
    subroutine matrix_axpy(alpha, A, B, ierr)
        implicit none
        real(real64), intent(in) :: alpha
        class(abst_matrix), intent(in) :: A
        class(abst_matrix), intent(inout) :: B
        integer(int32), intent(inout) :: ierr

        real(real64), dimension(:, :), pointer :: A_data
        real(real64), dimension(:, :), pointer :: B_data

        select type (A)
        type is (type_dense)
            select type (B)
            type is (type_dense)
                A_data => A%get_val()
                B_data => B%get_val()
                B_data = alpha * A_data + B_data
                ierr = MATRIX_STATUS_SUCCESS
            class default
                ierr = MATRIX_STATUS_ILL_OPERATIONS
            end select
        class default
            ierr = MATRIX_STATUS_ILL_OPERATIONS
        end select

    end subroutine matrix_axpy

    !>
    !> Matrix XAPY
    subroutine matrix_xpay(alpha, A, B, ierr)
        implicit none
        real(real64), intent(in) :: alpha
        class(abst_matrix), intent(in) :: A
        class(abst_matrix), intent(inout) :: B
        integer(int32), intent(inout) :: ierr

        real(real64), dimension(:, :), pointer :: A_data
        real(real64), dimension(:, :), pointer :: B_data

        select type (A)
        type is (type_dense)
            select type (B)
            type is (type_dense)
                A_data => A%get_val()
                B_data => B%get_val()
                B_data = A_data + alpha * B_data
                ierr = MATRIX_STATUS_SUCCESS
            class default
                ierr = MATRIX_STATUS_ILL_OPERATIONS
            end select
        class default
            ierr = MATRIX_STATUS_ILL_OPERATIONS
        end select

    end subroutine matrix_xpay

    !>
    !> Matrix AXPYZ
    subroutine matrix_axpyz(alpha, A, B, C, ierr)
        implicit none
        real(real64), intent(in) :: alpha
        class(abst_matrix), intent(in) :: A
        class(abst_matrix), intent(in) :: B
        class(abst_matrix), intent(inout) :: C
        integer(int32), intent(inout) :: ierr

        real(real64), dimension(:, :), pointer :: A_data
        real(real64), dimension(:, :), pointer :: B_data
        real(real64), dimension(:, :), pointer :: C_data

        select type (A)
        type is (type_dense)
            select type (B)
            type is (type_dense)
                select type (C)
                type is (type_dense)
                    A_data => A%get_val()
                    B_data => B%get_val()
                    C_data => C%get_val()
                    C_data = alpha * A_data + B_data
                    ierr = MATRIX_STATUS_SUCCESS
                class default
                    ierr = MATRIX_STATUS_ILL_OPERATIONS
                end select
            class default
                ierr = MATRIX_STATUS_ILL_OPERATIONS
            end select
        class default
            ierr = MATRIX_STATUS_ILL_OPERATIONS
        end select

    end subroutine matrix_axpyz

    subroutine matrix_scale(A, b, d, op, ierr)
        implicit none
        class(abst_matrix), intent(inout) :: A
        type(type_vector_dp), intent(inout) :: b
        type(type_vector_dp), intent(inout) :: d
        integer(int32), intent(in) :: op
        integer(int32), intent(inout) :: ierr

        real(real64), dimension(:), pointer :: diag

        call d%copy(b)
        call A%get_diagonal(d)
        diag => d%get_data()

        select case (op)
        case (OP_SCALE_SYMM_DIAG)
            diag = 1.0d0 / sqrt(diag)
        case (OP_SCALE_JACOBI)
            diag = 1.0d0 / diag
        case default
            ierr = MATRIX_STATUS_ILL_OPERATIONS
            return
        end select

        call A%scale(op, d)

    end subroutine matrix_scale

end module linalg_matrix_operations
