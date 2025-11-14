module field_matrix_operations
    use, intrinsic :: iso_fortran_env
    use :: module_core
    use :: module_linalg
    use :: field_jacobian_matrix, only:type_jacobian_matrix
    use :: field_residual_vector, only:type_residual_vector
    implicit none
    private

    public :: matvec

contains

    subroutine matvec(A, alpha, x, beta, y)
        implicit none
        type(type_jacobian_matrix), intent(in) :: A
        real(real64), intent(in) :: alpha
        type(type_residual_vector), intent(in) :: x
        real(real64), intent(in) :: beta
        type(type_residual_vector), intent(inout) :: y

        class(abst_matrix), pointer :: matrix_block
        type(type_vector_dp), pointer :: vec_x
        type(type_vector_dp), pointer :: vec_y
        real(real64), dimension(:), pointer :: vec_xx
        real(real64), dimension(:), pointer :: vec_yy

        integer(int32) :: i, j, num_dofs

        call y%scale(beta)

        select case (A%get_coupling_mode())
        case (COUPLING_MODE_STAGGERED)
            num_dofs = A%get_num_dofs_per_node()
            do i = 1, num_dofs
                matrix_block => A%get_matrix_block(i, i)
                vec_x => x%get_data(i)
                vec_xx => vec_x%get_data()
                vec_y => y%get_data(i)
                vec_yy => vec_y%get_data()
                call matrix_block%gemv(alpha, vec_xx, 1.0d0, vec_yy)
            end do
        case (COUPLING_MODE_MONOLITHIC)
            do i = 1, num_dofs
                vec_y => y%get_data(i)
                vec_yy => vec_y%get_data()
                do j = 1, num_dofs
                    matrix_block => A%get_matrix_block(i, j)
                    vec_x => x%get_data(j)
                    vec_xx => vec_x%get_data()
                    call matrix_block%gemv(alpha, vec_xx, 1.0d0, vec_yy)
                end do
            end do
        case default
            write (*, '(A)') 'Error: matvec: Unknown coupling mode.'
            stop
        end select

    end subroutine matvec
end module field_matrix_operations

