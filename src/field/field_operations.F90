module field_operations
    use, intrinsic :: iso_fortran_env
    use :: module_core
    use :: module_linalg
    use :: field_jacobian_matrix, only:type_jacobian_matrix
    use :: field_residual_vector, only:type_residual_vector
    implicit none
    private

    public :: ftdss_gemv
    public :: ftdss_sub
    public :: ftdss_dot

    interface ftdss_gemv
        module procedure :: ftdss_dgemv
    end interface

    interface ftdss_sub
        module procedure :: ftdss_dsub
    end interface

    interface ftdss_dot
        module procedure :: ftdss_ddot
    end interface

contains

    !> Performs the matrix–vector operation
    !>     y = alpha * A * x + beta * y
    !> using the Jacobian matrix A and residual vectors x and y.
    subroutine ftdss_dgemv(A, alpha, x, beta, y)
        implicit none
        !> Jacobian matrix
        type(type_jacobian_matrix), intent(in) :: A
        !> Scalar coefficient alpha
        real(real64), intent(in) :: alpha
        !> Input vector x
        type(type_residual_vector), intent(in) :: x
        !> Scalar coefficient beta
        real(real64), intent(in) :: beta
        !> Output vector y (updated in place)
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
            write (*, '(A)') 'Error: ftdss_dgemv: Unknown coupling mode.'
            stop
        end select

    end subroutine ftdss_dgemv

    !> Performs the vector subtraction
    !>     z = x - y
    !> using the residual vectors x, y, and z.
    subroutine ftdss_dsub(x, y, z)
        implicit none
        !> Input vector x
        type(type_residual_vector), intent(in) :: x
        !> Input vector y
        type(type_residual_vector), intent(in) :: y
        !> Output vector z (updated in place)
        type(type_residual_vector), intent(inout) :: z

        type(type_vector_dp), pointer :: vec_x
        type(type_vector_dp), pointer :: vec_y
        type(type_vector_dp), pointer :: vec_z
        integer(int32) :: i, num_dofs

        num_dofs = x%get_num_dofs_per_node()

        do i = 1, num_dofs
            vec_x => x%get_data(i)
            vec_y => y%get_data(i)
            vec_z => z%get_data(i)
            call sub(vec_x, vec_y, vec_z)
        end do

        vec_x => null()
        vec_y => null()
        vec_z => null()

    end subroutine ftdss_dsub

    !>
    !> inner product of two residual vectors
    !> result = (x, y)
    !>
    function ftdss_ddot(x, y) result(res)
        implicit none
        !> Input vector x
        type(type_residual_vector), intent(in) :: x
        !> Input vector y
        type(type_residual_vector), intent(in) :: y
        !> Result of the inner product
        real(real64) :: res

        type(type_vector_dp), pointer :: vec_x
        type(type_vector_dp), pointer :: vec_y
        integer(int32) :: i, num_dofs

        res = 0.0d0
        num_dofs = x%get_num_dofs_per_node()

        do i = 1, num_dofs
            vec_x => x%get_data(i)
            vec_y => y%get_data(i)
            res = res + dot(vec_x, vec_y)
        end do

        vec_x => null()
        vec_y => null()

    end function ftdss_ddot
end module field_operations

