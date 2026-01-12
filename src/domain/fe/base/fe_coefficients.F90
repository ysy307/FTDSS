submodule(domain_base_fe) domain_base_fe_coefficients
    implicit none
contains

    !>
    !> Computes shape functions, their global gradients, and the Jacobian determinant.
    !>
    module subroutine calc_shape_function_abst_fe(self, r, node_coords, psi, dpsi_dx, inverse_jacobian, determinant_jacobian)
        implicit none
        class(abst_fe), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)

        real(real64), intent(inout), optional :: psi(:)
        real(real64), intent(inout), optional :: dpsi_dx(:, :)
        real(real64), intent(inout), optional :: inverse_jacobian(:, :)
        real(real64), intent(inout), optional :: determinant_jacobian

        integer(int32) :: i

        ! 1. Evaluate Shape Functions
        if (present(psi)) then
            psi(:) = 0.0d0
            do i = 1, self%num_nodes
                call self%psi(i, r, psi(i))
            end do
        end if

        ! 2. Compute Jacobian Matrix (Isoparametric formulation)
        if (present(determinant_jacobian)) then
            determinant_jacobian = 0.0d0
            call self%jacobian_det(r, node_coords, determinant_jacobian)
        end if

        ! 3. Compute Determinant and Inverse Jacobian
        if (present(inverse_jacobian)) then
            inverse_jacobian(:, :) = 0.0d0
            call self%calc_inverse_jacobian(r, node_coords, inverse_jacobian(1:self%dimension, 1:self%dimension))
        end if

        ! 4. Compute Global Gradients if requested
        if (present(dpsi_dx)) then
            dpsi_dx(:, :) = 0.0d0
            call self%dpsi_dx(r, node_coords, dpsi_dx(:, 1:self%dimension))
        end if

    end subroutine calc_shape_function_abst_fe

    module subroutine calc_inverse_jacobian_abst_fe(self, r, node_coords, inverse_jacobian)
        implicit none
        class(abst_fe), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        real(real64), intent(inout) :: inverse_jacobian(:, :)

        integer(int32) :: ierr

        call self%jacobian(r, node_coords, inverse_jacobian)
        call matrix_inverse(inverse_jacobian, ierr)

    end subroutine calc_inverse_jacobian_abst_fe

    module subroutine dpsi_dx_abst_fe(self, r, node_coords, dpsi_dx)
        implicit none
        class(abst_fe), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        real(real64), intent(inout) :: dpsi_dx(:, :)

        integer(int32) :: i, j
        real(real64) :: inverse_jacobian(3, 3)
        real(real64) :: dpsi_dxi(3)

        call self%calc_inverse_jacobian(r, node_coords, inverse_jacobian)
        do i = 1, self%num_nodes
            do j = 1, self%dimension
                call self%dpsi(i, j, r, dpsi_dxi(j))
            end do

            ! Transform to global coordinates
            if (self%dimension == 1) then
                dpsi_dx(i, 1) = dpsi_dxi(1) * inverse_jacobian(1, 1)
            else if (self%dimension == 2) then
                dpsi_dx(i, 1) = dpsi_dxi(1) * inverse_jacobian(1, 1) + dpsi_dxi(2) * inverse_jacobian(2, 1)
                dpsi_dx(i, 2) = dpsi_dxi(1) * inverse_jacobian(1, 2) + dpsi_dxi(2) * inverse_jacobian(2, 2)
            else if (self%dimension == 3) then
                dpsi_dx(i, 1) = vector_dot(dpsi_dxi, inverse_jacobian(:, 1))
                dpsi_dx(i, 2) = vector_dot(dpsi_dxi, inverse_jacobian(:, 2))
                dpsi_dx(i, 3) = vector_dot(dpsi_dxi, inverse_jacobian(:, 3))
            end if
        end do

    end subroutine dpsi_dx_abst_fe

end submodule domain_base_fe_coefficients
