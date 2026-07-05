submodule(domain_base_fe) domain_base_fe_coefficients
    implicit none
contains

    !>
    !> Computes shape functions, their global gradients, and the Jacobian determinant.
    !> Optimized to avoid redundant Jacobian calculations.
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

        integer(int32) :: i, j, k, dim
        ! Use fixed-size arrays on the stack, allocated for max dimension and controlled by dim
        real(real64) :: local_J(self%dimension, self%dimension)
        real(real64) :: local_inv_J(self%dimension, self%dimension)
        real(real64) :: local_det_J
        real(real64) :: dpsi_dxi(self%dimension)
        logical :: need_jacobian, need_inverse

        dim = self%dimension

        ! 1. Evaluate Shape Functions (Requires no Jacobian)
        if (present(psi)) then
            psi(:) = 0.0d0
            do i = 1, self%num_nodes
                call self%calc_psi(i, r, psi(i))
            end do
        end if

        ! Determine whether Jacobian computation is needed
        need_jacobian = present(determinant_jacobian) .or. &
                        present(inverse_jacobian) .or. &
                        present(dpsi_dx)

        if (.not. need_jacobian) return

        ! --- A. Compute Jacobian Matrix (ONCE) ---
        call self%calc_jacobian(r, node_coords, local_J)

        ! --- B. Determinant (analytic for dim<=3; avoids LAPACK overhead on tiny J) ---
        if (dim == 1) then
            local_det_J = local_J(1, 1)
        else if (dim == 2) then
            local_det_J = local_J(1, 1) * local_J(2, 2) - local_J(1, 2) * local_J(2, 1)
        else
            local_det_J = local_J(1, 1) * (local_J(2, 2) * local_J(3, 3) - local_J(2, 3) * local_J(3, 2)) &
                          - local_J(1, 2) * (local_J(2, 1) * local_J(3, 3) - local_J(2, 3) * local_J(3, 1)) &
                          + local_J(1, 3) * (local_J(2, 1) * local_J(3, 2) - local_J(2, 2) * local_J(3, 1))
        end if
        if (local_det_J <= 0.0_real64) then
            error stop "Non-positive Jacobian determinant: degenerate or inverted element"
        end if
        if (present(determinant_jacobian)) determinant_jacobian = local_det_J

        ! Determine whether the inverse Jacobian is needed (also required for dpsi_dx)
        need_inverse = present(inverse_jacobian) .or. present(dpsi_dx)

        if (.not. need_inverse) return

        ! --- C. Inverse Jacobian (analytic for dim<=3) ---
        if (dim == 1) then
            local_inv_J(1, 1) = 1.0d0 / local_det_J
        else if (dim == 2) then
            local_inv_J(1, 1) = local_J(2, 2) / local_det_J
            local_inv_J(1, 2) = -local_J(1, 2) / local_det_J
            local_inv_J(2, 1) = -local_J(2, 1) / local_det_J
            local_inv_J(2, 2) = local_J(1, 1) / local_det_J
        else
            local_inv_J(1, 1) = (local_J(2, 2) * local_J(3, 3) - local_J(2, 3) * local_J(3, 2)) / local_det_J
            local_inv_J(1, 2) = -(local_J(1, 2) * local_J(3, 3) - local_J(1, 3) * local_J(3, 2)) / local_det_J
            local_inv_J(1, 3) = (local_J(1, 2) * local_J(2, 3) - local_J(1, 3) * local_J(2, 2)) / local_det_J
            local_inv_J(2, 1) = -(local_J(2, 1) * local_J(3, 3) - local_J(2, 3) * local_J(3, 1)) / local_det_J
            local_inv_J(2, 2) = (local_J(1, 1) * local_J(3, 3) - local_J(1, 3) * local_J(3, 1)) / local_det_J
            local_inv_J(2, 3) = -(local_J(1, 1) * local_J(2, 3) - local_J(1, 3) * local_J(2, 1)) / local_det_J
            local_inv_J(3, 1) = (local_J(2, 1) * local_J(3, 2) - local_J(2, 2) * local_J(3, 1)) / local_det_J
            local_inv_J(3, 2) = -(local_J(1, 1) * local_J(3, 2) - local_J(1, 2) * local_J(3, 1)) / local_det_J
            local_inv_J(3, 3) = (local_J(1, 1) * local_J(2, 2) - local_J(1, 2) * local_J(2, 1)) / local_det_J
        end if

        if (present(inverse_jacobian)) then
            inverse_jacobian(1:dim, 1:dim) = local_inv_J(1:dim, 1:dim)
        end if

        ! --- D. Compute Global Gradients (dpsi_dx) ---
        ! Formula: dpsi_dx = J^{-T} * dpsi_dxi
        if (present(dpsi_dx)) then
            dpsi_dx(:, :) = 0.0d0

            do i = 1, self%num_nodes
                ! Get local gradients (dpsi/dxi, dpsi/deta, ...)
                do j = 1, dim
                    call self%calc_dpsi(i, j, r, dpsi_dxi(j))
                end do

                ! Transform to physical gradients: multiply by J^{-T}
                ! dpsi_dx(k, i) = sum_l ( invJ(l, k) * dpsi_dxi(l) )
                ! Note: invJ(l, k) uses transposed access, hence the (l, k) indexing
                do k = 1, dim
                    do j = 1, dim
                        dpsi_dx(k, i) = dpsi_dx(k, i) + local_inv_J(j, k) * dpsi_dxi(j)
                    end do
                end do
            end do
        end if

    end subroutine calc_shape_function_abst_fe

    module subroutine calc_inverse_jacobian_abst_fe(self, r, node_coords, inverse_jacobian)
        implicit none
        class(abst_fe), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        real(real64), intent(inout) :: inverse_jacobian(:, :)

        integer(int32) :: ierr

        call self%calc_jacobian(r, node_coords, inverse_jacobian)
        call matrix_inverse(inverse_jacobian(1:self%dimension, 1:self%dimension), ierr)

    end subroutine calc_inverse_jacobian_abst_fe

    module subroutine calc_dpsi_dx_abst_fe(self, r, node_coords, dpsi_dx)
        implicit none
        class(abst_fe), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        real(real64), intent(inout) :: dpsi_dx(:, :)

        integer(int32) :: i, j, k
        real(real64) :: inverse_jacobian(self%dimension, self%dimension)
        real(real64) :: dpsi_dxi(self%dimension)
        integer(int32) :: ierr

        call self%calc_jacobian(r, node_coords, inverse_jacobian)
        call matrix_inverse(inverse_jacobian, ierr)

        dpsi_dx(:, :) = 0.0d0
        do i = 1, self%num_nodes
            do j = 1, self%dimension
                call self%calc_dpsi(i, j, r, dpsi_dxi(j))
            end do

            ! J^{-T} * dpsi_dxi
            do k = 1, self%dimension
                do j = 1, self%dimension
                    dpsi_dx(k, i) = dpsi_dx(k, i) + inverse_jacobian(j, k) * dpsi_dxi(j)
                end do
            end do
        end do

    end subroutine calc_dpsi_dx_abst_fe

    module subroutine calc_jacobian_determinant_abst_fe(self, r, node_coords, determinant_jacobian)
        implicit none
        class(abst_fe), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        real(real64), intent(inout) :: determinant_jacobian

        real(real64) :: jacobian(self%dimension, self%dimension)
        integer(int32) :: ierr

        call self%calc_jacobian(r, node_coords, jacobian)
        call matrix_determinant(jacobian, determinant_jacobian, ierr)
        if (determinant_jacobian <= 0.0d0) then
            error stop "Non-positive Jacobian determinant: invalid element orientation"
        end if

    end subroutine calc_jacobian_determinant_abst_fe

end submodule domain_base_fe_coefficients
