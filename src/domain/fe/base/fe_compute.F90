submodule(domain_base_fe) domain_base_fe_compute
    implicit none
contains

    !==========================================================================
    ! Integration Matrices using Coefficients at Gauss Points (A_gp, M_gp, etc.)
    !==========================================================================
    !>
    !> 1. Capacity Matrix K1
    !>    K_ij = \int  A(x) * psi_i * psi_j  dOmega
    !>    A_gp: Coefficients evaluated directly at Gauss points [num_gauss]
    !>    work_psi, work_dpsi_dx: (Optional) Pre-allocated workspace to avoid repeated allocation.
    !>
    module subroutine compute_K1_capacity_abst_fe(self, nodes, A_gp, elem_mat, work_psi)
        implicit none
        class(abst_fe), intent(in) :: self
        real(real64), intent(in) :: nodes(:, :)
        real(real64), intent(in) :: A_gp(:)
        real(real64), intent(inout) :: elem_mat(:, :)
        real(real64), intent(inout), optional, target :: work_psi(:)

        integer(int32) :: p, i, j, num_nodes, dim, num_gauss
        real(real64) :: w, det_J, A_val
        type(type_coordinate_dp) :: r

        real(real64), pointer, dimension(:) :: p_psi
        real(real64), allocatable, target :: local_psi(:)

        nullify (p_psi)

        call self%get_dimension(dim)
        call self%get_num_nodes(num_nodes)
        call self%get_num_gauss(num_gauss)

        if (present(work_psi)) then
            p_psi => work_psi
        else
            call allocate_array(local_psi, num_nodes)
            p_psi => local_psi
        end if

        elem_mat(:, :) = 0.0d0
        p_psi(:) = 0.0d0

        do p = 1, num_gauss
            r = self%integration_rule%gauss(p)
            w = self%integration_rule%weight(p)

            call self%calc_shape_function(r, nodes, psi=p_psi, determinant_jacobian=det_J)
            A_val = A_gp(p)

            do j = 1, num_nodes
                do i = 1, num_nodes
                    elem_mat(i, j) = elem_mat(i, j) + w * abs(det_J) * A_val * p_psi(i) * p_psi(j)
                end do
            end do
        end do

        nullify (p_psi)
        if (.not. present(work_psi)) then
            call deallocate_array(local_psi)
        end if
    end subroutine compute_K1_capacity_abst_fe

    !>
    !> 1-Lumped. Lumped Capacity Matrix K1 (Diagonal)
    !>    K_ii = \int A(x) * psi_i dOmega (Row-sum lumping approximation)
    !>    A_gp: Coefficients evaluated directly at Gauss points [num_gauss]
    !>    The result is stored in the diagonal elements of elem_mat.
    !>    Off-diagonal elements are zero.
    !>
    module subroutine compute_K1_lumped_capacity_abst_fe(self, nodes, A_gp, elem_mat, work_psi)
        implicit none
        class(abst_fe), intent(in) :: self
        real(real64), intent(in) :: nodes(:, :)
        real(real64), intent(in) :: A_gp(:)
        real(real64), intent(inout) :: elem_mat(:, :)
        real(real64), intent(inout), optional, target :: work_psi(:)

        integer(int32) :: p, i, num_nodes, dim, num_gauss
        real(real64) :: w, det_J, A_val
        type(type_coordinate_dp) :: r

        real(real64), pointer, dimension(:) :: p_psi
        real(real64), allocatable, target :: local_psi(:)

        nullify (p_psi)

        call self%get_dimension(dim)
        call self%get_num_nodes(num_nodes)
        call self%get_num_gauss(num_gauss)

        if (present(work_psi)) then
            p_psi => work_psi
        else
            call allocate_array(local_psi, num_nodes)
            p_psi => local_psi
        end if

        ! Initialize matrix to zero (including off-diagonal)
        elem_mat(:, :) = 0.0d0
        p_psi(:) = 0.0d0

        do p = 1, num_gauss
            r = self%integration_rule%gauss(p)
            w = self%integration_rule%weight(p)

            ! Calc shape function only (derivatives not needed for mass matrix)
            call self%calc_shape_function(r, nodes, psi=p_psi, determinant_jacobian=det_J)
            A_val = A_gp(p)

            do i = 1, num_nodes
                ! Row-sum lumping:
                ! Consistent mass term is psi_i * psi_j.
                ! Since sum(psi_j) = 1, summing rows is equivalent to integrating psi_i.
                elem_mat(i, i) = elem_mat(i, i) + w * abs(det_J) * A_val * p_psi(i)
            end do
        end do

        nullify (p_psi)
        if (.not. present(work_psi)) then
            call deallocate_array(local_psi)
        end if
    end subroutine compute_K1_lumped_capacity_abst_fe

    !>
    !> 2. Diffusion Matrix K2
    !>    K_ij =  \int  nabla psi_i . (M(x) * nabla psi_j)  dOmega
    !>    M_gp: Tensor coefficients evaluated at Gauss points [dim, dim, num_gauss]
    !>
    module subroutine compute_K2_diffusion_abst_fe(self, nodes, M_gp, elem_mat, &
                                                   work_psi, work_dpsi_dx, work_vec)
        implicit none
        class(abst_fe), intent(in) :: self
        real(real64), intent(in) :: nodes(:, :)
        real(real64), intent(in) :: M_gp(:, :, :)
        real(real64), intent(inout) :: elem_mat(:, :)
        real(real64), intent(inout), optional, target :: work_psi(:)
        real(real64), intent(inout), optional, target :: work_dpsi_dx(:, :)
        real(real64), intent(inout), optional, target :: work_vec(:) ! For M_grad_psi_j [dim]

        integer(int32) :: p, i, j, num_nodes, dim, num_gauss
        integer(int32) :: d, kk
        real(real64) :: w, det_J, dotv, wdet
        type(type_coordinate_dp) :: r

        real(real64), pointer :: p_psi(:)
        real(real64), pointer :: p_dpsi_dx(:, :)
        real(real64), pointer :: p_M_grad_psi_j(:)

        real(real64), allocatable, target :: local_psi(:)
        real(real64), allocatable, target :: local_dpsi_dx(:, :)
        real(real64), allocatable, target :: local_vec_dim(:)

        nullify (p_psi)
        nullify (p_dpsi_dx)
        nullify (p_M_grad_psi_j)

        call self%get_dimension(dim)
        call self%get_num_nodes(num_nodes)
        call self%get_num_gauss(num_gauss)

        if (present(work_psi)) then
            p_psi => work_psi
        else
            call allocate_array(local_psi, num_nodes)
            p_psi => local_psi
        end if

        if (present(work_dpsi_dx)) then
            p_dpsi_dx => work_dpsi_dx
        else
            call allocate_array(local_dpsi_dx, dim, num_nodes)
            p_dpsi_dx => local_dpsi_dx
        end if

        if (present(work_vec)) then
            p_M_grad_psi_j => work_vec
        else
            call allocate_array(local_vec_dim, dim)
            p_M_grad_psi_j => local_vec_dim
        end if

        elem_mat = 0.0d0
        p_psi(:) = 0.0d0
        p_dpsi_dx(:, :) = 0.0d0
        p_M_grad_psi_j(:) = 0.0d0

        do p = 1, num_gauss
            r = self%integration_rule%gauss(p)
            w = self%integration_rule%weight(p)

            call self%calc_shape_function(r, nodes, psi=p_psi, dpsi_dx=p_dpsi_dx, determinant_jacobian=det_J)

            wdet = w * abs(det_J)
            do j = 1, num_nodes
                ! M_grad_psi_j = M_gp(:,:,p) . dpsi_dx(:,j)  (small dim=2/3: inline, not MKL)
                do d = 1, dim
                    p_M_grad_psi_j(d) = 0.0d0
                    do kk = 1, dim
                        p_M_grad_psi_j(d) = p_M_grad_psi_j(d) + M_gp(d, kk, p) * p_dpsi_dx(kk, j)
                    end do
                end do

                do i = 1, num_nodes
                    ! elem_mat(i,j) += wdet * (dpsi_dx(:,i) . M_grad_psi_j)  (inline dot)
                    dotv = 0.0d0
                    do d = 1, dim
                        dotv = dotv + p_dpsi_dx(d, i) * p_M_grad_psi_j(d)
                    end do
                    elem_mat(i, j) = elem_mat(i, j) + wdet * dotv
                end do
            end do
        end do

        call deallocate_array(local_psi)
        call deallocate_array(local_dpsi_dx)
        call deallocate_array(local_vec_dim)

        nullify (p_psi)
        nullify (p_dpsi_dx)
        nullify (p_M_grad_psi_j)

    end subroutine compute_K2_diffusion_abst_fe

    !>
    !> 2. Diffusion Matrix K2 (Scalar Version)
    !>    K_ij = \int  M(x) * (nabla psi_i . nabla psi_j)  dOmega
    !>    M_gp: Scalar coefficients evaluated at Gauss points [num_gauss]
    !>
    module subroutine compute_K2_diffusion_scalar_abst_fe(self, nodes, M_gp, elem_mat, &
                                                          work_psi, work_dpsi_dx)
        implicit none
        class(abst_fe), intent(in) :: self
        real(real64), intent(in) :: nodes(:, :)
        real(real64), intent(in) :: M_gp(:)
        real(real64), intent(inout) :: elem_mat(:, :)
        real(real64), intent(inout), optional, target :: work_psi(:)
        real(real64), intent(inout), optional, target :: work_dpsi_dx(:, :)

        integer(int32) :: p, i, j, num_nodes, dim, num_gauss, d
        real(real64) :: w, det_J, M_val, grad_dot
        type(type_coordinate_dp) :: r

        real(real64), pointer :: p_psi(:)
        real(real64), pointer :: p_dpsi_dx(:, :)

        real(real64), allocatable, target :: local_psi(:)
        real(real64), allocatable, target :: local_dpsi_dx(:, :)

        call self%get_dimension(dim)
        call self%get_num_nodes(num_nodes)
        call self%get_num_gauss(num_gauss)

        if (present(work_psi)) then
            p_psi => work_psi
        else
            call allocate_array(local_psi, num_nodes)
            p_psi => local_psi
        end if

        if (present(work_dpsi_dx)) then
            p_dpsi_dx => work_dpsi_dx
        else
            call allocate_array(local_dpsi_dx, dim, num_nodes)
            p_dpsi_dx => local_dpsi_dx
        end if

        elem_mat = 0.0d0

        do p = 1, num_gauss
            r = self%integration_rule%gauss(p)
            w = self%integration_rule%weight(p)

            call self%calc_shape_function(r, nodes, psi=p_psi, dpsi_dx=p_dpsi_dx, determinant_jacobian=det_J)

            M_val = M_gp(p)

            do j = 1, num_nodes
                do i = 1, num_nodes
                    grad_dot = 0.0d0
                    do d = 1, dim
                        grad_dot = grad_dot + p_dpsi_dx(d, i) * p_dpsi_dx(d, j)
                    end do

                    elem_mat(i, j) = elem_mat(i, j) + &
                                     w * abs(det_J) * M_val * grad_dot
                end do
            end do
        end do

        call deallocate_array(local_psi)
        call deallocate_array(local_dpsi_dx)

        nullify (p_psi)
        nullify (p_dpsi_dx)

    end subroutine compute_K2_diffusion_scalar_abst_fe

    !>
    !> 3. Mixed Matrix K3 (Advection/Convection)
    !>    K_ij = \int ((nabla psi_i . V(x)) * psi_j ) dOmega
    !>    V_gp: Vector coefficients evaluated at Gauss points [dim, num_gauss]
    !>
    module subroutine compute_K3_mixed_abst_fe(self, nodes, V_gp, elem_mat, work_psi, work_dpsi_dx)
        implicit none
        class(abst_fe), intent(in) :: self
        real(real64), intent(in) :: nodes(:, :)
        real(real64), intent(in) :: V_gp(:, :)
        real(real64), intent(inout) :: elem_mat(:, :)
        real(real64), intent(inout), optional, target :: work_psi(:)
        real(real64), intent(inout), optional, target :: work_dpsi_dx(:, :)

        integer(int32) :: p, i, j, num_nodes, dim, num_gauss, d
        real(real64) :: w, det_J, grad_psi_j_dot_V
        type(type_coordinate_dp) :: r

        real(real64), pointer :: p_psi(:)
        real(real64), pointer :: p_dpsi_dx(:, :)

        real(real64), allocatable, target :: local_psi(:)
        real(real64), allocatable, target :: local_dpsi_dx(:, :)

        call self%get_dimension(dim)
        call self%get_num_nodes(num_nodes)
        call self%get_num_gauss(num_gauss)

        if (present(work_psi)) then
            p_psi => work_psi
        else
            call allocate_array(local_psi, num_nodes)
            p_psi => local_psi
        end if

        if (present(work_dpsi_dx)) then
            p_dpsi_dx => work_dpsi_dx
        else
            call allocate_array(local_dpsi_dx, dim, num_nodes)
            p_dpsi_dx => local_dpsi_dx
        end if

        elem_mat = 0.0d0
        p_psi(:) = 0.0d0
        p_dpsi_dx(:, :) = 0.0d0

        do p = 1, num_gauss
            r = self%integration_rule%gauss(p)
            w = self%integration_rule%weight(p)

            call self%calc_shape_function(r, nodes, psi=p_psi, dpsi_dx=p_dpsi_dx, determinant_jacobian=det_J)

            do i = 1, num_nodes
                do j = 1, num_nodes
                    grad_psi_j_dot_V = 0.0d0
                    do d = 1, dim
                        grad_psi_j_dot_V = grad_psi_j_dot_V + p_dpsi_dx(d, j) * V_gp(d, p)
                    end do
                    elem_mat(i, j) = elem_mat(i, j) + w * abs(det_J) * p_psi(i) * grad_psi_j_dot_V
                end do
            end do
        end do

        call deallocate_array(local_psi)
        call deallocate_array(local_dpsi_dx)

        nullify (p_psi)
        nullify (p_dpsi_dx)

    end subroutine compute_K3_mixed_abst_fe

    !>
    !> 1. Scalar Source Residual R1
    !>    R_i = \int( \psi_i * S(x) ) d\Omega
    !>    S_gp: Source term evaluated at Gauss points [num_gauss]
    !>    work_psi: (Optional) Workspace for shape functions
    !>
    module subroutine compute_R1_source_abst_fe(self, nodes, S_gp, elem_vec, work_psi)
        implicit none
        class(abst_fe), intent(in) :: self
        real(real64), intent(in) :: nodes(:, :)
        real(real64), intent(in) :: S_gp(:)
        real(real64), intent(inout) :: elem_vec(:)
        real(real64), intent(inout), optional, target :: work_psi(:)

        integer(int32) :: p, i, num_nodes, dim, num_gauss
        real(real64) :: w, det_J, S_val
        type(type_coordinate_dp) :: r

        real(real64), pointer :: p_psi(:)

        real(real64), allocatable, target :: local_psi(:)

        call self%get_dimension(dim)
        call self%get_num_nodes(num_nodes)
        call self%get_num_gauss(num_gauss)

        if (present(work_psi)) then
            p_psi => work_psi
        else
            call allocate_array(local_psi, num_nodes)
            p_psi => local_psi
        end if

        elem_vec = 0.0d0

        do p = 1, num_gauss
            r = self%integration_rule%gauss(p)
            w = self%integration_rule%weight(p)

            call self%calc_shape_function(r, nodes, psi=p_psi, determinant_jacobian=det_J)

            S_val = S_gp(p)

            do i = 1, num_nodes
                elem_vec(i) = elem_vec(i) + w * abs(det_J) * S_val * p_psi(i)
            end do
        end do

        call deallocate_array(local_psi)
        nullify (p_psi)

    end subroutine compute_R1_source_abst_fe

    !>
    !> 1-Lumped. Lumped Scalar Source Residual R1 (Dedicated Routine)
    !>
    !>    Calculation:
    !>       R_i = S_node_i * \int \psi_i d\Omega
    !>
    !>    Physically equivalent to:
    !>       Residual_i = (Source Value at Node i) * (Lumped Volume of Node i)
    !>
    !>    S_node: Source term evaluated AT NODES [num_nodes]
    !>            (e.g., dU/dt at each node)
    !>
    module subroutine compute_R1_lumped_source_abst_fe(self, nodes, S_node, elem_vec, work_psi)
        implicit none
        class(abst_fe), intent(in) :: self
        real(real64), intent(in) :: nodes(:, :)
        real(real64), intent(in) :: S_node(:)
        real(real64), intent(inout) :: elem_vec(:)
        real(real64), intent(inout), optional, target :: work_psi(:)

        integer(int32) :: p, i, num_nodes, dim, num_gauss
        real(real64) :: w, det_J
        type(type_coordinate_dp) :: r

        real(real64), pointer :: p_psi(:)
        real(real64), allocatable, target :: local_psi(:)

        call self%get_dimension(dim)
        call self%get_num_nodes(num_nodes)
        call self%get_num_gauss(num_gauss)

        if (present(work_psi)) then
            p_psi => work_psi
        else
            call allocate_array(local_psi, num_nodes)
            p_psi => local_psi
        end if

        elem_vec = 0.0d0

        do p = 1, num_gauss
            r = self%integration_rule%gauss(p)
            w = self%integration_rule%weight(p)

            call self%calc_shape_function(r, nodes, psi=p_psi, determinant_jacobian=det_J)

            ! Compute (control volume) * (nodal value) for each node
            do i = 1, num_nodes
                ! Contribution from \int \psi_i d\Omega
                ! R_i += Volume_fraction * S_node_i
                elem_vec(i) = elem_vec(i) + w * abs(det_J) * p_psi(i) * S_node(i)
            end do
        end do

        if (.not. present(work_psi)) then
            call deallocate_array(local_psi)
        end if
        nullify (p_psi)

    end subroutine compute_R1_lumped_source_abst_fe

    !>
    !> 2. Flux/Divergence Residual R2
    !>    R_i = \int( \nabla \psi_i \cdot \mathbf{F}(x) ) d\Omega
    !>    F_gp: Flux vector evaluated at Gauss points [dim, num_gauss]
    !>    work_psi, work_dpsi_dx: (Optional) Workspace
    !>
    module subroutine compute_R2_flux_abst_fe(self, nodes, F_gp, elem_vec, work_dpsi_dx)
        implicit none
        class(abst_fe), intent(in) :: self
        real(real64), intent(in) :: nodes(:, :)
        real(real64), intent(in) :: F_gp(:, :)
        real(real64), intent(inout) :: elem_vec(:)
        real(real64), intent(inout), optional, target :: work_dpsi_dx(:, :)

        integer(int32) :: p, i, num_nodes, dim, num_gauss, d
        real(real64) :: w, det_J, grad_psi_i_dot_F
        type(type_coordinate_dp) :: r

        real(real64), pointer :: p_dpsi_dx(:, :)

        real(real64), allocatable, target :: local_dpsi_dx(:, :)

        call self%get_dimension(dim)
        call self%get_num_nodes(num_nodes)
        call self%get_num_gauss(num_gauss)

        if (present(work_dpsi_dx)) then
            p_dpsi_dx => work_dpsi_dx
        else
            call allocate_array(local_dpsi_dx, dim, num_nodes)
            p_dpsi_dx => local_dpsi_dx
        end if

        elem_vec = 0.0d0

        do p = 1, num_gauss
            r = self%integration_rule%gauss(p)
            w = self%integration_rule%weight(p)

            call self%calc_shape_function(r, nodes, dpsi_dx=p_dpsi_dx, determinant_jacobian=det_J)

            do i = 1, num_nodes
                grad_psi_i_dot_F = 0.0d0
                do d = 1, dim
                    grad_psi_i_dot_F = grad_psi_i_dot_F + p_dpsi_dx(d, i) * F_gp(d, p)
                end do

                elem_vec(i) = elem_vec(i) + w * abs(det_J) * grad_psi_i_dot_F
            end do
        end do

        call deallocate_array(local_dpsi_dx)

        nullify (p_dpsi_dx)

    end subroutine compute_R2_flux_abst_fe

end submodule domain_base_fe_compute
