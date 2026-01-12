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

        integer(int32) :: p, i, j, nd, dim, num_gauss
        real(real64) :: w, det_J, A_val
        type(type_coordinate_dp) :: r

        real(real64), pointer :: p_psi(:) => null()
        real(real64), allocatable, target :: local_psi(:)

        ! --- 1. 次元情報の取得 ---
        call self%get_dimension(dim)
        call self%get_num_nodes(nd)
        call self%get_num_gauss(num_gauss)

        ! --- 2. ワークスペースのセットアップ（ポインタエイリアシング） ---
        ! psi のセットアップ
        if (present(work_psi)) then
            p_psi => work_psi
        else
            call allocate_array(local_psi, nd)
            p_psi => local_psi
        end if

        elem_mat(:, :) = 0.0d0
        p_psi(:) = 0.0d0

        ! --- 3. 数値積分ループ ---
        do p = 1, num_gauss
            r = self%integration_rule%gauss(p)
            w = self%integration_rule%weight(p)

            ! 形状関数とヤコビアン行列式の計算
            call self%calc_shape_function(r, nodes, psi=p_psi, determinant_jacobian=det_J)
            A_val = A_gp(p)

            ! 行列積算
            do j = 1, nd
                do i = 1, nd
                    ! p_psi ポインタ経由でアクセス
                    elem_mat(i, j) = elem_mat(i, j) + w * det_J * A_val * p_psi(i) * p_psi(j)
                end do
            end do
        end do

        ! --- 4. クリーンアップ ---
        ! 自分で確保したローカル配列のみ解放する
        nullify (p_psi)
        if (.not. present(work_psi)) then
            call deallocate_array(local_psi)
        end if
    end subroutine compute_K1_capacity_abst_fe

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
        !> Optional workspace
        real(real64), intent(inout), optional, target :: work_psi(:)
        real(real64), intent(inout), optional, target :: work_dpsi_dx(:, :)
        real(real64), intent(inout), optional, target :: work_vec(:) ! For M_grad_psi_j [dim]

        integer(int32) :: p, i, j, nd, dim, num_gauss
        integer(int32) :: ierr
        real(real64) :: w, det_J
        type(type_coordinate_dp) :: r

        !> Pointers for alias
        real(real64), pointer :: p_psi(:) => null()
        real(real64), pointer :: p_dpsi_dx(:, :) => null()
        real(real64), pointer :: p_M_grad_psi_j(:) => null()

        !> Local allocatables (fallback)
        real(real64), allocatable, target :: local_psi(:)
        real(real64), allocatable, target :: local_dpsi_dx(:, :)
        real(real64), allocatable, target :: local_vec_dim(:)

        call self%get_dimension(dim)
        call self%get_num_nodes(nd)
        call self%get_num_gauss(num_gauss)

        ! --- Workspace Setup ---
        if (present(work_psi)) then
            p_psi => work_psi
        else
            call allocate_array(local_psi, nd)
            p_psi => local_psi
        end if

        if (present(work_dpsi_dx)) then
            p_dpsi_dx => work_dpsi_dx
        else
            call allocate_array(local_dpsi_dx, nd, dim)
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

        ! --- Integration Loop ---
        do p = 1, num_gauss
            r = self%integration_rule%gauss(p)
            w = self%integration_rule%weight(p)

            call self%calc_shape_function(r, nodes, psi=p_psi, dpsi_dx=p_dpsi_dx, determinant_jacobian=det_J)

            ! Compute term: M * nabla psi_j
            do j = 1, nd
                ! Matrix-Vector multiplication using pointers
                ! M_gp(:, :, p) is [dim x dim], p_dpsi_dx(j, :) is [dim]
                call matvec(M_gp(:, :, p), p_dpsi_dx(j, :), p_M_grad_psi_j, ierr)

                do i = 1, nd
                    ! nabla psi_i . (M * nabla psi_j)
                    elem_mat(i, j) = elem_mat(i, j) + &
                                     w * det_J * vector_dot(p_dpsi_dx(i, :), p_M_grad_psi_j)
                end do
            end do
        end do

        ! --- Cleanup ---
        if (allocated(local_psi)) call deallocate_array(local_psi)
        if (allocated(local_dpsi_dx)) call deallocate_array(local_dpsi_dx)
        if (allocated(local_vec_dim)) call deallocate_array(local_vec_dim)

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
        !> Scalar coefficient at Gauss points
        real(real64), intent(in) :: M_gp(:)
        real(real64), intent(inout) :: elem_mat(:, :)
        !> Optional workspace
        real(real64), intent(inout), optional, target :: work_psi(:)
        real(real64), intent(inout), optional, target :: work_dpsi_dx(:, :)

        integer(int32) :: p, i, j, nd, dim, num_gauss
        real(real64) :: w, det_J, M_val, grad_dot
        type(type_coordinate_dp) :: r

        !> Pointers for alias
        real(real64), pointer :: p_psi(:) => null()
        real(real64), pointer :: p_dpsi_dx(:, :) => null()

        !> Local allocatables (fallback)
        real(real64), allocatable, target :: local_psi(:)
        real(real64), allocatable, target :: local_dpsi_dx(:, :)

        call self%get_dimension(dim)
        call self%get_num_nodes(nd)
        call self%get_num_gauss(num_gauss)

        ! --- Workspace Setup ---
        if (present(work_psi)) then
            p_psi => work_psi
        else
            call allocate_array(local_psi, nd)
            p_psi => local_psi
        end if

        if (present(work_dpsi_dx)) then
            p_dpsi_dx => work_dpsi_dx
        else
            call allocate_array(local_dpsi_dx, nd, dim)
            p_dpsi_dx => local_dpsi_dx
        end if

        elem_mat = 0.0d0

        ! --- Integration Loop ---
        do p = 1, num_gauss
            r = self%integration_rule%gauss(p)
            w = self%integration_rule%weight(p)

            call self%calc_shape_function(r, nodes, psi=p_psi, dpsi_dx=p_dpsi_dx, determinant_jacobian=det_J)

            ! Direct evaluation at Gauss point (Scalar)
            M_val = M_gp(p)

            do j = 1, nd
                do i = 1, nd
                    ! nabla psi_i . nabla psi_j (Simple dot product)
                    grad_dot = vector_dot(p_dpsi_dx(i, :), p_dpsi_dx(j, :))

                    elem_mat(i, j) = elem_mat(i, j) + &
                                     w * det_J * M_val * grad_dot
                end do
            end do
        end do

        ! --- Cleanup ---
        if (allocated(local_psi)) call deallocate_array(local_psi)
        if (allocated(local_dpsi_dx)) call deallocate_array(local_dpsi_dx)

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
        !> Optional workspace
        real(real64), intent(inout), optional, target :: work_psi(:)
        real(real64), intent(inout), optional, target :: work_dpsi_dx(:, :)

        integer(int32) :: p, i, j, nd, dim, num_gauss
        real(real64) :: w, det_J, grad_psi_i_dot_V
        type(type_coordinate_dp) :: r

        !> Pointers for alias
        real(real64), pointer :: p_psi(:) => null()
        real(real64), pointer :: p_dpsi_dx(:, :) => null()

        !> Local allocatables (fallback)
        real(real64), allocatable, target :: local_psi(:)
        real(real64), allocatable, target :: local_dpsi_dx(:, :)

        call self%get_dimension(dim)
        call self%get_num_nodes(nd)
        call self%get_num_gauss(num_gauss)

        ! --- Workspace Setup ---
        if (present(work_psi)) then
            p_psi => work_psi
        else
            call allocate_array(local_psi, nd)
            p_psi => local_psi
        end if

        if (present(work_dpsi_dx)) then
            p_dpsi_dx => work_dpsi_dx
        else
            call allocate_array(local_dpsi_dx, nd, dim)
            p_dpsi_dx => local_dpsi_dx
        end if

        elem_mat = 0.0d0
        p_psi(:) = 0.0d0
        p_dpsi_dx(:, :) = 0.0d0

        ! --- Integration Loop ---
        do p = 1, num_gauss
            r = self%integration_rule%gauss(p)
            w = self%integration_rule%weight(p)

            call self%calc_shape_function(r, nodes, psi=p_psi, dpsi_dx=p_dpsi_dx, determinant_jacobian=det_J)

            do i = 1, nd
                ! (nabla psi_i . V)
                grad_psi_i_dot_V = vector_dot(p_dpsi_dx(i, :), V_gp(:, p))

                do j = 1, nd
                    elem_mat(i, j) = elem_mat(i, j) + &
                                     w * det_J * grad_psi_i_dot_V * p_psi(j)
                end do
            end do
        end do

        ! --- Cleanup ---
        if (allocated(local_psi)) call deallocate_array(local_psi)
        if (allocated(local_dpsi_dx)) call deallocate_array(local_dpsi_dx)

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
        !> Optional workspace
        real(real64), intent(inout), optional, target :: work_psi(:)

        integer(int32) :: p, i, nd, dim, num_gauss
        real(real64) :: w, det_J, S_val
        type(type_coordinate_dp) :: r

        !> Pointers for alias
        real(real64), pointer :: p_psi(:) => null()

        !> Local allocatables (fallback)
        real(real64), allocatable, target :: local_psi(:)

        call self%get_dimension(dim)
        call self%get_num_nodes(nd)
        call self%get_num_gauss(num_gauss)

        ! --- Workspace Setup ---
        if (present(work_psi)) then
            p_psi => work_psi
        else
            call allocate_array(local_psi, nd)
            p_psi => local_psi
        end if

        elem_vec = 0.0d0

        ! --- Integration Loop ---
        do p = 1, num_gauss
            r = self%integration_rule%gauss(p)
            w = self%integration_rule%weight(p)

            ! R1では dpsi_dx は不要なので、psi のみ計算
            call self%calc_shape_function(r, nodes, psi=p_psi, determinant_jacobian=det_J)

            S_val = S_gp(p)

            do i = 1, nd
                elem_vec(i) = elem_vec(i) + w * det_J * S_val * p_psi(i)
            end do
        end do

        ! --- Cleanup ---
        if (allocated(local_psi)) call deallocate_array(local_psi)
        nullify (p_psi)

    end subroutine compute_R1_source_abst_fe

    !>
    !> 2. Flux/Divergence Residual R2
    !>    R_i = \int( \nabla \psi_i \cdot \mathbf{F}(x) ) d\Omega
    !>    F_gp: Flux vector evaluated at Gauss points [dim, num_gauss]
    !>    work_psi, work_dpsi_dx: (Optional) Workspace
    !>
    module subroutine compute_R2_flux_abst_fe(self, nodes, F_gp, elem_vec, &
                                              work_psi, work_dpsi_dx)
        implicit none
        class(abst_fe), intent(in) :: self
        real(real64), intent(in) :: nodes(:, :)
        real(real64), intent(in) :: F_gp(:, :)
        real(real64), intent(inout) :: elem_vec(:)
        !> Optional workspace
        real(real64), intent(inout), optional, target :: work_psi(:)
        real(real64), intent(inout), optional, target :: work_dpsi_dx(:, :)

        integer(int32) :: p, i, nd, dim, num_gauss
        real(real64) :: w, det_J, grad_psi_i_dot_F
        type(type_coordinate_dp) :: r

        !> Pointers for alias
        real(real64), pointer :: p_psi(:) => null()
        real(real64), pointer :: p_dpsi_dx(:, :) => null()

        !> Local allocatables (fallback)
        real(real64), allocatable, target :: local_psi(:)
        real(real64), allocatable, target :: local_dpsi_dx(:, :)

        call self%get_dimension(dim)
        call self%get_num_nodes(nd)
        call self%get_num_gauss(num_gauss)

        ! --- Workspace Setup ---
        if (present(work_psi)) then
            p_psi => work_psi
        else
            call allocate_array(local_psi, nd)
            p_psi => local_psi
        end if

        if (present(work_dpsi_dx)) then
            p_dpsi_dx => work_dpsi_dx
        else
            call allocate_array(local_dpsi_dx, nd, dim)
            p_dpsi_dx => local_dpsi_dx
        end if

        elem_vec = 0.0d0

        ! --- Integration Loop ---
        do p = 1, num_gauss
            r = self%integration_rule%gauss(p)
            w = self%integration_rule%weight(p)

            ! R2では dpsi_dx が必須。psiは幾何計算(Jacobian)で内部的に使われる可能性があるため渡しておく
            call self%calc_shape_function(r, nodes, psi=p_psi, dpsi_dx=p_dpsi_dx, determinant_jacobian=det_J)

            ! Direct evaluation: F_gp(:, p)
            do i = 1, nd
                ! (nabla psi_i . F)
                grad_psi_i_dot_F = vector_dot(p_dpsi_dx(i, :), F_gp(:, p))

                elem_vec(i) = elem_vec(i) + w * det_J * grad_psi_i_dot_F
            end do
        end do

        ! --- Cleanup ---
        if (allocated(local_psi)) call deallocate_array(local_psi)
        if (allocated(local_dpsi_dx)) call deallocate_array(local_dpsi_dx)

        nullify (p_psi)
        nullify (p_dpsi_dx)

    end subroutine compute_R2_flux_abst_fe

end submodule domain_base_fe_compute
