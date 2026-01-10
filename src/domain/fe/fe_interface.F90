!>
!> Defines the abstract interface and common functionalities for finite elements.
!> Refactored to perform numerical integration using coefficients evaluated
!> directly at Gauss quadrature points, rather than interpolating nodal coefficients.
!>
module domain_fe
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: mpi_f08
    use :: stdlib_logger
    use :: stdlib_strings
    use :: module_core

    implicit none
    private

    public :: abst_fe
    public :: holder_fes

    !>
    !> An abstract base type for finite element objects.
    !>
    type, abstract :: abst_fe
        private
        !> The element type identifier.
        integer(int32) :: type
        !> The number of nodes defining the element.
        integer(int32) :: num_nodes
        !> The geometric dimension of the element.
        integer(int32) :: dimension
        !> The interpolation order.
        integer(int32) :: order
        !> The number of Gauss points.
        integer(int32) :: num_gauss
        !> Weights for Gauss integration points.
        real(real64), allocatable :: weight(:)
        !> Local coordinates of Gauss integration points.
        type(type_coordinate_dp), allocatable :: gauss(:)

    contains
        !----------------------------------------------------------------------
        ! Public methods with common implementations
        !----------------------------------------------------------------------
        procedure, pass(self), public :: initialize => initialize_abst_fe
        procedure, pass(self), public :: destroy => destroy_abst_fe
        procedure, pass(self), public :: get_type
        procedure, pass(self), public :: get_num_nodes
        procedure, pass(self), public :: get_dimension
        procedure, pass(self), public :: get_order
        procedure, pass(self), public :: get_num_gauss
        procedure, pass(self), public :: get_weight
        procedure, pass(self), public :: get_gauss

        !> State interpolation methods (used by assembler to get T/P at Gauss points)
        procedure, pass(self), private :: lerp_1d
        procedure, pass(self), private :: lerp_2d
        procedure, pass(self), private :: lerp_3d
        generic, public :: lerp => lerp_1d, lerp_2d, lerp_3d

        procedure, pass(self), private :: dlerp_1d
        generic, public :: dlerp => dlerp_1d

        procedure, pass(self), public :: display

        !>
        !> Calculates shape functions, physical gradients, and Jacobian determinant
        !> at a given local coordinate.
        !>
        procedure, pass(self), public :: calc_shape_data

        !> Integration routines taking Gauss-point values
        procedure, pass(self), public :: compute_K1 => compute_K1_capacity
        procedure, pass(self), private :: compute_K2_diffusion
        procedure, pass(self), private :: compute_K2_diffusion_scalar
        generic, public :: compute_K2 => compute_K2_diffusion, compute_K2_diffusion_scalar
        procedure, pass(self), public :: compute_K3 => compute_K3_mixed

        procedure, pass(self), public :: compute_R1 => compute_R1_source
        procedure, pass(self), public :: compute_R2 => compute_R2_flux

        !----------------------------------------------------------------------
        ! Abstract methods to be implemented in derived types
        !----------------------------------------------------------------------
        procedure(abst_get_geometry), pass(self), public, deferred :: get_geometry
        procedure(abst_psi), pass(self), public, deferred :: psi
        procedure(abst_dpsi), pass(self), public, deferred :: dpsi
        procedure(abst_jacobian), pass(self), public, deferred :: jacobian
        procedure(abst_jacobian_det), pass(self), public, deferred :: jacobian_det
        procedure(abst_is_inside), pass(self), public, deferred :: is_inside
    end type

    abstract interface
        subroutine abst_get_geometry(self, node_coords, geometry)
            import :: abst_fe, int32, real64
            class(abst_fe), intent(in) :: self
            real(real64), intent(in) :: node_coords(:, :)
            real(real64), intent(inout) :: geometry
        end subroutine abst_get_geometry

        pure elemental subroutine abst_psi(self, i, r, psi_val)
            import :: abst_fe, type_coordinate_dp, int32, real64
            class(abst_fe), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(inout) :: psi_val
        end subroutine abst_psi

        pure elemental subroutine abst_dpsi(self, i, j, r, dpsi_val)
            import :: abst_fe, type_coordinate_dp, int32, real64
            class(abst_fe), intent(in) :: self
            integer(int32), intent(in) :: i
            integer(int32), intent(in) :: j
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(inout) :: dpsi_val
        end subroutine abst_dpsi

        pure subroutine abst_jacobian(self, r, node_coords, jac)
            import :: abst_fe, type_coordinate_dp, int32, real64
            class(abst_fe), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :)
            real(real64), intent(inout) :: jac(:, :)
        end subroutine abst_jacobian

        pure subroutine abst_jacobian_det(self, r, node_coords, det_j)
            import :: abst_fe, type_coordinate_dp, int32, real64
            class(abst_fe), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :)
            real(real64), intent(inout) :: det_j
        end subroutine abst_jacobian_det

        subroutine abst_is_inside(self, cartesian, normalized, node_coords, is_in)
            import abst_fe, type_coordinate_dp, int32, real64
            class(abst_fe), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: cartesian
            type(type_coordinate_dp), intent(inout) :: normalized
            real(real64), intent(in) :: node_coords(:, :)
            logical, intent(inout) :: is_in
        end subroutine abst_is_inside
    end interface

    !> Wrapper for polymorphic FE objects
    type :: holder_fes
        class(abst_fe), allocatable :: fe
    end type holder_fes

contains

    !>
    !> Computes shape functions, their global gradients, and the Jacobian determinant.
    !>
    pure subroutine calc_shape_data(self, r, node_coords, psi, dpsi_dx, det_j)
        implicit none
        class(abst_fe), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)

        real(real64), intent(inout), optional :: psi(:)
        real(real64), intent(inout), optional :: dpsi_dx(:, :)
        real(real64), intent(inout), optional :: det_j

        integer(int32) :: i, j, dim, nn
        real(real64) :: jac(3, 3), inv_jac(3, 3)
        real(real64) :: dpsi_dxi(3) ! Local gradients

        dim = self%dimension
        nn = self%num_nodes

        ! 1. Evaluate Shape Functions
        if (present(psi)) then
            psi(:) = 0.0d0
            do i = 1, nn
                call self%psi(i, r, psi(i))
            end do
        end if

        if (present(dpsi_dx)) then
            dpsi_dx(:, :) = 0.0d0
            ! We need local gradients to compute Jacobian
        end if

        ! 2. Compute Jacobian Matrix (Isoparametric formulation)
        jac = 0.0d0
        call self%jacobian(r, node_coords, jac)
        call self%jacobian_det(r, node_coords, det_j)

        ! 3. Compute Determinant and Inverse Jacobian
        call calc_inverse_matrix(dim, jac(1:dim, 1:dim), det_j, inv_jac(1:dim, 1:dim))

        ! 4. Compute Global Gradients if requested
        if (present(dpsi_dx)) then
            do i = 1, nn
                ! Get local gradients again (or optimize to reuse)
                do j = 1, dim
                    call self%dpsi(i, j, r, dpsi_dxi(j))
                end do

                ! Transform to global coordinates
                if (dim == 1) then
                    dpsi_dx(i, 1) = dpsi_dxi(1) * inv_jac(1, 1)
                else if (dim == 2) then
                    dpsi_dx(i, 1) = dpsi_dxi(1) * inv_jac(1, 1) + dpsi_dxi(2) * inv_jac(2, 1)
                    dpsi_dx(i, 2) = dpsi_dxi(1) * inv_jac(1, 2) + dpsi_dxi(2) * inv_jac(2, 2)
                else if (dim == 3) then
                    dpsi_dx(i, 1) = dot_product(dpsi_dxi, inv_jac(:, 1))
                    dpsi_dx(i, 2) = dot_product(dpsi_dxi, inv_jac(:, 2))
                    dpsi_dx(i, 3) = dot_product(dpsi_dxi, inv_jac(:, 3))
                end if
            end do
        end if

    end subroutine calc_shape_data

    pure subroutine calc_inverse_matrix(dim, A, det, A_inv)
        implicit none
        integer(int32), intent(in) :: dim
        real(real64), intent(in) :: A(:, :)
        real(real64), intent(inout) :: det
        real(real64), intent(inout) :: A_inv(:, :)
        real(real64) :: inv_det

        select case (dim)
        case (1)
            det = A(1, 1)
            if (abs(det) > epsilon(det)) then
                A_inv(1, 1) = 1.0d0 / det
            else
                A_inv(1, 1) = 0.0d0
            end if
        case (2)
            det = A(1, 1) * A(2, 2) - A(1, 2) * A(2, 1)
            if (abs(det) > epsilon(det)) then
                inv_det = 1.0d0 / det
                A_inv(1, 1) = A(2, 2) * inv_det
                A_inv(1, 2) = -A(1, 2) * inv_det
                A_inv(2, 1) = -A(2, 1) * inv_det
                A_inv(2, 2) = A(1, 1) * inv_det
            else
                A_inv = 0.0d0
            end if
        case (3)
            det = A(1, 1) * (A(2, 2) * A(3, 3) - A(2, 3) * A(3, 2)) &
                  - A(1, 2) * (A(2, 1) * A(3, 3) - A(2, 3) * A(3, 1)) &
                  + A(1, 3) * (A(2, 1) * A(3, 2) - A(2, 2) * A(3, 1))

            if (abs(det) > epsilon(det)) then
                inv_det = 1.0d0 / det
                A_inv(1, 1) = (A(2, 2) * A(3, 3) - A(2, 3) * A(3, 2)) * inv_det
                A_inv(1, 2) = (A(1, 3) * A(3, 2) - A(1, 2) * A(3, 3)) * inv_det
                A_inv(1, 3) = (A(1, 2) * A(2, 3) - A(1, 3) * A(2, 2)) * inv_det

                A_inv(2, 1) = (A(2, 3) * A(3, 1) - A(2, 1) * A(3, 3)) * inv_det
                A_inv(2, 2) = (A(1, 1) * A(3, 3) - A(1, 3) * A(3, 1)) * inv_det
                A_inv(2, 3) = (A(1, 3) * A(2, 1) - A(1, 1) * A(2, 3)) * inv_det

                A_inv(3, 1) = (A(2, 1) * A(3, 2) - A(2, 2) * A(3, 1)) * inv_det
                A_inv(3, 2) = (A(1, 2) * A(3, 1) - A(1, 1) * A(3, 2)) * inv_det
                A_inv(3, 3) = (A(1, 1) * A(2, 2) - A(1, 2) * A(2, 1)) * inv_det
            else
                A_inv = 0.0d0
            end if
        end select
    end subroutine calc_inverse_matrix

    !--------------------------------------------------------------------------
    ! Implementations of existing public methods
    !--------------------------------------------------------------------------

    subroutine initialize_abst_fe(self, type, dimension, order, num_nodes, num_gauss, weight, gauss)
        ! (No changes)
        implicit none
        class(abst_fe), intent(inout) :: self
        integer(int32), intent(in) :: type
        integer(int32), intent(in) :: dimension
        integer(int32), intent(in) :: order
        integer(int32), intent(in) :: num_nodes
        integer(int32), intent(in) :: num_gauss
        real(real64), intent(in) :: weight(:)
        real(real64), intent(in) :: gauss(:, :)
        integer(int32) :: i

        self%type = type
        self%dimension = dimension
        self%order = order
        self%num_nodes = num_nodes
        self%num_gauss = num_gauss

        call allocate_array(self%weight, self%num_gauss)
        self%weight(:) = weight(:)

        allocate (self%gauss(self%num_gauss))
        do i = 1, self%num_gauss
            call self%gauss(i)%set(gauss(1, i), gauss(2, i), gauss(3, i))
        end do
    end subroutine initialize_abst_fe

    ! (Getters and Lerp functions remain the same to support state interpolation)
    pure elemental subroutine get_type(self, type)
        implicit none
        class(abst_fe), intent(in) :: self
        integer(int32), intent(inout) :: type
        type = self%type
    end subroutine get_type

    pure elemental subroutine get_num_nodes(self, num_nodes)
        implicit none
        class(abst_fe), intent(in) :: self
        integer(int32), intent(inout) :: num_nodes
        num_nodes = self%num_nodes
    end subroutine get_num_nodes

    pure elemental subroutine get_dimension(self, dimension)
        implicit none
        class(abst_fe), intent(in) :: self
        integer(int32), intent(inout) :: dimension
        dimension = self%dimension
    end subroutine get_dimension

    pure elemental subroutine get_order(self, order)
        implicit none
        class(abst_fe), intent(in) :: self
        integer(int32), intent(inout) :: order
        order = self%order
    end subroutine get_order

    pure elemental subroutine get_num_gauss(self, num_gauss)
        implicit none
        class(abst_fe), intent(in) :: self
        integer(int32), intent(inout) :: num_gauss
        num_gauss = self%num_gauss
    end subroutine get_num_gauss

    subroutine get_weight(self, weight)
        implicit none
        class(abst_fe), intent(in), target :: self
        real(real64), intent(inout), pointer, contiguous, dimension(:) :: weight
        weight => self%weight
    end subroutine get_weight

    subroutine get_gauss(self, gauss)
        implicit none
        class(abst_fe), intent(in), target :: self
        type(type_coordinate_dp), intent(inout), pointer, contiguous, dimension(:) :: gauss
        gauss => self%gauss
    end subroutine get_gauss

    pure subroutine lerp_1d(self, r, values, lerped_value)
        ! (No changes - needed for State variable interpolation)
        implicit none
        class(abst_fe), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: values(:)
        real(real64), intent(inout) :: lerped_value
        integer(int32) :: i
        real(real64) :: psi_i

        lerped_value = 0.0d0
        do i = 1, self%num_nodes
            call self%psi(i, r, psi_i)
            lerped_value = lerped_value + psi_i * values(i)
        end do
    end subroutine

    pure subroutine lerp_2d(self, r, values, lerped_values)
        implicit none
        class(abst_fe), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: values(:, :)
        real(real64), intent(inout) :: lerped_values(:)
        integer(int32) :: i
        real(real64) :: psi_i

        lerped_values(:) = 0.0d0
        do i = 1, self%num_nodes
            call self%psi(i, r, psi_i)
            lerped_values(:) = lerped_values(:) + psi_i * values(:, i)
        end do
    end subroutine lerp_2d

    pure subroutine lerp_3d(self, r, values, lerped_values)
        implicit none
        class(abst_fe), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: values(:, :, :)
        real(real64), intent(inout) :: lerped_values(:, :)
        integer(int32) :: i
        real(real64) :: psi_i

        lerped_values = 0.0d0
        do i = 1, self%num_nodes
            call self%psi(i, r, psi_i)
            lerped_values(:, :) = lerped_values(:, :) + psi_i * values(:, :, i)
        end do
    end subroutine lerp_3d

    pure subroutine dlerp_1d(self, r, values, dlerped_value)
        implicit none
        class(abst_fe), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: values(:)
        type(type_coordinate_dp), intent(inout) :: dlerped_value
        integer(int32) :: i
        real(real64) :: dpsi_i

        dlerped_value%x = 0.0d0; dlerped_value%y = 0.0d0; dlerped_value%z = 0.0d0
        do i = 1, self%num_nodes
            if (self%dimension >= 1) then
                call self%dpsi(i, 1, r, dpsi_i)
                dlerped_value%x = dlerped_value%x + dpsi_i * values(i)
            end if
            if (self%dimension >= 2) then
                call self%dpsi(i, 2, r, dpsi_i)
                dlerped_value%y = dlerped_value%y + dpsi_i * values(i)
                dlerped_value%z = dlerped_value%z + dpsi_i * values(i)
            end if
            if (self%dimension >= 3) then
                call self%dpsi(i, 3, r, dpsi_i)
                dlerped_value%z = dlerped_value%z + dpsi_i * values(i)
            end if
        end do
    end subroutine dlerp_1d

    !==========================================================================
    ! Integration Matrices using Coefficients at Gauss Points (A_gp, M_gp, etc.)
    !==========================================================================
    !>
    !> 1. Capacity Matrix K1
    !>    K_ij = \int  A(x) * psi_i * psi_j  dOmega
    !>    A_gp: Coefficients evaluated directly at Gauss points [num_gauss]
    !>    work_psi, work_dpsi_dx: (Optional) Pre-allocated workspace to avoid repeated allocation.
    !>
    subroutine compute_K1_capacity(self, nodes, A_gp, elem_mat, work_psi)
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
            r = self%gauss(p)
            w = self%weight(p)

            ! 形状関数とヤコビアン行列式の計算
            call self%calc_shape_data(r, nodes, psi=p_psi, det_j=det_J)
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
    end subroutine compute_K1_capacity

    !>
    !> 2. Diffusion Matrix K2
    !>    K_ij =  \int  nabla psi_i . (M(x) * nabla psi_j)  dOmega
    !>    M_gp: Tensor coefficients evaluated at Gauss points [dim, dim, num_gauss]
    !>
    subroutine compute_K2_diffusion(self, nodes, M_gp, elem_mat, &
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
            r = self%gauss(p)
            w = self%weight(p)

            call self%calc_shape_data(r, nodes, psi=p_psi, dpsi_dx=p_dpsi_dx, det_j=det_J)

            ! Compute term: M * nabla psi_j
            do j = 1, nd
                ! Matrix-Vector multiplication using pointers
                ! M_gp(:, :, p) is [dim x dim], p_dpsi_dx(j, :) is [dim]
                p_M_grad_psi_j = matmul(M_gp(:, :, p), p_dpsi_dx(j, :))

                do i = 1, nd
                    ! nabla psi_i . (M * nabla psi_j)
                    elem_mat(i, j) = elem_mat(i, j) + &
                                     w * det_J * dot_product(p_dpsi_dx(i, :), p_M_grad_psi_j)
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

    end subroutine compute_K2_diffusion

    !>
    !> 2. Diffusion Matrix K2 (Scalar Version)
    !>    K_ij = \int  M(x) * (nabla psi_i . nabla psi_j)  dOmega
    !>    M_gp: Scalar coefficients evaluated at Gauss points [num_gauss]
    !>
    subroutine compute_K2_diffusion_scalar(self, nodes, M_gp, elem_mat, &
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
            r = self%gauss(p)
            w = self%weight(p)

            call self%calc_shape_data(r, nodes, psi=p_psi, dpsi_dx=p_dpsi_dx, det_j=det_J)

            ! Direct evaluation at Gauss point (Scalar)
            M_val = M_gp(p)

            do j = 1, nd
                do i = 1, nd
                    ! nabla psi_i . nabla psi_j (Simple dot product)
                    grad_dot = dot_product(p_dpsi_dx(i, :), p_dpsi_dx(j, :))

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

    end subroutine compute_K2_diffusion_scalar

    !>
    !> 3. Mixed Matrix K3 (Advection/Convection)
    !>    K_ij = \int ((nabla psi_i . V(x)) * psi_j ) dOmega
    !>    V_gp: Vector coefficients evaluated at Gauss points [dim, num_gauss]
    !>
    subroutine compute_K3_mixed(self, nodes, V_gp, elem_mat, work_psi, work_dpsi_dx)
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
            r = self%gauss(p)
            w = self%weight(p)

            call self%calc_shape_data(r, nodes, psi=p_psi, dpsi_dx=p_dpsi_dx, det_j=det_J)

            do i = 1, nd
                ! (nabla psi_i . V)
                grad_psi_i_dot_V = dot_product(p_dpsi_dx(i, :), V_gp(:, p))

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

    end subroutine compute_K3_mixed

    !>
    !> 1. Scalar Source Residual R1
    !>    R_i = \int( \psi_i * S(x) ) d\Omega
    !>    S_gp: Source term evaluated at Gauss points [num_gauss]
    !>    work_psi: (Optional) Workspace for shape functions
    !>
    subroutine compute_R1_source(self, nodes, S_gp, elem_vec, work_psi)
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
            r = self%gauss(p)
            w = self%weight(p)

            ! R1では dpsi_dx は不要なので、psi のみ計算
            call self%calc_shape_data(r, nodes, psi=p_psi, det_j=det_J)

            S_val = S_gp(p)

            do i = 1, nd
                elem_vec(i) = elem_vec(i) + w * det_J * S_val * p_psi(i)
            end do
        end do

        ! --- Cleanup ---
        if (allocated(local_psi)) call deallocate_array(local_psi)
        nullify (p_psi)

    end subroutine compute_R1_source

    !>
    !> 2. Flux/Divergence Residual R2
    !>    R_i = \int( \nabla \psi_i \cdot \mathbf{F}(x) ) d\Omega
    !>    F_gp: Flux vector evaluated at Gauss points [dim, num_gauss]
    !>    work_psi, work_dpsi_dx: (Optional) Workspace
    !>
    subroutine compute_R2_flux(self, nodes, F_gp, elem_vec, &
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
            r = self%gauss(p)
            w = self%weight(p)

            ! R2では dpsi_dx が必須。psiは幾何計算(Jacobian)で内部的に使われる可能性があるため渡しておく
            call self%calc_shape_data(r, nodes, psi=p_psi, dpsi_dx=p_dpsi_dx, det_j=det_J)

            ! Direct evaluation: F_gp(:, p)
            do i = 1, nd
                ! (nabla psi_i . F)
                grad_psi_i_dot_F = dot_product(p_dpsi_dx(i, :), F_gp(:, p))

                elem_vec(i) = elem_vec(i) + w * det_J * grad_psi_i_dot_F
            end do
        end do

        ! --- Cleanup ---
        if (allocated(local_psi)) call deallocate_array(local_psi)
        if (allocated(local_dpsi_dx)) call deallocate_array(local_dpsi_dx)

        nullify (p_psi)
        nullify (p_dpsi_dx)

    end subroutine compute_R2_flux

    subroutine display(self)
        implicit none
        class(abst_fe), intent(in) :: self
        character(256) :: msg
        integer(int32) :: i

        call global_logger%log_information("========================================================")
        call global_logger%log_information(" Finite Element Information ")
        call global_logger%log_information("========================================================")
        write (msg, '(a, i0)') "Element Type       : ", self%type
        call global_logger%log_information(strip(msg))
        write (msg, '(a, i0)') "Dimension          : ", self%dimension
        call global_logger%log_information(strip(msg))
        write (msg, '(a, i0)') "Order              : ", self%order
        call global_logger%log_information(strip(msg))
        write (msg, '(a, i0)') "Number of Nodes    : ", self%num_nodes
        call global_logger%log_information(strip(msg))
        write (msg, '(a, i0)') "Number of Gauss Pts: ", self%num_gauss
        call global_logger%log_information(strip(msg))

        if (self%num_gauss > 0) then
            call global_logger%log_information("--------------------------------------------------------")
            call global_logger%log_information("Gauss Quadrature Points and Weights:")
            call global_logger%log_information("--------------------------------------------------------")
            do i = 1, self%num_gauss
                write (msg, '(a, i2, a, 3(f12.8, a), f12.8)') "  GP ", i, ": (", &
                    self%gauss(i)%x, ", ", self%gauss(i)%y, ", ", self%gauss(i)%z, &
                    ")  Weight = ", self%weight(i)
                call global_logger%log_information(strip(msg))
            end do
        end if
        call global_logger%log_information("========================================================")
    end subroutine display

    subroutine destroy_abst_fe(self)
        implicit none
        class(abst_fe), intent(inout) :: self

        self%type = 0
        self%num_nodes = 0
        self%dimension = 0
        self%order = 0
        self%num_gauss = 0
        if (allocated(self%weight)) deallocate (self%weight)
        if (allocated(self%gauss)) deallocate (self%gauss)
    end subroutine destroy_abst_fe

end module domain_fe
