!>
!> Defines the abstract interface and common functionalities for finite elements.
!> Refactored to provide high-level geometric shape data calculation.
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
        procedure, pass(self), public :: lerp
        procedure, pass(self), public :: dlerp
        procedure, pass(self), public :: display

        !>
        !> Calculates shape functions, physical gradients, and Jacobian determinant
        !> at a given local coordinate. This acts as a high-level API for assemblers.
        !>
        procedure, pass(self), public :: calc_shape_data
        procedure, pass(self), private :: compute_K1_capacity
        procedure, pass(self), private :: compute_K2_diffusion
        procedure, pass(self), private :: compute_K3_mixed
        generic, public :: compute_K => compute_K1_capacity, compute_K2_diffusion, compute_K3_mixed

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
        subroutine abst_get_geometry(self, node_coords, connectivity, geometry)
            import :: abst_fe, int32, real64
            class(abst_fe), intent(in) :: self
            real(real64), intent(in) :: node_coords(:, :)
            integer(int32), intent(in) :: connectivity(:)
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

        pure subroutine abst_jacobian(self, r, node_coords, connectivity, jac)
            import :: abst_fe, type_coordinate_dp, int32, real64
            class(abst_fe), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :)
            integer(int32), intent(in) :: connectivity(:)
            real(real64), intent(inout) :: jac(:, :) ! dimension x dimension
        end subroutine abst_jacobian

        pure subroutine abst_jacobian_det(self, r, node_coords, connectivity, det_j)
            import :: abst_fe, type_coordinate_dp, int32, real64
            class(abst_fe), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :)
            integer(int32), intent(in) :: connectivity(:)
            real(real64), intent(inout) :: det_j
        end subroutine abst_jacobian_det

        subroutine abst_is_inside(self, cartesian, normalized, node_coords, connectivity, is_in)
            import abst_fe, type_coordinate_dp, int32, real64
            class(abst_fe), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: cartesian
            type(type_coordinate_dp), intent(inout) :: normalized
            real(real64), intent(in) :: node_coords(:, :)
            integer(int32), intent(in) :: connectivity(:)
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
    !> This unifies the geometric calculations required for matrix assembly.
    !>
    pure subroutine calc_shape_data(self, r, node_coords, connectivity, &
                                    psi_vec, dpsi_dx_mat, det_j)
        implicit none
        class(abst_fe), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        integer(int32), intent(in) :: connectivity(:) ! このルーチン内の計算では実質使いません

        !> Shape function values [num_nodes]
        real(real64), intent(inout) :: psi_vec(:)
        !> Global gradients of shape functions [num_nodes, dimension]
        real(real64), intent(inout) :: dpsi_dx_mat(:, :)
        !> Determinant of the Jacobian matrix
        real(real64), intent(inout) :: det_j

        integer(int32) :: i, j, dim, nn
        real(real64) :: jac(3, 3), inv_jac(3, 3)
        real(real64) :: dpsi_dxi(3) ! Local gradients

        dim = self%dimension
        nn = self%num_nodes

        ! 1. Evaluate Shape Functions
        do i = 1, nn
            call self%psi(i, r, psi_vec(i))
        end do

        ! 2. Compute Jacobian Matrix (Isoparametric formulation)
        jac = 0.0d0
        do i = 1, nn
            do j = 1, dim
                call self%dpsi(i, j, r, dpsi_dxi(j))

                ! [修正箇所]
                ! node_coords は (dim, num_nodes) のサイズで渡されており、
                ! 列の順番はローカルな節点番号(1..nn)に対応しています。
                ! connectivity(i) (グローバルID) を使うと範囲外参照になります。

                ! OLD (Error): node_coords(1, connectivity(i))
                ! NEW (Fixed): node_coords(1, i)

                jac(j, 1) = jac(j, 1) + dpsi_dxi(j) * node_coords(1, i)
                if (dim >= 2) jac(j, 2) = jac(j, 2) + dpsi_dxi(j) * node_coords(2, i)
                if (dim >= 3) jac(j, 3) = jac(j, 3) + dpsi_dxi(j) * node_coords(3, i)
            end do
        end do

        ! 3. Compute Determinant and Inverse Jacobian
        call calc_inverse_matrix(dim, jac(1:dim, 1:dim), det_j, inv_jac(1:dim, 1:dim))

        ! 4. Compute Global Gradients: nabla_x psi = J^(-T) * nabla_xi psi
        dpsi_dx_mat = 0.0d0
        do i = 1, nn
            ! Get local gradients again
            do j = 1, dim
                call self%dpsi(i, j, r, dpsi_dxi(j))
            end do

            ! Transform to global coordinates
            if (dim == 1) then
                dpsi_dx_mat(i, 1) = dpsi_dxi(1) * inv_jac(1, 1)
            else if (dim == 2) then
                dpsi_dx_mat(i, 1) = dpsi_dxi(1) * inv_jac(1, 1) + dpsi_dxi(2) * inv_jac(2, 1)
                dpsi_dx_mat(i, 2) = dpsi_dxi(1) * inv_jac(1, 2) + dpsi_dxi(2) * inv_jac(2, 2)
            else if (dim == 3) then
                dpsi_dx_mat(i, 1) = dot_product(dpsi_dxi, inv_jac(:, 1))
                dpsi_dx_mat(i, 2) = dot_product(dpsi_dxi, inv_jac(:, 2))
                dpsi_dx_mat(i, 3) = dot_product(dpsi_dxi, inv_jac(:, 3))
            end if
        end do

    end subroutine calc_shape_data
    !>
    !> Helper to calculate determinant and inverse of a small matrix (1x1 to 3x3).
    !>
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

    pure elemental subroutine get_type(self, val)
        implicit none
        class(abst_fe), intent(in) :: self
        integer(int32), intent(inout) :: val
        val = self%type
    end subroutine get_type

    pure elemental subroutine get_num_nodes(self, val)
        implicit none
        class(abst_fe), intent(in) :: self
        integer(int32), intent(inout) :: val
        val = self%num_nodes
    end subroutine get_num_nodes

    pure elemental subroutine get_dimension(self, val)
        implicit none
        class(abst_fe), intent(in) :: self
        integer(int32), intent(inout) :: val
        val = self%dimension
    end subroutine get_dimension

    pure elemental subroutine get_order(self, val)
        implicit none
        class(abst_fe), intent(in) :: self
        integer(int32), intent(inout) :: val
        val = self%order
    end subroutine get_order

    pure elemental subroutine get_num_gauss(self, val)
        implicit none
        class(abst_fe), intent(in) :: self
        integer(int32), intent(inout) :: val
        val = self%num_gauss
    end subroutine get_num_gauss

    pure subroutine get_weight(self, val)
        implicit none
        class(abst_fe), intent(in) :: self
        real(real64), intent(inout), allocatable :: val(:)
        val = self%weight
    end subroutine get_weight

    pure subroutine get_gauss(self, val)
        implicit none
        class(abst_fe), intent(in) :: self
        type(type_coordinate_dp), intent(inout), allocatable :: val(:)

        val = self%gauss
    end subroutine get_gauss

    pure subroutine lerp(self, r, global_values, connectivity, val)
        implicit none
        class(abst_fe), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: global_values(:)
        integer(int32), intent(in) :: connectivity(:)
        real(real64), intent(inout) :: val
        integer(int32) :: i
        real(real64) :: psi_i

        val = 0.0d0
        do i = 1, self%num_nodes
            call self%psi(i, r, psi_i)
            val = val + psi_i * global_values(connectivity(i))
        end do
    end subroutine lerp

    pure subroutine dlerp(self, r, global_values, connectivity, val)
        implicit none
        class(abst_fe), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: global_values(:)
        integer(int32), intent(in) :: connectivity(:)
        type(type_coordinate_dp), intent(inout) :: val
        integer(int32) :: i
        real(real64) :: dpsi_i

        val%x = 0.0d0; val%y = 0.0d0; val%z = 0.0d0
        do i = 1, self%num_nodes
            if (self%dimension >= 1) then
                call self%dpsi(i, 1, r, dpsi_i)
                val%x = val%x + dpsi_i * global_values(connectivity(i))
            end if
            if (self%dimension >= 2) then
                call self%dpsi(i, 2, r, dpsi_i)
                val%y = val%y + dpsi_i * global_values(connectivity(i))
            end if
            if (self%dimension >= 3) then
                call self%dpsi(i, 3, r, dpsi_i)
                val%z = val%z + dpsi_i * global_values(connectivity(i))
            end if
        end do
    end subroutine dlerp

    !>
    !> 1. 容量型行列 (Capacity Matrix) K1
    !>    K_ij = Integ { A(x) * psi_i * psi_j } dOmega
    !>    分布係数 A はスカラー (節点値 A_vec から補間)
    !>
    subroutine compute_K1_capacity(self, nodes, conn, A_vec, elem_mat)
        implicit none
        class(abst_fe), intent(in) :: self
        real(real64), intent(in) :: nodes(:, :) ! 全節点座標
        integer(int32), intent(in) :: conn(:) ! コネクティビティ
        real(real64), intent(in) :: A_vec(:) ! [num_nodes] 係数
        real(real64), intent(inout) :: elem_mat(:, :)

        integer(int32) :: p, i, j, nd, dim, num_gauss
        real(real64) :: w, det_J, A_val
        real(real64), allocatable :: psi(:), dpsi_dx(:, :)
        type(type_coordinate_dp) :: r

        call self%get_dimension(dim)
        call self%get_num_nodes(nd)
        call self%get_num_gauss(num_gauss)

        allocate (psi(nd), dpsi_dx(nd, dim))
        elem_mat = 0.0d0

        do p = 1, num_gauss
            r = self%gauss(p)
            w = self%weight(p)

            ! 形状関数データ一括取得 (psi, nabla psi, detJ)
            call self%calc_shape_data(r, nodes, conn, psi, dpsi_dx, det_J)

            ! 係数 A の補間: A(r) = Sum psi_i * Ai
            A_val = dot_product(psi, A_vec)

            ! 行列積算: w * detJ * A * psi_i * psi_j
            do j = 1, nd
                do i = 1, nd
                    elem_mat(i, j) = elem_mat(i, j) + w * det_J * A_val * psi(i) * psi(j)
                end do
            end do
        end do
    end subroutine compute_K1_capacity

    !>
    !> 2. 拡散型行列 (Diffusion Matrix) K2
    !>    K_ij = Integ { nabla psi_i . (M(x) * nabla psi_j) } dOmega
    !>    分布係数 M はテンソル (節点値 M_vec から補間)
    !>
    subroutine compute_K2_diffusion(self, nodes, conn, M_vec, elem_mat)
        implicit none
        class(abst_fe), intent(in) :: self
        real(real64), intent(in) :: nodes(:, :)
        integer(int32), intent(in) :: conn(:)
        real(real64), intent(in) :: M_vec(:, :, :) ! [dim, dim, num_nodes]
        real(real64), intent(inout) :: elem_mat(:, :)

        integer(int32) :: p, i, j, k, nd, dim, num_gauss
        real(real64) :: w, det_J
        real(real64), allocatable :: psi(:), dpsi_dx(:, :)
        real(real64), allocatable :: M_val(:, :), M_grad_psi_j(:)
        type(type_coordinate_dp) :: r

        call self%get_dimension(dim)
        call self%get_num_nodes(nd)
        call self%get_num_gauss(num_gauss)

        allocate (psi(nd), dpsi_dx(nd, dim))
        allocate (M_val(dim, dim), M_grad_psi_j(dim))

        elem_mat = 0.0d0

        do p = 1, num_gauss
            r = self%gauss(p)
            w = self%weight(p)
            call self%calc_shape_data(r, nodes, conn, psi, dpsi_dx, det_J)

            ! 係数 M の補間: M(r) = Sum psi_k * M_k
            M_val = 0.0d0
            do k = 1, nd
                M_val = M_val + psi(k) * M_vec(:, :, k)
            end do

            ! 行列積算: w * detJ * (nabla psi_i)^T * M * nabla psi_j
            do j = 1, nd
                ! M * nabla psi_j を先に計算
                ! dpsi_dx(j, :) は nabla psi_j (ベクトル)
                M_grad_psi_j = matmul(M_val, dpsi_dx(j, :)) ! (dim)

                do i = 1, nd
                    ! nabla psi_i . (M * nabla psi_j)
                    elem_mat(i, j) = elem_mat(i, j) + &
                                     w * det_J * dot_product(dpsi_dx(i, :), M_grad_psi_j)
                end do
            end do
        end do
    end subroutine compute_K2_diffusion

    !>
    !> 3. 混合型行列 (Mixed Matrix) K3
    !>    K_ij = Integ { (nabla psi_i . V(x)) * psi_j } dOmega
    !>    (行i: 勾配，列j: 形状関数)
    !>    分布係数 V はベクトル (節点値 V_vec から補間)
    !>
    subroutine compute_K3_mixed(self, nodes, conn, V_vec, elem_mat)
        implicit none
        class(abst_fe), intent(in) :: self
        real(real64), intent(in) :: nodes(:, :)
        integer(int32), intent(in) :: conn(:)
        real(real64), intent(in) :: V_vec(:, :) ! [dim, num_nodes]
        real(real64), intent(inout) :: elem_mat(:, :)

        integer(int32) :: p, i, j, k, nd, dim, num_gauss
        real(real64) :: w, det_J, grad_psi_i_dot_V
        real(real64), allocatable :: psi(:), dpsi_dx(:, :)
        real(real64), allocatable :: V_val(:)
        type(type_coordinate_dp) :: r

        call self%get_dimension(dim)
        call self%get_num_nodes(nd)
        call self%get_num_gauss(num_gauss)

        allocate (psi(nd), dpsi_dx(nd, dim))
        allocate (V_val(dim))

        elem_mat = 0.0d0

        do p = 1, num_gauss
            r = self%gauss(p)
            w = self%weight(p)
            call self%calc_shape_data(r, nodes, conn, psi, dpsi_dx, det_J)

            ! 係数 V の補間: V(r) = Sum psi_k * Vk
            V_val = 0.0d0
            do k = 1, nd
                V_val = V_val + psi(k) * V_vec(:, k)
            end do

            ! 行列積算: w * detJ * (nabla psi_i . V) * psi_j
            do i = 1, nd
                ! (nabla psi_i . V)
                grad_psi_i_dot_V = dot_product(dpsi_dx(i, :), V_val)

                do j = 1, nd
                    elem_mat(i, j) = elem_mat(i, j) + &
                                     w * det_J * grad_psi_i_dot_V * psi(j)
                end do
            end do
        end do
    end subroutine compute_K3_mixed

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
