submodule(domain_fe_side) domain_fe_side_second
    implicit none

contains

    !----------------------------------------------------------------------
    ! CONSTRUCTOR: 3節点2次線要素の汎用計算オブジェクトを生成
    !----------------------------------------------------------------------
    module function construct_side_second(input) result(fe)
        implicit none
        type(type_input), intent(in) :: input
        class(abst_fe), allocatable :: fe

        character(len=32), parameter :: cell_name = "QuadraticEdge"
        integer(int32) :: vtk_type, num_nodes, dimension, order, num_gauss
        real(real64), allocatable :: weight(:), gauss(:, :)

        allocate (type_side_second :: fe)

        call vtk_constants%get_cell_info_from_cell_name(cell_name, vtk_type, num_nodes, dimension, order)

        ! ご指定のルールに基づき積分則を設定
        select case (strip(input%basic%geometry_settings%integration_type))
        case ("full")
            num_gauss = 2
            call allocate_array(weight, num_gauss)
            call allocate_array(gauss, 3, num_gauss)

            weight(:) = [1.0d0, 1.0d0]
            gauss(1, 1) = -1.0d0 / sqrt(3.0d0)
            gauss(1, 2) = 1.0d0 / sqrt(3.0d0)
            gauss(2:3, :) = 0.0d0
        case ("reduced")
            num_gauss = 1
            call allocate_array(weight, num_gauss)
            call allocate_array(gauss, 3, num_gauss)

            weight(1) = 2.0d0
            gauss(:, 1) = 0.0d0
        case ("free")
            num_gauss = 2
            call allocate_array(weight, num_gauss)
            call allocate_array(gauss, 3, num_gauss)

            weight(:) = [1.0d0, 1.0d0]
            gauss(1, 1) = -1.0d0 / sqrt(3.0d0)
            gauss(1, 2) = 1.0d0 / sqrt(3.0d0)
            gauss(2:3, :) = 0.0d0
        end select

        call fe%initialize(type=vtk_type, dimension=dimension, order=order, num_nodes=num_nodes, &
                           num_gauss=num_gauss, weight=weight, gauss=gauss)

        call deallocate_array(weight)
        call deallocate_array(gauss)

    end function construct_side_second

    !----------------------------------------------------------------------
    ! HELPER: 接線ベクトルを計算する補助関数
    !----------------------------------------------------------------------
    module pure function compute_tangent_vector_side_second(self, r, node_coords, connectivity) result(tangent_vec)
        implicit none
        class(type_side_second), intent(in) :: self
        type(type_dp_vector_3d), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        integer(int32), intent(in) :: connectivity(:)
        real(real64) :: tangent_vec(3)
        integer(int32) :: i, node_id

        tangent_vec = 0.0d0
        do i = 1, self%get_num_nodes()
            node_id = connectivity(i)
            tangent_vec(1) = tangent_vec(1) + self%dpsi(i, 1, r) * node_coords(1, node_id)
            tangent_vec(2) = tangent_vec(2) + self%dpsi(i, 1, r) * node_coords(2, node_id)
            tangent_vec(3) = tangent_vec(3) + self%dpsi(i, 1, r) * node_coords(3, node_id)
        end do
    end function compute_tangent_vector_side_second

    !----------------------------------------------------------------------
    ! GET_LENGTH: ガウス求積法による要素の曲線長を計算
    !----------------------------------------------------------------------
    module function get_length_side_second(self, node_coords, connectivity) result(length)
        implicit none
        class(type_side_second), intent(in) :: self
        real(real64), intent(in) :: node_coords(:, :)
        integer(int32), intent(in) :: connectivity(:)
        real(real64) :: length
        integer(int32) :: i
        type(type_dp_vector_3d), allocatable :: gauss_pts(:)
        real(real64), allocatable :: weights(:)

        length = 0.0d0
        gauss_pts = self%get_gauss()
        weights = self%get_weight()

        do i = 1, self%get_num_gauss()
            length = length + self%jacobian_det(gauss_pts(i), node_coords, connectivity) * weights(i)
        end do
    end function get_length_side_second

    !----------------------------------------------------------------------
    ! PSI: 3節点2次要素の形状関数
    !----------------------------------------------------------------------
    module pure elemental function psi_side_second(self, i, r) result(psi)
        implicit none
        class(type_side_second), intent(in) :: self
        integer(int32), intent(in) :: i
        type(type_dp_vector_3d), intent(in) :: r
        real(real64) :: psi
        real(real64) :: xi

        xi = r%x
        select case (i)
        case (1)
            psi = 0.5d0 * xi * (xi - 1.0d0)
        case (2)
            psi = 0.5d0 * xi * (xi + 1.0d0)
        case (3)
            psi = 1.0d0 - xi**2
        case default
            psi = 0.0d0
        end select
    end function psi_side_second

    !----------------------------------------------------------------------
    ! DPSI: 形状関数の自然座標に関する微分
    !----------------------------------------------------------------------
    module pure elemental function dpsi_side_second(self, i, j, r) result(dpsi)
        implicit none
        class(type_side_second), intent(in) :: self
        integer(int32), intent(in) :: i
        integer(int32), intent(in) :: j
        type(type_dp_vector_3d), intent(in) :: r
        real(real64) :: dpsi
        real(real64) :: xi

        dpsi = 0.0d0
        if (j == 1) then
            xi = r%x
            select case (i)
            case (1)
                dpsi = xi - 0.5d0
            case (2)
                dpsi = xi + 0.5d0
            case (3)
                dpsi = -2.0d0 * xi
            end select
        end if
    end function dpsi_side_second

    !----------------------------------------------------------------------
    ! JACOBIAN: ヤコビ行列を計算
    !----------------------------------------------------------------------
    module pure function jacobian_side_second(self, r, node_coords, connectivity) result(jacobian)
        implicit none
        class(type_side_second), intent(in) :: self
        type(type_dp_vector_3d), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        integer(int32), intent(in) :: connectivity(:)
        real(real64) :: jacobian(self%get_dimension(), self%get_dimension())
        real(real64) :: tangent_vec(3)

        ! 1次元要素のヤコビ行列(1x1)の成分は、接線ベクトルのノルム(大きさ)
        tangent_vec = compute_tangent_vector_side_second(self, r, node_coords, connectivity)
        jacobian(1, 1) = sqrt(sum(tangent_vec**2))
    end function jacobian_side_second

    !----------------------------------------------------------------------
    ! JACOBIAN_DET: ヤコビ行列式を計算
    !----------------------------------------------------------------------
    module pure function jacobian_det_side_second(self, r, node_coords, connectivity) result(jacobian_det)
        implicit none
        class(type_side_second), intent(in) :: self
        type(type_dp_vector_3d), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        integer(int32), intent(in) :: connectivity(:)
        real(real64) :: jacobian_det
        real(real64) :: jacobian_matrix(self%get_dimension(), self%get_dimension())

        ! ヤコビ行列式は、ヤコビ行列の(1,1)成分そのもの
        jacobian_matrix = self%jacobian(r, node_coords, connectivity)
        jacobian_det = jacobian_matrix(1, 1)
    end function jacobian_det_side_second

    !----------------------------------------------------------------------
    ! IS_IN: Newton-Raphson法で点が要素内部にあるか判定
    !----------------------------------------------------------------------
    module subroutine is_in_side_second(self, cartesian, normalized, node_coords, connectivity, is_in)
        implicit none
        class(type_side_second), intent(in) :: self
        type(type_dp_vector_3d), intent(in) :: cartesian
        type(type_dp_vector_3d), intent(inout) :: normalized
        real(real64), intent(in) :: node_coords(:, :)
        integer(int32), intent(in) :: connectivity(:)
        logical, intent(inout) :: is_in

        type(type_dp_vector_3d) :: r_local
        type(type_dp_vector_3d) :: pos_guess
        type(type_dp_vector_3d) :: residual_vec
        real(real64) :: tangent_vec(3)
        real(real64) :: tangent_dot_tangent
        real(real64) :: residual_norm
        real(real64) :: psi_val
        integer(int32) :: iter
        integer(int32) :: i
        integer(int32) :: node_id
        logical :: converged
        real(real64), parameter :: tol = 1.0e-9
        integer(int32), parameter :: max_iter = 10

        call r_local%set(0.0d0, 0.0d0, 0.0d0)
        converged = .false.

        do iter = 1, max_iter
            ! 1. 現在の自然座標から物理座標を計算 (MODIFIED: ベクトル演算を成分ごとに記述)
            call pos_guess%set(0.0d0, 0.0d0, 0.0d0)
            do i = 1, self%get_num_nodes()
                node_id = connectivity(i)
                psi_val = self%psi(i, r_local)
                pos_guess%x = pos_guess%x + psi_val * node_coords(1, node_id)
                pos_guess%y = pos_guess%y + psi_val * node_coords(2, node_id)
                pos_guess%z = pos_guess%z + psi_val * node_coords(3, node_id)
            end do

            ! 2. 目標座標との残差を計算
            residual_vec%x = cartesian%x - pos_guess%x
            residual_vec%y = cartesian%y - pos_guess%y
            residual_vec%z = cartesian%z - pos_guess%z

            ! ノルム計算を明示的に記述 (MODIFIED)
            residual_norm = sqrt(residual_vec%x**2 + residual_vec%y**2 + residual_vec%z**2)

            if (residual_norm < tol) then
                converged = .true.
                exit
            end if

            ! 3. 現在の自然座標における接線ベクトルを計算
            tangent_vec = compute_tangent_vector_side_second(self, r_local, node_coords, connectivity)
            tangent_dot_tangent = tangent_vec(1)**2 + tangent_vec(2)**2 + tangent_vec(3)**2

            if (tangent_dot_tangent < epsilon(tangent_dot_tangent)) then
                is_in = .false.
                return
            end if

            ! 4. Newton-Raphson法で更新 (MODIFIED: 内積計算を成分ごとに記述)
            r_local%x = r_local%x + (tangent_vec(1) * residual_vec%x + &
                                     tangent_vec(2) * residual_vec%y + &
                                     tangent_vec(3) * residual_vec%z) / tangent_dot_tangent
        end do

        ! 5. 収束後、自然座標が範囲内にあるか判定
        is_in = converged .and. (abs(r_local%x) <= 1.0d0 + tol)
        if (is_in) then
            normalized = r_local
        end if

    end subroutine is_in_side_second

end submodule domain_fe_side_second
