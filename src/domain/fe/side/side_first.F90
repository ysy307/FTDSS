submodule(domain_fe_side) domain_fe_side_first
    implicit none
contains

    !----------------------------------------------------------------------
    ! CONSTRUCTOR: 2節点1次線要素のオブジェクトを生成
    !----------------------------------------------------------------------
    module function construct_side_first(input) result(fe)
        implicit none
        type(type_input), intent(in) :: input
        class(abst_fe), allocatable :: fe

        character(len=32), parameter :: cell_name = "Line"
        integer(int32) :: vtk_type, num_nodes, dimension, order, num_gauss
        real(real64), allocatable :: weight(:)
        real(real64), allocatable :: gauss(:, :)

        allocate (type_side_first :: fe)

        call vtk_constants%get_cell_info_from_cell_name(cell_name, vtk_type, num_nodes, dimension, order)

        ! 1次要素の積分則 (1点ガウス積分)
        num_gauss = 1
        call allocate_array(weight, num_gauss)
        call allocate_array(gauss, 3, num_gauss)

        weight(1) = 2.0d0
        gauss(:, 1) = 0.0d0

        call fe%initialize(type=vtk_type, dimension=dimension, order=order, num_nodes=num_nodes, &
                           num_gauss=num_gauss, weight=weight, gauss=gauss)

        call deallocate_array(weight)
        call deallocate_array(gauss)
    end function construct_side_first

    !----------------------------------------------------------------------
    ! GET_LENGTH: 要素の長さを計算
    !----------------------------------------------------------------------
    module function get_length_side_first(self, node_coords, connectivity) result(length)
        implicit none
        class(type_side_first), intent(in) :: self
        real(real64), intent(in) :: node_coords(:, :)
        integer(int32), intent(in) :: connectivity(:)
        real(real64) :: length
        integer(int32) :: node1_id, node2_id
        real(real64) :: dx, dy, dz

        node1_id = connectivity(1)
        node2_id = connectivity(2)

        dx = node_coords(1, node2_id) - node_coords(1, node1_id)
        dy = node_coords(2, node2_id) - node_coords(2, node1_id)
        dz = node_coords(3, node2_id) - node_coords(3, node1_id)

        length = sqrt(dx**2 + dy**2 + dz**2)
    end function get_length_side_first

    !----------------------------------------------------------------------
    ! PSI: 形状関数
    !----------------------------------------------------------------------
    module pure elemental function psi_side_first(self, i, r) result(psi)
        implicit none
        class(type_side_first), intent(in) :: self
        integer(int32), intent(in) :: i
        type(type_dp_vector_3d), intent(in) :: r
        real(real64) :: psi

        select case (i)
        case (1)
            psi = 0.5d0 * (1.0d0 - r%x)
        case (2)
            psi = 0.5d0 * (1.0d0 + r%x)
        case default
            psi = 0.0d0
        end select
    end function psi_side_first

    !----------------------------------------------------------------------
    ! DPSI: 形状関数の微分
    !----------------------------------------------------------------------
    module pure elemental function dpsi_side_first(self, i, j, r) result(dpsi)
        implicit none
        class(type_side_first), intent(in) :: self
        integer(int32), intent(in) :: i
        integer(int32), intent(in) :: j
        type(type_dp_vector_3d), intent(in) :: r
        real(real64) :: dpsi

        dpsi = 0.0d0
        if (j == 1) then
            select case (i)
            case (1)
                dpsi = -0.5d0
            case (2)
                dpsi = 0.5d0
            end select
        end if
    end function dpsi_side_first

    !----------------------------------------------------------------------
    ! PRIVATE METHOD IMPLEMENTATION: 接線ベクトルを計算 (MODIFIED)
    !----------------------------------------------------------------------
    module pure function compute_tangent_vector_side_first(self, r, node_coords, connectivity) result(tangent_vec)
        implicit none
        class(type_side_first), intent(in) :: self
        type(type_dp_vector_3d), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        integer(int32), intent(in) :: connectivity(:)
        real(real64) :: tangent_vec(3)
        integer(int32) :: i, node_id

        ! ご指示のあった汎用的な計算方法に変更
        tangent_vec = 0.0d0
        do i = 1, self%get_num_nodes()
            node_id = connectivity(i)
            tangent_vec(1) = tangent_vec(1) + self%dpsi(i, 1, r) * node_coords(1, node_id)
            tangent_vec(2) = tangent_vec(2) + self%dpsi(i, 1, r) * node_coords(2, node_id)
            tangent_vec(3) = tangent_vec(3) + self%dpsi(i, 1, r) * node_coords(3, node_id)
        end do
    end function compute_tangent_vector_side_first

    !----------------------------------------------------------------------
    ! JACOBIAN: ヤコビ行列を計算
    !----------------------------------------------------------------------
    module pure function jacobian_side_first(self, r, node_coords, connectivity) result(jacobian)
        implicit none
        class(type_side_first), intent(in) :: self
        type(type_dp_vector_3d), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        integer(int32), intent(in) :: connectivity(:)
        real(real64) :: jacobian(self%get_dimension(), self%get_dimension())
        real(real64) :: tangent_vec(3)

        tangent_vec = self%compute_tangent_vector(r, node_coords, connectivity)

        jacobian(1, 1) = sqrt(sum(tangent_vec**2))
    end function jacobian_side_first

    !----------------------------------------------------------------------
    ! JACOBIAN_DET: ヤコビ行列式を計算
    !----------------------------------------------------------------------
    module pure function jacobian_det_side_first(self, r, node_coords, connectivity) result(jacobian_det)
        implicit none
        class(type_side_first), intent(in) :: self
        type(type_dp_vector_3d), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        integer(int32), intent(in) :: connectivity(:)
        real(real64) :: jacobian_det
        real(real64) :: jacobian(self%get_dimension(), self%get_dimension())

        jacobian = self%jacobian(r, node_coords, connectivity)
        jacobian_det = jacobian(1, 1)
    end function jacobian_det_side_first

    !----------------------------------------------------------------------
    ! IS_IN: 点が要素内部にあるか判定
    !----------------------------------------------------------------------
    module subroutine is_in_side_first(self, cartesian, normalized, node_coords, connectivity, is_in)
        implicit none
        class(type_side_first), intent(in) :: self
        type(type_dp_vector_3d), intent(in) :: cartesian
        type(type_dp_vector_3d), intent(inout) :: normalized
        real(real64), intent(in) :: node_coords(:, :)
        integer(int32), intent(in) :: connectivity(:)
        logical, intent(inout) :: is_in

        real(real64) :: v(3), w(3)
        real(real64) :: t, v_dot_v
        integer(int32) :: node1_id, node2_id
        real(real64), parameter :: tol = 1.0e-9

        node1_id = connectivity(1)
        node2_id = connectivity(2)

        v(1) = node_coords(1, node2_id) - node_coords(1, node1_id)
        v(2) = node_coords(2, node2_id) - node_coords(2, node1_id)
        v(3) = node_coords(3, node2_id) - node_coords(3, node1_id)

        w(1) = cartesian%x - node_coords(1, node1_id)
        w(2) = cartesian%y - node_coords(2, node1_id)
        w(3) = cartesian%z - node_coords(3, node1_id)

        v_dot_v = v(1)**2 + v(2)**2 + v(3)**2

        if (v_dot_v < tol**2) then
            is_in = (abs(w(1)) < tol .and. abs(w(2)) < tol .and. abs(w(3)) < tol)
        else
            t = (w(1) * v(1) + w(2) * v(2) + w(3) * v(3)) / v_dot_v
            is_in = (t >= 0.0d0 - tol .and. t <= 1.0d0 + tol)
        end if

        if (is_in) then
            call normalized%set(2.0d0 * t - 1.0d0, 0.0d0, 0.0d0)
        end if
    end subroutine is_in_side_first

end submodule domain_fe_side_first
