submodule(domain_mesh_side) domain_mesh_side_first
    implicit none
contains

    module function construct_side_first(id, global_coordinate, input) result(side)
        implicit none
        integer(int32), intent(in) :: id
        type(type_dp_3d), pointer, intent(in) :: global_coordinate
        type(type_input), intent(in) :: input
        class(abst_side), allocatable :: side

        integer(int32) :: num_nodes, num_gauss
        real(real64), allocatable :: weight(:)
        real(real64), allocatable :: gauss(:, :)

        allocate (type_side_first :: side)

        num_nodes = input%geometry%vtk%cells(id)%num_nodes_in_cell

        select case (input%basic%geometry_settings%integration_type)
        case ("full")
            num_gauss = 1_int32
            call allocate_array(weight, num_gauss)
            call allocate_array(gauss, num_gauss, 3_int32)

            weight(:) = [0.0d0]
            gauss(:, 1) = [2.0d0, 0.0d0, 0.0d0]
        case ("reduced")
            call global_logger%log_warning(message="Reduced-type integration is not implemented for first order sides.")
            num_gauss = 1_int32
            call allocate_array(weight, num_gauss)
            call allocate_array(gauss, num_gauss, 3_int32)

            weight(:) = [0.0d0]
            gauss(:, 1) = [2.0d0, 0.0d0, 0.0d0]
        case ("free")
            call global_logger%log_warning(message="Free-type integration is not implemented for first order sides.")
            num_gauss = 1_int32
            call allocate_array(weight, num_gauss)
            call allocate_array(gauss, num_gauss, 3_int32)

            weight(:) = [0.0d0]
            gauss(:, 1) = [2.0d0, 0.0d0, 0.0d0]
        end select

        call side%initialize(id=id, &
                             type=input%geometry%vtk%cells(id)%cell_type, &
                             group=input%geometry%vtk%cells(id)%cell_entity_id, &
                             dimension=input%geometry%vtk%cells(id)%get_dimension(), &
                             order=input%geometry%vtk%cells(id)%get_order(), &
                             num_nodes=num_nodes, &
                             connectivity=input%geometry%vtk%cells(id)%connectivity(1:num_nodes), &
                             num_gauss=num_gauss, &
                             weight=weight, &
                             gauss=gauss, &
                             global_coordinate=global_coordinate)

    end function construct_side_first

    module pure function get_length_side_first(self) result(length)
        implicit none
        class(type_side_first), intent(in) :: self
        real(real64) :: length

        type(type_dp_vector_3d) :: coord1
        type(type_dp_vector_3d) :: coord2
        type(type_dp_vector_3d) :: delta

        ! オブジェクト自身が持つ get_coordinate メソッドを使用して節点座標を取得
        coord1 = self%get_coordinate(1)
        coord2 = self%get_coordinate(2)

        ! 2点間のベクトルを計算
        delta = coord2 - coord1

        ! ユークリッド距離（ベクトルの大きさ）を計算
        length = sqrt(delta%x**2 + delta%y**2 + delta%z**2)

    end function get_length_side_first
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

    module pure elemental function dpsi_side_first(self, i, j, r) result(dpsi)
        implicit none
        class(type_side_first), intent(in) :: self
        type(type_dp_vector_3d), intent(in) :: r
        integer(int32), intent(in) :: i
        integer(int32), intent(in) :: j
        real(real64) :: dpsi

        select case (j)
        case (1)
            select case (i)
            case (1)
                dpsi = -0.5d0
            case (2)
                dpsi = 0.5d0
            case default
                dpsi = 0.0d0
            end select
        end select
    end function dpsi_side_first

    pure elemental module function jacobian_side_first(self, i, j, r) result(jacobian)
        implicit none
        class(type_side_first), intent(in) :: self
        integer(int32), intent(in) :: i
        integer(int32), intent(in) :: j
        type(type_dp_vector_3d), intent(in) :: r
        real(real64) :: jacobian

        type(type_dp_vector_3d) :: coord1
        type(type_dp_vector_3d) :: coord2
        type(type_dp_vector_3d) :: delta

        jacobian = 0.0d0
        select case (j)
        case (1)
            coord1 = self%get_coordinate(1)
            coord2 = self%get_coordinate(2)
            delta = coord2 - coord1

            ! J = 0.5 * (x2 - x1)
            select case (i)
            case (1) ! x-component
                jacobian = 0.5d0 * delta%x
            case (2) ! y-component
                jacobian = 0.5d0 * delta%y
            case (3) ! z-component
                jacobian = 0.5d0 * delta%z
            end select
        end select

    end function jacobian_side_first

    pure elemental module function jacobian_det_side_first(self, r) result(jacobian_det)
        implicit none
        class(type_side_first), intent(in) :: self
        type(type_dp_vector_3d), intent(in) :: r
        real(real64) :: jacobian_det

        jacobian_det = self%get_geometry() / 2.0d0

    end function jacobian_det_side_first

end submodule domain_mesh_side_First
