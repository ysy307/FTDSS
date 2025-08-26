submodule(domain_mesh_side) domain_mesh_side_second
    implicit none
contains

    module function construct_side_second(id, global_coordinate, input) result(side)
        implicit none
        integer(int32), intent(in) :: id
        type(type_dp_3d), pointer, intent(in) :: global_coordinate
        type(type_input), intent(in) :: input
        class(abst_side), allocatable :: side

        integer(int32) :: i
        integer(int32) :: num_nodes, num_gauss
        real(real64), allocatable :: weight(:)
        real(real64), allocatable :: gauss(:, :)

        allocate (type_side_second :: side)

        num_nodes = input%geometry%vtk%cells(id)%num_nodes_in_cell

        select case (input%basic%geometry_settings%integration_type)
        case ("full")
            num_gauss = 2_int32
            call allocate_array(weight, num_gauss)
            call allocate_array(gauss, num_gauss, 3_int32)

            weight(:) = [1.0d0, 1.0d0]
            gauss(:, 1) = [-sqrt(1.0d0 / 3.0d0), 0.0d0, 0.0d0]
            gauss(:, 2) = [sqrt(1.0d0 / 3.0d0), 0.0d0, 0.0d0]
        case ("reduced")
            num_gauss = 1_int32
            call allocate_array(weight, num_gauss)
            call allocate_array(gauss, num_gauss, 3_int32)

            weight(:) = [0.0d0]
            gauss(:, 1) = [2.0d0, 0.0d0, 0.0d0]
        case ("free")
            num_gauss = 2_int32
            call allocate_array(weight, num_gauss)
            call allocate_array(gauss, num_gauss, 3_int32)

            weight(:) = [1.0d0, 1.0d0]
            gauss(:, 1) = [-sqrt(1.0d0 / 3.0d0), 0.0d0, 0.0d0]
            gauss(:, 2) = [sqrt(1.0d0 / 3.0d0), 0.0d0, 0.0d0]
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

    end function construct_side_second

    module pure elemental function psi_side_second(self, i, r) result(psi)
        implicit none
        class(type_side_second), intent(in) :: self
        integer(int32), intent(in) :: i
        type(type_dp_vector_3d), intent(in) :: r
        real(real64) :: psi

        select case (i)
        case (1)
            psi = 0.5d0 * r%x * (1.0d0 - r%x)
        case (2)
            psi = 1.0d0 * r%x**2.0d0
        case (3)
            psi = 0.5d0 * r%x * (1.0d0 + r%x)
        case default
            psi = 0.0d0
        end select
    end function psi_side_second

    module pure elemental function dpsi_dxi_side_second(self, i, r) result(dpsi)
        implicit none
        class(type_side_second), intent(in) :: self
        integer(int32), intent(in) :: i
        type(type_dp_vector_3d), intent(in) :: r
        real(real64) :: dpsi

        select case (i)
        case (1)
            dpsi = 0.5d0 - r%x
        case (2)
            dpsi = 2.0d0 * r%x
        case (3)
            dpsi = 0.5d0 + r%x
        case default
            dpsi = 0.0d0
        end select
    end function dpsi_dxi_side_second

end submodule domain_mesh_side_Second
