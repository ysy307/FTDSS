submodule(domain_side) domain_side_first
    implicit none
contains

    module function construct_side_first(id, global_coordinate, cell_info) result(side)
        implicit none
        integer(int32), intent(in) :: id
        type(type_dp_3d), pointer, intent(in) :: global_coordinate
        type(type_vtk_cell), intent(in) :: cell_info
        class(abst_side), allocatable :: side

        integer(int32) :: i

        if (allocated(side)) deallocate (side)
        allocate (type_side_first :: side)

        side%id = id
        side%type = cell_info%cell_type
        side%group = cell_info%cell_entity_id
        side%dimension = cell_info%get_dimension()
        side%order = cell_info%get_order()

        side%num_nodes = cell_info%num_nodes_in_cell
        allocate (side%connectivity(side%num_nodes))
        side%connectivity(:) = cell_info%connectivity(1:side%num_nodes)

        allocate (side%x(side%num_nodes))
        allocate (side%y(side%num_nodes))
        allocate (side%z(side%num_nodes))
        do i = 1, side%num_nodes
            nullify (side%x(i)%val)
            nullify (side%y(i)%val)
            nullify (side%z(i)%val)
            side%x(i)%val => global_coordinate%x(side%connectivity(i))
            side%y(i)%val => global_coordinate%y(side%connectivity(i))
            side%z(i)%val => global_coordinate%z(side%connectivity(i))
        end do

        side%num_Gauss = 1_int32
        call allocate_array(side%weight, side%num_Gauss)
        call allocate_array(side%gauss, side%num_Gauss)
        side%weight(:) = [0.0d0]
        side%gauss(:) = [2.0d0]
    end function construct_side_first

    module function get_id_side_first(self) result(id)
        implicit none
        class(type_side_first), intent(in) :: self
        integer(int32) :: id

        id = self%id
    end function get_id_side_first

    module function get_type_side_first(self) result(type)
        implicit none
        class(type_side_first), intent(in) :: self
        integer(int32) :: type

        type = self%type
    end function get_type_side_first

    module function get_num_nodes_side_first(self) result(num_nodes)
        implicit none
        class(type_side_first), intent(in) :: self
        integer(int32) :: num_nodes

        num_nodes = self%num_nodes
    end function get_num_nodes_side_first

    module function get_group_side_first(self) result(group)
        implicit none
        class(type_side_first), intent(in) :: self
        integer(int32) :: group

        group = self%group
    end function get_group_side_first

    module function get_order_side_first(self) result(order)
        implicit none
        class(type_side_first), intent(in) :: self
        integer(int32) :: order

        order = self%order
    end function get_order_side_first

    module function get_dimension_side_first(self) result(dimension)
        implicit none
        class(type_side_first), intent(in) :: self
        integer(int32) :: dimension

        dimension = self%dimension
    end function get_dimension_side_first

    module function get_num_gauss_side_first(self) result(num_gauss)
        implicit none
        class(type_side_first), intent(in) :: self
        integer(int32) :: num_gauss

        num_gauss = self%num_Gauss
    end function get_num_gauss_side_first

    module function psi_side_first(self, i, xi) result(psi)
        implicit none
        class(type_side_first), intent(in) :: self
        integer(int32), intent(in) :: i
        real(real64), intent(in) :: xi
        real(real64) :: psi

        select case (i)
        case (1)
            psi = 0.5d0 * (1.0d0 - xi)
        case (2)
            psi = 0.5d0 * (1.0d0 + xi)
        case default
            psi = 0.0d0
        end select
    end function psi_side_first

    module function dpsi_dxi_side_first(self, i) result(dpsi)
        implicit none
        class(type_side_first), intent(in) :: self
        integer(int32), intent(in) :: i
        real(real64) :: dpsi

        select case (i)
        case (1)
            dpsi = -0.5d0
        case (2)
            dpsi = 0.5d0
        case default
            dpsi = 0.0d0
        end select
    end function dpsi_dxi_side_first

end submodule domain_side_First
