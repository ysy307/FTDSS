module domain_mesh_side_factory
    use, intrinsic :: iso_fortran_env, only: int32
    use :: module_core, only:type_dp_3d
    use :: module_input, only:type_input
    use :: domain_mesh_side, only:abst_side, type_side_first, type_side_second

    implicit none
    public :: create_side

contains
    function create_side(id, global_coordinate, input) result(new_side)
        implicit none
        integer(int32), intent(in) :: id
        type(type_dp_3d), pointer, intent(in) :: global_coordinate
        type(type_input), intent(in) :: input
        class(abst_side), allocatable :: new_side

        character(:), allocatable :: type_name

        type_name = input%geometry%vtk%cells(id)%cell_type_name

        select case (type_name)
        case ("Line")
            new_side = type_side_first(id, global_coordinate, input)
        case ("QuadraticEdge")
            new_side = type_side_second(id, global_coordinate, input)
        case default
            write (*, '(a)') "Error: Unknown side shape type = "//type_name
        end select

    end function create_side

end module domain_mesh_side_factory
