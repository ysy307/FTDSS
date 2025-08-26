module domain_mesh_element_factory
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core, only:type_dp_3d
    use :: module_input, only:type_input
    use :: domain_mesh_element, only:abst_element, & !&
                                type_triangle_first, type_triangle_second, & !&
                                type_square_first, type_square_second !&
    implicit none
    private

    public :: create_element

contains
    function create_element(id, global_coordinate, input) result(new_element)
        implicit none
        integer(int32), intent(in) :: id
        type(type_dp_3d), pointer, intent(in) :: global_coordinate
        type(type_input), intent(in) :: input
        class(abst_element), allocatable :: new_element

        character(:), allocatable :: type_name

        type_name = input%geometry%vtk%cells(id)%cell_type_name

        select case (type_name)
        case ("Triangle")
            new_element = type_triangle_first(id, global_coordinate, input)
        case ("Quad")
            new_element = type_square_first(id, global_coordinate, input)
        case ("QuadraticTriangle")
            new_element = type_triangle_second(id, global_coordinate, input)
        case ("QuadraticQuad")
            new_element = type_square_second(id, global_coordinate, input)
        case default
            write (*, '(a)') "Error: Unknown side shape type = "//type_name
        end select

    end function create_element

end module domain_mesh_element_factory
