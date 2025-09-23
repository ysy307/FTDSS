module domain_mesh_element_factory
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core
    use :: module_input, only:type_input
    use :: domain_mesh_element, only:abst_element, & !&
                                type_triangle_first, type_triangle_second, & !&
                                type_square_first, type_square_second !&
    implicit none
    private

    public :: create_element

contains
    function create_element(mesh_id, input) result(new_element)
        implicit none
        integer(int32), intent(in) :: mesh_id
        type(type_input), intent(in) :: input
        class(abst_element), allocatable :: new_element

        character(:), allocatable :: cell_name

        cell_name = vtk_constants%get_cell_name(mesh_id)

        select case (cell_name)
        case ("Triangle")
            new_element = type_triangle_first(input)
        case ("Quad")
            new_element = type_square_first(input)
        case ("QuadraticTriangle")
            new_element = type_triangle_second(input)
        case ("QuadraticQuad")
            new_element = type_square_second(input)
        end select

    end function create_element

end module domain_mesh_element_factory
