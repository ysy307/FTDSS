module domain_element_factory
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core, only:type_dp_3d, type_vtk_cell
    use :: domain_element, only:abst_element, & !&
                                type_triangle_first, type_triangle_second, & !&
                                type_square_first, type_square_second !&
    implicit none
    private

    public :: create_element

contains
    ! 要素オブジェクトを生成して返す、独立したファクトリサブルーチン
    subroutine create_element(new_element, id, global_coordinate, cell_info, ierr)
        implicit none
        class(abst_element), allocatable, intent(inout) :: new_element
        integer(int32), intent(in) :: id
        type(type_dp_3d), pointer, intent(in) :: global_coordinate
        type(type_vtk_cell), intent(in) :: cell_info
        integer(int32), intent(inout) :: ierr

        character(:), allocatable :: type_name

        ierr = 0

        if (allocated(new_element)) deallocate (new_element)

        type_name = cell_info%cell_type_name

        select case (type_name)
        case ("Triangle")
            new_element = type_triangle_first(id, global_coordinate, cell_info)
        case ("Quad")
            new_element = type_square_first(id, global_coordinate, cell_info)
        case ("QuadraticTriangle")
            new_element = type_triangle_second(id, global_coordinate, cell_info)
        case ("QuadraticQuad")
            new_element = type_square_second(id, global_coordinate, cell_info)
        case default
            write (*, '(a)') "Error: Unknown side shape type = "//type_name
            ierr = -1
        end select

    end subroutine create_element

end module domain_element_factory
