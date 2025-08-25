module domain_side_factory
    use, intrinsic :: iso_fortran_env, only: int32
    use :: module_core, only:type_dp_3d, type_vtk_cell
    use :: module_input, only:type_geometry_settings
    use :: domain_side, only:abst_side, type_side_first, type_side_second

    implicit none

    public :: create_side

contains
    ! 要素オブジェクトを生成して返す、独立したファクトリサブルーチン
    subroutine create_side(new_side, id, global_coordinate, cell_info, integration, ierr)
        implicit none
        class(abst_side), allocatable, intent(inout) :: new_side
        integer(int32), intent(in) :: id
        type(type_dp_3d), pointer, intent(in) :: global_coordinate
        type(type_vtk_cell), intent(in) :: cell_info
        type(type_geometry_settings), intent(in) :: integration
        integer(int32), intent(inout) :: ierr

        character(:), allocatable :: type_name

        ierr = 0
        if (allocated(new_side)) deallocate (new_side)

        type_name = cell_info%cell_type_name

        select case (type_name)
        case ("Line")
            new_side = type_side_first(id, global_coordinate, cell_info, integration)
        case ("QuadraticEdge")
            new_side = type_side_second(id, global_coordinate, cell_info, integration)
        case default
            write (*, '(a)') "Error: Unknown side shape type = "//type_name
            ierr = -1
        end select

    end subroutine create_side

end module domain_side_factory
