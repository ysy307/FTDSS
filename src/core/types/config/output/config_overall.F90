module types_config_overall
    use, intrinsic :: iso_fortran_env
    use :: core_constants
    use :: types_config_base, only:abst_config
    use :: types_geometry_coordinate_array, only:type_coordinate_array_dp
    implicit none

    public :: type_config_overall

    type, extends(abst_config) :: type_config_overall
        type(type_constant_id) :: file_format
        type(type_constant_id), allocatable :: output_variables(:)
        character(:), allocatable :: format_output_file

        integer(int32) :: num_points
        integer(int32) :: num_cells
        type(type_coordinate_array_dp) :: coordinate
        integer(int32), allocatable :: connectivities(:)
        integer(int32), allocatable :: offsets(:)
        integer(int8), allocatable :: cell_types(:)
    contains
        procedure, public, pass(self) :: copy => copy_config_overall_geometry
        procedure, public, pass(self) :: reset => reset_config_overall_geometry
    end type type_config_overall

contains

    subroutine copy_config_overall_geometry(self, source)
        implicit none
        class(type_config_overall), intent(inout) :: self
        class(abst_config), intent(in) :: source

        select type (source)
        type is (type_config_overall)
            call self%set(self%file_format, source%file_format)
            call self%set(self%output_variables, source%output_variables)
            call self%set(self%format_output_file, source%format_output_file)

            call self%set(self%num_points, source%num_points)
            call self%set(self%num_cells, source%num_cells)
            call self%set(self%coordinate, source%coordinate)
            call self%set(self%connectivities, source%connectivities)
            call self%set(self%offsets, source%offsets)
            call self%set(self%cell_types, source%cell_types)
        class default
            call self%reset()
        end select
    end subroutine copy_config_overall_geometry

    subroutine reset_config_overall_geometry(self)
        implicit none
        class(type_config_overall), intent(inout) :: self

        self%file_format = FILE_FORMATS%NONE
        call deallocate_array(self%output_variables)
        self%format_output_file = ""

        self%num_points = 0
        self%num_cells = 0
        call self%coordinate%destroy()
        call deallocate_array(self%connectivities)
        call deallocate_array(self%offsets)
        call deallocate_array(self%cell_types)

    end subroutine reset_config_overall_geometry

end module types_config_overall
