module io_input_geometry
    use, intrinsic :: iso_fortran_env
!$  use :: omp_lib
    use :: mpi_f08
    use :: stdlib_strings, only:to_string, strip, ends_with
    use :: stdlib_logger
    use :: json_module, only:json_file
    use :: module_core
    use :: io_input_base, only:get_json_value, abst_input
    implicit none
    private

    public :: type_input_geometry

    character(*), parameter :: thermal = "thermal"
    character(*), parameter :: calculate_thermal = "calculate_thermal"
    character(*), parameter :: hydraulic = "hydraulic"
    character(*), parameter :: calculate_hydraulic = "calculate_hydraulic"
    character(*), parameter :: mechanical = "mechanical"
    character(*), parameter :: calculate_mechanical = "calculate_mechanical"

    type :: type_input_geometry
        class(abst_input), pointer :: parent => null()
        type(type_vtk) :: vtk
        character(:), allocatable :: point_data_names(:)
    contains
        procedure, pass(self), public :: initialize => initialize_type_input_geometry
        procedure, pass(self), private :: collect_fields_from_conditions
    end type type_input_geometry

    interface
        module subroutine initialize_type_input_geometry(self)
            implicit none
            class(type_input_geometry), intent(inout) :: self
        end subroutine initialize_type_input_geometry

        module function collect_fields_from_conditions(self) result(field_list)
            implicit none
            class(type_input_geometry), intent(inout) :: self
            character(:), allocatable :: field_list(:)
        end function collect_fields_from_conditions

        ! module subroutine display_input_geometry(self)
        !     implicit none
        !     class(type_input_geometry), intent(in) :: self
        ! end subroutine display_input_geometry
    end interface

end module io_input_geometry
