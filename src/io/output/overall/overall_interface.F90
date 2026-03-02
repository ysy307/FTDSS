module io_output_overall
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding, only: c_int64_t, c_ptr, c_f_pointer, c_char, c_null_char, c_associated
    use :: omp_lib
    use :: stdlib_strings, only:to_string, strip
    use :: stdlib_io, only:open
    use :: vtk_fortran, only:vtk_file
    use :: module_core
    use :: module_input
    use :: module_control

    implicit none
    private

    public :: type_output_overall

    !---------------------------------------------------------------------------
    ! type_output_vtk / type_output_overall
    !---------------------------------------------------------------------------
    type :: type_output_vtk
        integer(int32) :: num_points
        integer(int32) :: num_cells
        type(type_coordinate_array_dp) :: coordinate
        integer(int32), allocatable :: connectivities(:)
        integer(int32), allocatable :: offsets(:)
        integer(int8), allocatable :: cell_types(:)
    end type

    type :: type_output_overall
        private
        character(:), allocatable :: dir_output_field
        character(:), allocatable :: format_output
        character(:), allocatable :: file_extension
        character(:), allocatable :: variable_names(:)
        logical :: do_output
        ! DATA
        type(type_output_vtk) :: vtk

        procedure(abst_output_overall_fields), public, pointer, pass(self) :: write_fields => null()
        procedure(abst_output_overall_cell), public, pointer, pass(self) :: write_cell => null()
    contains
        procedure, public, pass(self) :: initialize => initialize_input_type_output_overall
        procedure, private, pass(self) :: initialize_vtk => initialize_output_overall_vtk
        procedure, private, pass(self) :: initialize_vtu => initialize_output_overall_vtu

        procedure, public, pass(self) :: should_output => should_output_overall
    end type

    abstract interface
        subroutine abst_output_overall_fields(self, file_counts, porosity, temperature, si, pressure, water_flux)
            import :: type_output_overall, type_coordinate_array_dp, real64, int32
            implicit none
            class(type_output_overall), intent(inout) :: self
            integer(int32), intent(in) :: file_counts
            real(real64), intent(in), optional :: porosity(:)
            real(real64), intent(in), optional :: temperature(:)
            real(real64), intent(in), optional :: si(:)
            real(real64), intent(in), optional :: pressure(:)
            type(type_coordinate_array_dp), intent(in), optional :: water_flux
        end subroutine abst_output_overall_fields

        subroutine abst_output_overall_cell(self, file_name, variable_name, variable)
            import :: type_output_overall, int32
            implicit none
            class(type_output_overall), intent(inout) :: self
            character(*), intent(in) :: file_name
            character(*), intent(in) :: variable_name
            integer(int32), intent(in) :: variable(:)
        end subroutine abst_output_overall_cell
    end interface

    interface
        ! [修正] control 追加, coordinate 削除
        module subroutine initialize_input_type_output_overall(self, input, control, dir_output)
            implicit none
            class(type_output_overall), intent(inout) :: self
            type(type_input), intent(in) :: input
            type(type_control), intent(in) :: control
            ! type(type_domain), intent(inout) :: domain
            character(*), intent(in) :: dir_output
        end subroutine initialize_input_type_output_overall

        ! [修正] coordinate 削除
        module subroutine initialize_output_overall_vtk(self, input)
            implicit none
            class(type_output_overall), intent(inout) :: self
            type(type_input), intent(in) :: input
            ! type(type_domain), intent(inout) :: domain
        end subroutine initialize_output_overall_vtk

        ! [修正] coordinate 削除
        module subroutine initialize_output_overall_vtu(self, input)
            implicit none
            class(type_output_overall), intent(inout) :: self
            type(type_input), intent(in) :: input
            ! type(type_domain), intent(inout) :: domain
        end subroutine initialize_output_overall_vtu

        module pure function should_output_overall(self) result(should_output)
            implicit none
            class(type_output_overall), intent(in) :: self
            logical :: should_output
        end function should_output_overall
    end interface

end module io_output_overall
