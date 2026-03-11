module core_interop_vtu_wrapper
    use, intrinsic :: iso_c_binding
    implicit none
    private

    ! --- Public procedures ---
    public :: vtu_initialize
    public :: vtu_read_header
    public :: vtu_get_num_points
    public :: vtu_get_points
    public :: vtu_get_num_cells
    public :: vtu_get_total_connectivity_size
    public :: vtu_get_cell_info
    public :: vtu_get_num_cell_data_components
    public :: vtu_get_num_point_data_components
    public :: vtu_get_cell_data_int32
    public :: vtu_get_cell_data_float64
    public :: vtu_get_point_data_int32
    public :: vtu_get_point_data_float64
    public :: vtu_finalize

    !----------------------------------------------------------------!
    ! Interface definitions for C++ wrapper functions
    !----------------------------------------------------------------!
    interface
        ! Initialize returns a pointer (handle) to a C++ object
        function vtu_initialize(filename, error_code) bind(C, name='c_vtu_initialize')
            import :: c_ptr, c_char, c_int
            type(c_ptr) :: vtu_initialize
            character(kind=c_char), intent(in) :: filename(*)
            integer(c_int), intent(inout) :: error_code
        end function vtu_initialize

        ! Finalize takes the handle to be released as an argument
        subroutine vtu_finalize(handle) bind(C, name='c_vtu_finalize')
            import :: c_ptr
            type(c_ptr), value, intent(in) :: handle
        end subroutine vtu_finalize

        subroutine vtu_read_header(handle, format, format_len, dataset, dataset_len) bind(C, name='c_vtu_read_header')
            import :: c_ptr, c_char, c_int
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(inout) :: format(*)
            integer(c_int), value, intent(in) :: format_len
            character(kind=c_char), intent(inout) :: dataset(*)
            integer(c_int), value, intent(in) :: dataset_len
        end subroutine vtu_read_header

        subroutine vtu_get_num_points(handle, num_points) bind(C, name='c_vtu_get_num_points')
            import :: c_ptr, c_int
            type(c_ptr), value, intent(in) :: handle
            integer(c_int), intent(inout) :: num_points
        end subroutine vtu_get_num_points

        subroutine vtu_get_points(handle, x, y, z) bind(C, name='c_vtu_get_points')
            import :: c_ptr, c_double
            type(c_ptr), value, intent(in) :: handle
            real(c_double), intent(inout) :: x(*), y(*), z(*)
        end subroutine vtu_get_points

        subroutine vtu_get_num_cells(handle, num_cells) bind(C, name='c_vtu_get_num_cells')
            import :: c_ptr, c_int
            type(c_ptr), value, intent(in) :: handle
            integer(c_int), intent(inout) :: num_cells
        end subroutine vtu_get_num_cells

        subroutine vtu_get_total_connectivity_size(handle, size_val) bind(C, name='c_vtu_get_total_connectivity_size')
            import :: c_ptr, c_long_long
            type(c_ptr), value, intent(in) :: handle
            integer(c_long_long), intent(inout) :: size_val
        end subroutine vtu_get_total_connectivity_size

        subroutine vtu_get_cell_info(handle, connectivity, offsets, types) bind(C, name='c_vtu_get_cell_info')
            import :: c_ptr, c_long_long, c_int
            type(c_ptr), value, intent(in) :: handle
            integer(c_long_long), intent(inout) :: connectivity(*)
            integer(c_long_long), intent(inout) :: offsets(*)
            integer(c_int), intent(inout) :: types(*)
        end subroutine vtu_get_cell_info

        subroutine vtu_get_num_cell_data_components(handle, array_name, num_components) &
            bind(C, name='c_vtu_get_num_cell_data_components')
            import :: c_ptr, c_char, c_int
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(in) :: array_name(*)
            integer(c_int), intent(inout) :: num_components
        end subroutine vtu_get_num_cell_data_components

        subroutine vtu_get_num_point_data_components(handle, array_name, num_components) &
            bind(C, name='c_vtu_get_num_point_data_components')
            import :: c_ptr, c_char, c_int
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(in) :: array_name(*)
            integer(c_int), intent(inout) :: num_components
        end subroutine vtu_get_num_point_data_components

        subroutine vtu_get_cell_data_int32(handle, array_name, ids) bind(C, name='c_vtu_get_cell_data_int32')
            import :: c_ptr, c_int, c_char
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(in) :: array_name(*)
            integer(c_int), intent(inout) :: ids(*)
        end subroutine vtu_get_cell_data_int32

        subroutine vtu_get_cell_data_float64(handle, array_name, data) bind(C, name='c_vtu_get_cell_data_float64')
            import :: c_ptr, c_double, c_char
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(in) :: array_name(*)
            real(c_double), intent(inout) :: data(*)
        end subroutine vtu_get_cell_data_float64

        subroutine vtu_get_point_data_int32(handle, array_name, point_data) bind(C, name='c_vtu_get_point_data_int32')
            import :: c_ptr, c_int, c_char
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(in) :: array_name(*)
            integer(c_int), intent(inout) :: point_data(*)
        end subroutine vtu_get_point_data_int32

        subroutine vtu_get_point_data_float64(handle, array_name, point_data) bind(C, name='c_vtu_get_point_data_float64')
            import :: c_ptr, c_double, c_char
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(in) :: array_name(*)
            real(c_double), intent(inout) :: point_data(*)
        end subroutine vtu_get_point_data_float64

    end interface
end module core_interop_vtu_wrapper
