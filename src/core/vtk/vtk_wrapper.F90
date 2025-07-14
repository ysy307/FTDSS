module core_vtk_vtk_wrapper
    use, intrinsic :: iso_c_binding
    implicit none
    private

    public :: vtk_initialize
    public :: vtk_read_header
    public :: vtk_get_num_points
    public :: vtk_get_points
    public :: vtk_get_num_cells
    public :: vtk_get_total_connectivity_size
    public :: vtk_get_cell_info
    public :: vtk_get_cell_ids
    public :: vtk_finalize

    ! Cラッパー関数のインターフェース
    interface
        subroutine vtk_initialize(filename, error_code) bind(C, name='c_vtk_initialize')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: filename(*)
            integer(c_int), intent(out) :: error_code
        end subroutine vtk_initialize

        subroutine vtk_read_header(format, format_len, dataset, dataset_len) bind(C, name='c_vtk_read_header')
            import :: c_char, c_int
            character(kind=c_char), intent(out) :: format(*)
            integer(c_int), value, intent(in) :: format_len
            character(kind=c_char), intent(out) :: dataset(*)
            integer(c_int), value, intent(in) :: dataset_len
        end subroutine vtk_read_header

        subroutine vtk_get_num_points(num_points) bind(C, name='c_vtk_get_num_points')
            import :: c_int
            integer(c_int), intent(out) :: num_points
        end subroutine vtk_get_num_points

        subroutine vtk_get_points(x, y, z) bind(C, name='c_vtk_get_points')
            import :: c_double
            real(c_double), intent(out) :: x(*), y(*), z(*)
        end subroutine vtk_get_points

        subroutine vtk_get_num_cells(num_cells) bind(C, name='c_vtk_get_num_cells')
            import :: c_int
            integer(c_int), intent(out) :: num_cells
        end subroutine vtk_get_num_cells

        subroutine vtk_get_total_connectivity_size(size) bind(C, name='c_vtk_get_total_connectivity_size')
            import :: c_long_long
            integer(c_long_long), intent(out) :: size
        end subroutine vtk_get_total_connectivity_size

        subroutine vtk_get_cell_info(connectivity, offsets, types) bind(C, name='c_vtk_get_cell_info')
            import :: c_long_long, c_int
            integer(c_long_long), intent(out) :: connectivity(*)
            integer(c_long_long), intent(out) :: offsets(*)
            integer(c_int), intent(out) :: types(*)
        end subroutine vtk_get_cell_info

        subroutine vtk_get_cell_ids(array_name, ids) bind(C, name='c_vtk_get_cell_ids')
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: array_name(*)
            integer(c_int), intent(out) :: ids(*)
        end subroutine vtk_get_cell_ids

        subroutine vtk_finalize() bind(C, name='c_vtk_finalize')
        end subroutine
    end interface

end module core_vtk_vtk_wrapper
