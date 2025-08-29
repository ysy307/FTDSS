module core_vtk_vtk_wrapper
    use, intrinsic :: iso_c_binding
    implicit none
    private

    ! --- 公開するプロシージャ ---
    public :: vtk_initialize
    public :: vtk_read_header
    public :: vtk_get_num_points
    public :: vtk_get_points
    public :: vtk_get_num_cells
    public :: vtk_get_total_connectivity_size
    public :: vtk_get_cell_info
    public :: vtk_get_cell_ids
    public :: vtk_get_point_data
    public :: vtk_finalize

    !----------------------------------------------------------------!
    ! C++のラッパー関数へのインターフェース定義
    !----------------------------------------------------------------!
    interface
        ! initializeはC++オブジェクトのポインタ(ハンドル)を返す関数になる
        function vtk_initialize(filename, error_code) bind(C, name='c_vtk_initialize')
            import :: c_ptr, c_char, c_int
            type(c_ptr) :: vtk_initialize
            character(kind=c_char), intent(in) :: filename(*)
            integer(c_int), intent(out) :: error_code
        end function vtk_initialize

        ! finalizeは解放対象のハンドルを引数として受け取る
        subroutine vtk_finalize(handle) bind(C, name='c_vtk_finalize')
            import :: c_ptr
            type(c_ptr), value, intent(in) :: handle
        end subroutine vtk_finalize

        subroutine vtk_read_header(handle, format, format_len, dataset, dataset_len) bind(C, name='c_vtk_read_header')
            import :: c_ptr, c_char, c_int
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(out) :: format(*)
            integer(c_int), value, intent(in) :: format_len
            character(kind=c_char), intent(out) :: dataset(*)
            integer(c_int), value, intent(in) :: dataset_len
        end subroutine vtk_read_header

        subroutine vtk_get_num_points(handle, num_points) bind(C, name='c_vtk_get_num_points')
            import :: c_ptr, c_int
            type(c_ptr), value, intent(in) :: handle
            integer(c_int), intent(out) :: num_points
        end subroutine vtk_get_num_points

        subroutine vtk_get_points(handle, x, y, z) bind(C, name='c_vtk_get_points')
            import :: c_ptr, c_double
            type(c_ptr), value, intent(in) :: handle
            real(c_double), intent(out) :: x(*), y(*), z(*)
        end subroutine vtk_get_points

        subroutine vtk_get_num_cells(handle, num_cells) bind(C, name='c_vtk_get_num_cells')
            import :: c_ptr, c_int
            type(c_ptr), value, intent(in) :: handle
            integer(c_int), intent(out) :: num_cells
        end subroutine vtk_get_num_cells

        subroutine vtk_get_total_connectivity_size(handle, size_val) bind(C, name='c_vtk_get_total_connectivity_size')
            import :: c_ptr, c_long_long
            type(c_ptr), value, intent(in) :: handle
            integer(c_long_long), intent(out) :: size_val
        end subroutine vtk_get_total_connectivity_size

        subroutine vtk_get_cell_info(handle, connectivity, offsets, types) bind(C, name='c_vtk_get_cell_info')
            import :: c_ptr, c_long_long, c_int
            type(c_ptr), value, intent(in) :: handle
            integer(c_long_long), intent(out) :: connectivity(*)
            integer(c_long_long), intent(out) :: offsets(*)
            integer(c_int), intent(out) :: types(*)
        end subroutine vtk_get_cell_info

        subroutine vtk_get_cell_ids(handle, array_name, ids) bind(C, name='c_vtk_get_cell_ids')
            import :: c_ptr, c_int, c_char
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(in) :: array_name(*)
            integer(c_int), intent(out) :: ids(*)
        end subroutine vtk_get_cell_ids

        subroutine vtk_get_point_data(handle, array_name, point_data) bind(C, name='c_vtk_get_point_data')
            import :: c_ptr, c_double, c_char
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(in) :: array_name(*)
            real(c_double), intent(out) :: point_data(*)
        end subroutine vtk_get_point_data

    end interface

end module core_vtk_vtk_wrapper
