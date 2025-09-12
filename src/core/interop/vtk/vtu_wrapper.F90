module core_vtk_vtu_wrapper
    use, intrinsic :: iso_c_binding
    implicit none
    private

    ! --- 公開するプロシージャ ---
    public :: vtu_initialize
    public :: vtu_read_header
    public :: vtu_get_num_points
    public :: vtu_get_points
    public :: vtu_get_num_cells
    public :: vtu_get_total_connectivity_size
    public :: vtu_get_cell_info
    public :: vtu_get_cell_data_int32
    public :: vtu_get_cell_data_float64
    public :: vtu_get_point_data_int32
    public :: vtu_get_point_data_float64
    public :: vtu_finalize

    !----------------------------------------------------------------!
    ! C++のラッパー関数へのインターフェース定義
    !----------------------------------------------------------------!
    interface
        ! initializeはC++オブジェクトのポインタ(ハンドル)を返す関数になる
        function vtu_initialize(filename, error_code) bind(C, name='c_vtu_initialize')
            import :: c_ptr, c_char, c_int
            type(c_ptr) :: vtu_initialize
            character(kind=c_char), intent(in) :: filename(*)
            integer(c_int), intent(out) :: error_code
        end function vtu_initialize

        ! finalizeは解放対象のハンドルを引数として受け取る
        subroutine vtu_finalize(handle) bind(C, name='c_vtu_finalize')
            import :: c_ptr
            type(c_ptr), value, intent(in) :: handle
        end subroutine vtu_finalize

        subroutine vtu_read_header(handle, format, format_len, dataset, dataset_len) bind(C, name='c_vtu_read_header')
            import :: c_ptr, c_char, c_int
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(out) :: format(*)
            integer(c_int), value, intent(in) :: format_len
            character(kind=c_char), intent(out) :: dataset(*)
            integer(c_int), value, intent(in) :: dataset_len
        end subroutine vtu_read_header

        subroutine vtu_get_num_points(handle, num_points) bind(C, name='c_vtu_get_num_points')
            import :: c_ptr, c_int
            type(c_ptr), value, intent(in) :: handle
            integer(c_int), intent(out) :: num_points
        end subroutine vtu_get_num_points

        subroutine vtu_get_points(handle, x, y, z) bind(C, name='c_vtu_get_points')
            import :: c_ptr, c_double
            type(c_ptr), value, intent(in) :: handle
            real(c_double), intent(out) :: x(*), y(*), z(*)
        end subroutine vtu_get_points

        subroutine vtu_get_num_cells(handle, num_cells) bind(C, name='c_vtu_get_num_cells')
            import :: c_ptr, c_int
            type(c_ptr), value, intent(in) :: handle
            integer(c_int), intent(out) :: num_cells
        end subroutine vtu_get_num_cells

        subroutine vtu_get_total_connectivity_size(handle, size_val) bind(C, name='c_vtu_get_total_connectivity_size')
            import :: c_ptr, c_long_long
            type(c_ptr), value, intent(in) :: handle
            integer(c_long_long), intent(out) :: size_val
        end subroutine vtu_get_total_connectivity_size

        subroutine vtu_get_cell_info(handle, connectivity, offsets, types) bind(C, name='c_vtu_get_cell_info')
            import :: c_ptr, c_long_long, c_int
            type(c_ptr), value, intent(in) :: handle
            integer(c_long_long), intent(out) :: connectivity(*)
            integer(c_long_long), intent(out) :: offsets(*)
            integer(c_int), intent(out) :: types(*)
        end subroutine vtu_get_cell_info

        subroutine vtu_get_cell_data_int32(handle, array_name, ids) bind(C, name='c_vtu_get_cell_data_int32')
            import :: c_ptr, c_int, c_char
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(in) :: array_name(*)
            integer(c_int), intent(out) :: ids(*)
        end subroutine vtu_get_cell_data_int32

        subroutine vtu_get_cell_data_float64(handle, array_name, data) bind(C, name='c_vtu_get_cell_data_float64')
            import :: c_ptr, c_double, c_char
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(in) :: array_name(*)
            real(c_double), intent(out) :: data(*)
        end subroutine vtu_get_cell_data_float64

        subroutine vtu_get_point_data_int32(handle, array_name, point_data) bind(C, name='c_vtu_get_point_data_int32')
            import :: c_ptr, c_int, c_char
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(in) :: array_name(*)
            integer(c_int), intent(out) :: point_data(*)
        end subroutine vtu_get_point_data_int32

        subroutine vtu_get_point_data_float64(handle, array_name, point_data) bind(C, name='c_vtu_get_point_data_float64')
            import :: c_ptr, c_double, c_char
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(in) :: array_name(*)
            real(c_double), intent(out) :: point_data(*)
        end subroutine vtu_get_point_data_float64

    end interface
end module core_vtk_vtu_wrapper
