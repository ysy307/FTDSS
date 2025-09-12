module core_vtk
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding
#ifdef _MPI
    use mpi_f08
#endif
    use :: stdlib_strings, only:to_string
    use :: stdlib_sorting, only:sort
    use :: core_types, only:type_dp_3d
    use :: core_allocate, only:allocate_array
    use :: core_deallocate, only:deallocate_array
    use :: core_unique, only:unique
    use :: core_vtk_vtk_constants
    use :: core_vtk_vtk_wrapper, only:vtk_initialize, vtk_read_header, vtk_get_num_points, &
        vtk_get_points, vtk_get_num_cells, vtk_get_total_connectivity_size, &
        vtk_get_cell_info, vtk_get_cell_data_int32, vtk_get_cell_data_float64, vtk_get_point_data_int32, vtk_get_point_data_float64, vtk_finalize
    use :: core_vtk_vtu_wrapper, only:vtu_initialize, vtu_read_header, vtu_get_num_points, &
        vtu_get_points, vtu_get_num_cells, vtu_get_total_connectivity_size, &
        vtu_get_cell_info, vtu_get_cell_data_int32, vtu_get_cell_data_float64, vtu_get_point_data_int32, vtu_get_point_data_float64, vtu_finalize

    implicit none
    private

    public :: type_vtk
    public :: type_vtk_cell

    ! Fortran側のデータ構造

    type :: type_vtk_cell
        integer(int32) :: cell_type
        character(:), allocatable :: cell_type_name
        integer(int32) :: num_nodes_in_cell
        integer(int32) :: cell_entity_id
        integer(int32) :: cell_dimension
        integer(int32) :: cell_order
        integer(int32), allocatable :: connectivity(:)

    contains
        procedure :: set => type_vtk_cell_set
        procedure :: get_dimension => type_vtk_cell_get_dimension
        procedure :: get_order => type_vtk_cell_get_order
    end type type_vtk_cell

    type :: type_vtk
        character(:), allocatable :: format
        character(:), allocatable :: dataset
        ! VTK Points data
        integer(int32) :: num_points ! Local number of points
        type(type_dp_3d) :: points
        ! VTK Cells data
        integer(int32) :: num_total_cells ! Local number of cells
        type(type_vtk_cell), allocatable :: cells(:)

#ifdef _MPI
        ! --- MPI-specific members ---
        integer(int32) :: my_rank = 0
        integer(int32) :: num_procs = 1
        integer(int32) :: global_num_points = 0
        integer(int32) :: global_num_total_cells = 0
        integer(int32), allocatable :: global_node_ids(:)
#endif

        type(c_ptr), private :: handle = c_null_ptr
        character(4), private :: reader_type = "none"
    contains
        procedure :: initialize_vtk => type_vtk_vtk_initialize
        procedure :: initialize_vtu => type_vtk_vtu_initialize
        procedure :: get_active_region_info
        final :: finalize_vtk_object
    end type type_vtk

contains
    ! =================================================================
    ! 公開APIの実装 (内部をC++呼び出しに置き換え)
    ! =================================================================
    subroutine type_vtk_vtk_initialize(self, file_name, cell_id_array_name, field_names, field_values)
        !> Read VTK file using C++ backend with the handle pattern
        implicit none
        class(type_vtk), intent(inout) :: self
        character(*), intent(in) :: file_name
        character(*), intent(in) :: cell_id_array_name
        character(*), intent(in), optional :: field_names(:)
        real(real64), intent(inout), allocatable, optional :: field_values(:, :)

        ! --- ローカル変数 ---
        character(len=256) :: c_file_name
        character(len=256) :: c_cell_id_array_name
        character(len=256) :: c_field_name
        integer(c_int) :: ierr
        integer(int32) :: i

        ! --- 生データ格納用の一時配列 ---
        integer(int64), allocatable :: raw_connectivity(:)
        integer(int64), allocatable :: raw_offsets(:)
        integer(int32), allocatable :: raw_cell_types(:)
        integer(int32), allocatable :: raw_cell_entity_ids(:)
        integer(int64) :: total_conn_size
        integer(int32) :: connectivity_first, connectivity_last, num_nodes_in_cell
        character(50, kind=c_char) :: f_format, f_dataset
        integer(c_int) :: len_f_format, len_f_dataset
#ifdef _MPI
        integer(int32) :: global_max_node_id, local_max_node_id
        integer(int32) :: my_rank
        integer(int32) :: dot_pos
#endif

        !----------------------------------------------------------------!
        ! 1. C++リーダーの初期化とハンドルの取得
        !----------------------------------------------------------------!

        ! 自分が "vtk" リーダーであることを記録 (finalizerでの呼び分けに使う)
        self%reader_type = "vtk"

        ! もし既にハンドルが有効なら、古いC++オブジェクトを解放してメモリリークを防ぐ
        if (c_associated(self%handle)) then
            call vtk_finalize(self%handle)
            self%handle = c_null_ptr
        end if

        ! C言語で扱えるように文字列を準備
#ifdef _MPI
        call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)

        dot_pos = index(trim(file_name), '.', back=.true.)
        if (dot_pos > 0) then
            c_file_name = file_name(1:dot_pos - 1)//"_"//to_string(my_rank) &
                          //file_name(dot_pos:len_trim(file_name))//c_null_char
        else
            c_file_name = trim(file_name)//"_"//to_string(my_rank)//c_null_char
        end if
#else
        c_file_name = trim(file_name)//c_null_char
#endif
        c_cell_id_array_name = trim(cell_id_array_name)//c_null_char

        ! C++リーダーを初期化し、返されたポインタをハンドルとして保存
        self%handle = vtk_initialize(c_file_name, ierr)

        ! ハンドルが有効か、エラーコードが0かを確認
        if (.not. c_associated(self%handle) .or. ierr /= 0) then
            write (*, *) "C++ VTK Reader failed to initialize for file: ", trim(c_file_name)
            write (*, *) "Error code: ", ierr
            stop "VTK Initialization Failed"
        end if

        !----------------------------------------------------------------!
        ! 2. ハンドルを使って各種データを取得
        !----------------------------------------------------------------!

        ! ヘッダー情報の取得
        len_f_dataset = 50
        len_f_format = 50
        call vtk_read_header(self%handle, f_format, len_f_dataset, f_dataset, len_f_format)
        allocate (character(len=len_trim(f_format)) :: self%format)
        self%format = trim(adjustl(f_format))
        allocate (character(len=len_trim(f_dataset)) :: self%dataset)
        self%dataset = trim(adjustl(f_dataset))

        ! ポイントデータの取得
        call vtk_get_num_points(self%handle, self%num_points)
        if (self%num_points > 0) then
            call self%POINTS%initialize(self%num_points)
            call vtk_get_points(self%handle, self%POINTS%x, self%POINTS%y, self%POINTS%z)
        end if

        ! セルデータの取得
        call vtk_get_num_cells(self%handle, self%num_total_cells)
        if (self%num_total_cells > 0) then
            ! 生データをCから取得するためのメモリ確保
            call vtk_get_total_connectivity_size(self%handle, total_conn_size)
            call allocate_array(raw_connectivity, total_conn_size)
            call allocate_array(raw_offsets, self%num_total_cells + 1_int64)
            call allocate_array(raw_cell_types, self%num_total_cells)
            call allocate_array(raw_cell_entity_ids, self%num_total_cells)

            ! 生データをCから取得
            call vtk_get_cell_info(self%handle, raw_connectivity, raw_offsets, raw_cell_types)
            call vtk_get_cell_data_int32(self%handle, c_cell_id_array_name, raw_cell_entity_ids)

            ! Fortran構造体にデータを格納し直す
            allocate (self%cells(self%num_total_cells))
            do i = 1, self%num_total_cells
                self%cells(i)%cell_type = raw_cell_types(i)
                self%cells(i)%cell_entity_id = raw_cell_entity_ids(i)

                connectivity_first = raw_offsets(i) + 1
                connectivity_last = raw_offsets(i + 1)
                num_nodes_in_cell = connectivity_last - connectivity_first + 1

                call allocate_array(self%cells(i)%connectivity, num_nodes_in_cell)
                ! Connectivity read from file is 0-based, needs to be converted to 1-based for Fortran
                self%cells(i)%connectivity(:) = int(raw_connectivity(connectivity_first:connectivity_last), kind=int32) + 1
                call self%cells(i)%set(num_nodes_in_cell)
            end do
        end if

        ! 初期条件フィールドの読み込み (オプション)
        if (present(field_names) .and. present(field_values)) then
            if (self%num_points > 0 .and. size(field_names) > 0) then
                allocate (field_values(self%num_points, size(field_names)))
                do i = 1, size(field_names)
                    c_field_name = trim(field_names(i))//c_null_char
                    call vtk_get_point_data_float64(self%handle, c_field_name, field_values(:, i))
                end do
            end if
        end if

#ifdef _MPI
        ! --- 3. MPI: グローバル情報の計算とインデックスの更新 ---
        call MPI_Comm_rank(MPI_COMM_WORLD, self%my_rank, ierr)
        call MPI_Comm_size(MPI_COMM_WORLD, self%num_procs, ierr)

        ! [MODIFIED] Read global_node_id from point data
        if (self%num_points > 0) then
            allocate (self%global_node_ids(self%num_points))
            c_field_name = "global_node_id"//c_null_char
            call vtk_get_point_data_int32(self%handle, c_field_name, self%global_node_ids)
            self%global_node_ids = self%global_node_ids + 1 ! Convert to 1-based indexing
        end if

        ! グローバルな総数を計算 (global_num_pointsは最大IDから推定するか、別途ファイルで渡すのが堅牢)
        if (self%num_points > 0) then
            local_max_node_id = maxval(self%global_node_ids)
        else
            local_max_node_id = 0
        end if

        call MPI_Allreduce(local_max_node_id, global_max_node_id, 1, MPI_INTEGER4, MPI_MAX, MPI_COMM_WORLD, ierr)
        self%global_num_points = global_max_node_id
        call MPI_Allreduce(self%num_total_cells, self%global_num_total_cells, 1, MPI_INTEGER4, MPI_SUM, MPI_COMM_WORLD, ierr)

        ! [MODIFIED] Convert connectivity to global indices using the map
        if (self%num_total_cells > 0 .and. self%num_points > 0) then
            do i = 1, self%num_total_cells
                self%cells(i)%connectivity(:) = self%global_node_ids(self%cells(i)%connectivity(:))
            end do
        end if

#endif

        !----------------------------------------------------------------!
        ! 4. 後片付け (C++オブジェクトのfinalizeは呼ばない)
        !----------------------------------------------------------------!
        call deallocate_array(raw_connectivity)
        call deallocate_array(raw_offsets)
        call deallocate_array(raw_cell_types)
        call deallocate_array(raw_cell_entity_ids)

    end subroutine type_vtk_vtk_initialize

    subroutine type_vtk_vtu_initialize(self, file_name, cell_id_array_name, field_names, field_values)
        !> Read VTU file using C++ backend with the handle pattern
        implicit none
        class(type_vtk), intent(inout) :: self
        character(*), intent(in) :: file_name
        character(*), intent(in) :: cell_id_array_name
        character(*), intent(in), optional :: field_names(:)
        real(real64), intent(inout), allocatable, optional :: field_values(:, :)

        ! --- ローカル変数 ---
        character(len=256) :: c_file_name
        character(len=256) :: c_cell_id_array_name
        character(len=256) :: c_field_name
        integer(c_int) :: ierr
        integer(int32) :: i

        ! --- 生データ格納用の一時配列 ---
        integer(int64), allocatable :: raw_connectivity(:)
        integer(int64), allocatable :: raw_offsets(:)
        integer(int32), allocatable :: raw_cell_types(:)
        integer(int32), allocatable :: raw_cell_entity_ids(:)
        integer(int64) :: total_conn_size
        integer(int32) :: connectivity_first, connectivity_last, num_nodes_in_cell
        character(50, kind=c_char) :: f_format, f_dataset
        integer(c_int) :: len_f_format, len_f_dataset
#ifdef _MPI
        integer(int32) :: my_rank
        integer(int32) :: dot_pos
        integer(int32) :: local_max_node_id, global_max_node_id
#endif

        !----------------------------------------------------------------!
        ! 1. C++リーダーの初期化とハンドルの取得
        !----------------------------------------------------------------!

        ! 自分が "vtu" リーダーであることを記録 (finalizerでの呼び分けに使う)
        self%reader_type = "vtu"

        ! もし既にハンドルが有効なら、古いC++オブジェクトを解放してメモリリークを防ぐ
        if (c_associated(self%handle)) then
            call vtu_finalize(self%handle)
            self%handle = c_null_ptr
        end if

#ifdef _MPI
        call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)

        dot_pos = index(trim(file_name), '.', back=.true.)
        if (dot_pos > 0) then
            c_file_name = trim(file_name(1:dot_pos - 1))//"_"//trim(adjustl(to_string(my_rank))) &
                          //trim(file_name(dot_pos:len_trim(file_name)))//c_null_char
        else
            c_file_name = trim(file_name)//"_"//trim(adjustl(to_string(my_rank)))//c_null_char
        end if
#else
        c_file_name = trim(file_name)//c_null_char
#endif
        c_cell_id_array_name = trim(cell_id_array_name)//c_null_char

        ! C++リーダーを初期化し、返されたポインタをハンドルとして保存
        self%handle = vtu_initialize(c_file_name, ierr)

        ! ハンドルが有効か、エラーコードが0かを確認
        if (.not. c_associated(self%handle) .or. ierr /= 0) then
            write (*, *) "C++ VTU Reader failed to initialize for file: ", trim(c_file_name)
            write (*, *) "Error code: ", ierr
            stop "VTU Initialization Failed"
        end if

        !----------------------------------------------------------------!
        ! 2. ハンドルを使って各種データを取得
        !----------------------------------------------------------------!

        ! ヘッダー情報の取得
        len_f_dataset = 50
        len_f_format = 50
        call vtu_read_header(self%handle, f_format, len_f_dataset, f_dataset, len_f_format)
        allocate (character(len=len_trim(f_format)) :: self%format)
        self%format = trim(adjustl(f_format))
        allocate (character(len=len_trim(f_dataset)) :: self%dataset)
        self%dataset = trim(adjustl(f_dataset))

        ! ポイントデータの取得
        call vtu_get_num_points(self%handle, self%num_points)
        if (self%num_points > 0) then
            call self%POINTS%initialize(self%num_points)
            call vtu_get_points(self%handle, self%POINTS%x, self%POINTS%y, self%POINTS%z)
        end if

        ! セルデータの取得
        call vtu_get_num_cells(self%handle, self%num_total_cells)
        if (self%num_total_cells > 0) then
            ! 生データをCから取得するためのメモリ確保
            call vtu_get_total_connectivity_size(self%handle, total_conn_size)
            call allocate_array(raw_connectivity, total_conn_size)
            call allocate_array(raw_offsets, self%num_total_cells + 1_int64)
            call allocate_array(raw_cell_types, self%num_total_cells)
            call allocate_array(raw_cell_entity_ids, self%num_total_cells)

            ! 生データをCから取得
            call vtu_get_cell_info(self%handle, raw_connectivity, raw_offsets, raw_cell_types)
            call vtu_get_cell_data_int32(self%handle, c_cell_id_array_name, raw_cell_entity_ids)

            ! Fortran構造体にデータを格納し直す
            allocate (self%cells(self%num_total_cells))
            do i = 1, self%num_total_cells
                self%cells(i)%cell_type = raw_cell_types(i)
                self%cells(i)%cell_entity_id = raw_cell_entity_ids(i)

                connectivity_first = raw_offsets(i) + 1
                connectivity_last = raw_offsets(i + 1)
                num_nodes_in_cell = connectivity_last - connectivity_first + 1

                call allocate_array(self%cells(i)%connectivity, num_nodes_in_cell)
                ! Connectivity read from file is 0-based, needs to be converted to 1-based for Fortran
                self%cells(i)%connectivity(:) = int(raw_connectivity(connectivity_first:connectivity_last), kind=int32) + 1
                call self%cells(i)%set(num_nodes_in_cell)
            end do
        end if

        ! 初期条件フィールドの読み込み (オプション)
        if (present(field_names) .and. present(field_values)) then
            if (self%num_points > 0 .and. size(field_names) > 0) then
                allocate (field_values(self%num_points, size(field_names)))
                do i = 1, size(field_names)
                    c_field_name = trim(field_names(i))//c_null_char
                    call vtu_get_point_data_float64(self%handle, c_field_name, field_values(:, i))
                end do
            end if
        end if

#ifdef _MPI
        ! --- 3. MPI: グローバル情報の計算とインデックスの更新 ---
        call MPI_Comm_rank(MPI_COMM_WORLD, self%my_rank, ierr)
        call MPI_Comm_size(MPI_COMM_WORLD, self%num_procs, ierr)

        ! [MODIFIED] Read global_node_id from point data
        if (self%num_points > 0) then
            allocate (self%global_node_ids(self%num_points))
            c_field_name = "global_node_id"//c_null_char
            ! NOTE: vtu_get_point_data should be able to handle integer arrays.
            ! Assuming a generic or specific integer version exists.
            call vtu_get_point_data_int32(self%handle, c_field_name, self%global_node_ids)
            self%global_node_ids = self%global_node_ids + 1 ! Convert to 1-based indexing
        end if

        ! グローバルな総数を計算
        if (self%num_points > 0) then
            local_max_node_id = maxval(self%global_node_ids)
        else
            local_max_node_id = 0
        end if
        call MPI_Allreduce(local_max_node_id, global_max_node_id, 1, MPI_INTEGER4, MPI_MAX, MPI_COMM_WORLD, ierr)
        self%global_num_points = global_max_node_id
        call MPI_Allreduce(self%num_total_cells, self%global_num_total_cells, 1, MPI_INTEGER4, MPI_SUM, MPI_COMM_WORLD, ierr)

        ! [MODIFIED] Convert connectivity to global indices using the map
        ! The connectivity array holds 1-based local indices. Use them to look up the global ID.
        if (self%num_total_cells > 0 .and. self%num_points > 0) then
            do i = 1, self%num_total_cells
                self%cells(i)%connectivity(:) = self%global_node_ids(self%cells(i)%connectivity(:))
            end do
        end if

#endif

        !----------------------------------------------------------------!
        ! 4. 後片付け (C++オブジェクトのfinalizeは呼ばない)
        !----------------------------------------------------------------!
        call deallocate_array(raw_connectivity)
        call deallocate_array(raw_offsets)
        call deallocate_array(raw_cell_types)
        call deallocate_array(raw_cell_entity_ids)

    end subroutine type_vtk_vtu_initialize

    subroutine type_vtk_cell_set(self, num_nodes_in_cell)
        implicit none
        class(type_vtk_cell), intent(inout) :: self !! VTK cells data
        integer(int32), intent(in) :: num_nodes_in_cell !! セルのノード数

        select case (self%cell_type)
        case (VTK_VERTEX)
            self%cell_type_name = "Vertex"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 0 ! 0次元要素
            self%cell_order = 1
        case (VTK_POLY_VERTEX)
            self%cell_type_name = "PolyVertex"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 0 ! 0次元要素の集合
            self%cell_order = 1
        case (VTK_LINE)
            self%cell_type_name = "Line"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 1
            self%cell_order = 1
        case (VTK_POLY_LINE)
            self%cell_type_name = "PolyLine"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 1
            self%cell_order = -1 ! 可変
        case (VTK_TRIANGLE)
            self%cell_type_name = "Triangle"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = 1
        case (VTK_TRIANGLE_STRIP)
            self%cell_type_name = "TriangleStrip"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = 1
        case (VTK_POLYGON)
            self%cell_type_name = "Polygon"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = -1 ! 可変
        case (VTK_PIXEL)
            self%cell_type_name = "Pixel"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = 1
        case (VTK_QUAD)
            self%cell_type_name = "Quad"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = 1
        case (VTK_TETRA)
            self%cell_type_name = "Tetra"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = 1
        case (VTK_VOXEL)
            self%cell_type_name = "Voxel"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = 1
        case (VTK_HEXAHEDRON)
            self%cell_type_name = "Hexahedron"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = 1
        case (VTK_WEDGE)
            self%cell_type_name = "Wedge"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = 1
        case (VTK_PYRAMID)
            self%cell_type_name = "Pyramid"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = 1
        case (VTK_PENTAGONAL_PRISM)
            self%cell_type_name = "PentagonalPrism"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = 1
        case (VTK_HEXAGONAL_PRISM)
            self%cell_type_name = "HexagonalPrism"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = 1
        case (VTK_QUADRATIC_EDGE)
            self%cell_type_name = "QuadraticEdge"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 1
            self%cell_order = 2
        case (VTK_QUADRATIC_TRIANGLE)
            self%cell_type_name = "QuadraticTriangle"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = 2
        case (VTK_QUADRATIC_QUAD)
            self%cell_type_name = "QuadraticQuad"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = 2
        case (VTK_QUADRATIC_POLYGON)
            self%cell_type_name = "QuadraticPolygon"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = 2
        case (VTK_QUADRATIC_TETRA)
            self%cell_type_name = "QuadraticTetra"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = 2
        case (VTK_QUADRATIC_HEXAHEDRON)
            self%cell_type_name = "QuadraticHexahedron"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = 2
        case (VTK_QUADRATIC_WEDGE)
            self%cell_type_name = "QuadraticWedge"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = 2
        case (VTK_QUADRATIC_PYRAMID)
            self%cell_type_name = "QuadraticPyramid"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = 2
        case (VTK_BIQUADRATIC_QUAD)
            self%cell_type_name = "BiquadraticQuad"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = 2 ! 双二次
        case (VTK_TRIQUADRATIC_HEXAHEDRON)
            self%cell_type_name = "TriquadraticHexahedron"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = 2 ! 三次二次
        case (VTK_TRIQUADRATIC_PYRAMID)
            self%cell_type_name = "TriquadraticPyramid"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = 2 ! 三次二次
        case (VTK_QUADRATIC_LINEAR_QUAD)
            self%cell_type_name = "QuadraticLinearQuad"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = 2 ! 線形と二次
        case (VTK_QUADRATIC_LINEAR_WEDGE)
            self%cell_type_name = "QuadraticLinearWedge"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = 2 ! 線形と二次
        case (VTK_BIQUADRATIC_QUADRATIC_WEDGE)
            self%cell_type_name = "BiquadraticQuadraticWedge"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = 2 ! 双二次と二次
        case (VTK_BIQUADRATIC_QUADRATIC_HEXAHEDRON)
            self%cell_type_name = "BiquadraticQuadraticHexahedron"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = 2 ! 双二次と二次
        case (VTK_BIQUADRATIC_TRIANGLE)
            self%cell_type_name = "BiquadraticTriangle"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = 2 ! 双二次
        case (VTK_CUBIC_LINE)
            self%cell_type_name = "CubicLine"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 1
            self%cell_order = 3
        case (VTK_CONVEX_POINT_SET)
            self%cell_type_name = "ConvexPointSet"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = -1 ! 可変
            self%cell_order = -1 ! 可変
        case (VTK_POLYHEDRON)
            self%cell_type_name = "Polyhedron"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = 1
        case (VTK_PARAMETRIC_CURVE)
            self%cell_type_name = "ParametricCurve"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 1
            self%cell_order = -1 ! 可変
        case (VTK_PARAMETRIC_SURFACE)
            self%cell_type_name = "ParametricSurface"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = -1 ! 可変
        case (VTK_PARAMETRIC_TRI_SURFACE)
            self%cell_type_name = "ParametricTriSurface"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = -1 ! 可変
        case (VTK_PARAMETRIC_QUAD_SURFACE)
            self%cell_type_name = "ParametricQuadSurface"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = -1 ! 可変
        case (VTK_PARAMETRIC_TETRA_REGION)
            self%cell_type_name = "ParametricTetraRegion"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = -1 ! 可変
        case (VTK_PARAMETRIC_HEX_REGION)
            self%cell_type_name = "ParametricHexRegion"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = -1 ! 可変
        case (VTK_HIGHER_ORDER_EDGE)
            self%cell_type_name = "HigherOrderEdge"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 1
            self%cell_order = -1 ! 可変
        case (VTK_HIGHER_ORDER_TRIANGLE)
            self%cell_type_name = "HigherOrderTriangle"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = -1 ! 可変
        case (VTK_HIGHER_ORDER_QUAD)
            self%cell_type_name = "HigherOrderQuad"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = -1 ! 可変
        case (VTK_HIGHER_ORDER_POLYGON)
            self%cell_type_name = "HigherOrderPolygon"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = -1 ! 可変
        case (VTK_HIGHER_ORDER_TETRAHEDRON)
            self%cell_type_name = "HigherOrderTetrahedron"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = -1 ! 可変
        case (VTK_HIGHER_ORDER_WEDGE)
            self%cell_type_name = "HigherOrderWedge"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = -1 ! 可変
        case (VTK_HIGHER_ORDER_PYRAMID)
            self%cell_type_name = "HigherOrderPyramid"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = -1 ! 可変
        case (VTK_HIGHER_ORDER_HEXAHEDRON)
            self%cell_type_name = "HigherOrderHexahedron"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = -1 ! 可変
        case (VTK_LAGRANGE_CURVE)
            self%cell_type_name = "LagrangeCurve"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 1
            self%cell_order = -1 ! 可変
        case (VTK_LAGRANGE_TRIANGLE)
            self%cell_type_name = "LagrangeTriangle"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = -1 ! 可変
        case (VTK_LAGRANGE_QUADRILATERAL)
            self%cell_type_name = "LagrangeQuadrilateral"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = -1 ! 可変
        case (VTK_LAGRANGE_TETRAHEDRON)
            self%cell_type_name = "LagrangeTetrahedron"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = -1 ! 可変
        case (VTK_LAGRANGE_HEXAHEDRON)
            self%cell_type_name = "LagrangeHexahedron"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = -1 ! 可変
        case (VTK_LAGRANGE_WEDGE)
            self%cell_type_name = "LagrangeWedge"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = -1 ! 可変
        case (VTK_LAGRANGE_PYRAMID)
            self%cell_type_name = "LagrangePyramid"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = -1 ! 可変
        case (VTK_BEZIER_CURVE)
            self%cell_type_name = "BezierCurve"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 1
            self%cell_order = -1 ! 可変
        case (VTK_BEZIER_TRIANGLE)
            self%cell_type_name = "BezierTriangle"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = -1 ! 可変
        case (VTK_BEZIER_QUADRILATERAL)
            self%cell_type_name = "BezierQuadrilateral"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 2
            self%cell_order = -1 ! 可変
        case (VTK_BEZIER_TETRAHEDRON)
            self%cell_type_name = "BezierTetrahedron"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = -1 ! 可変
        case (VTK_BEZIER_HEXAHEDRON)
            self%cell_type_name = "BezierHexahedron"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = -1 ! 可変
        case (VTK_BEZIER_WEDGE)
            self%cell_type_name = "BezierWedge"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = -1 ! 可変
        case (VTK_BEZIER_PYRAMID)
            self%cell_type_name = "BezierPyramid"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 3
            self%cell_order = -1 ! 可変
        case default
            self%cell_type_name = "Unknown"
            self%num_nodes_in_cell = num_nodes_in_cell
            self%cell_dimension = 0
            self%cell_order = 0
        end select
    end subroutine type_vtk_cell_set

    function type_vtk_cell_get_dimension(self) result(dimension)
        implicit none
        class(type_vtk_cell), intent(in) :: self !! VTK cell data
        integer(int32) :: dimension
        !> Get the dimension of the cell
        dimension = self%cell_dimension
        if (dimension < 0) then
            ! 可変次元のセルの場合、セルのノード数から計算
            select case (self%cell_type)
            case (VTK_POLY_VERTEX, VTK_POLY_LINE, VTK_POLYGON, &
                  VTK_QUADRATIC_POLYGON, VTK_QUADRATIC_TRIANGLE, &
                  VTK_QUADRATIC_QUAD, VTK_QUADRATIC_TETRA, &
                  VTK_QUADRATIC_HEXAHEDRON, VTK_QUADRATIC_WEDGE, &
                  VTK_QUADRATIC_PYRAMID, VTK_BIQUADRATIC_QUAD, &
                  VTK_TRIQUADRATIC_HEXAHEDRON, VTK_TRIQUADRATIC_PYRAMID, &
                  VTK_QUADRATIC_LINEAR_QUAD, VTK_QUADRATIC_LINEAR_WEDGE, &
                  VTK_BIQUADRATIC_QUADRATIC_WEDGE, &
                  VTK_BIQUADRATIC_QUADRATIC_HEXAHEDRON, &
                  VTK_BIQUADRATIC_TRIANGLE, VTK_CUBIC_LINE, &
                  VTK_CONVEX_POINT_SET, VTK_POLYHEDRON, &
                  VTK_PARAMETRIC_CURVE, VTK_PARAMETRIC_SURFACE, &
                  VTK_PARAMETRIC_TRI_SURFACE, VTK_PARAMETRIC_QUAD_SURFACE, &
                  VTK_PARAMETRIC_TETRA_REGION, VTK_PARAMETRIC_HEX_REGION, &
                  VTK_HIGHER_ORDER_EDGE, VTK_HIGHER_ORDER_TRIANGLE, &
                  VTK_HIGHER_ORDER_QUAD, VTK_HIGHER_ORDER_POLYGON, &
                  VTK_HIGHER_ORDER_TETRAHEDRON, VTK_HIGHER_ORDER_WEDGE, &
                  VTK_HIGHER_ORDER_PYRAMID, VTK_HIGHER_ORDER_HEXAHEDRON)
                ! 可変次元のセルは、セルのノード数から計算
                if (self%num_nodes_in_cell > 0) then
                    dimension = 1
                else
                    dimension = -1
                end if
            case default
                ! それ以外のセルは、セルのノード数から計算
                if (self%num_nodes_in_cell > 0) then
                    dimension = 3
                else
                    dimension = -1
                end if
            end select
        end if
    end function type_vtk_cell_get_dimension

    function type_vtk_cell_get_order(self) result(order)
        !> Get the order of the cell
        implicit none
        class(type_vtk_cell), intent(in) :: self !! VTK cell data
        integer(int32) :: order

        order = self%cell_order

        if (order < 0) then
            ! 可変次元のセルの場合、セルのノード数から計算
            select case (self%cell_type)
            case (VTK_POLY_VERTEX, VTK_POLY_LINE, VTK_POLYGON, &
                  VTK_QUADRATIC_POLYGON, VTK_QUADRATIC_TRIANGLE, &
                  VTK_QUADRATIC_QUAD, VTK_QUADRATIC_TETRA, &
                  VTK_QUADRATIC_HEXAHEDRON, VTK_QUADRATIC_WEDGE, &
                  VTK_QUADRATIC_PYRAMID, VTK_BIQUADRATIC_QUAD, &
                  VTK_TRIQUADRATIC_HEXAHEDRON, VTK_TRIQUADRATIC_PYRAMID, &
                  VTK_QUADRATIC_LINEAR_QUAD, VTK_QUADRATIC_LINEAR_WEDGE, &
                  VTK_BIQUADRATIC_QUADRATIC_WEDGE, &
                  VTK_BIQUADRATIC_QUADRATIC_HEXAHEDRON, &
                  VTK_BIQUADRATIC_TRIANGLE, VTK_CUBIC_LINE, &
                  VTK_CONVEX_POINT_SET, VTK_POLYHEDRON, &
                  VTK_PARAMETRIC_CURVE, VTK_PARAMETRIC_SURFACE, &
                  VTK_PARAMETRIC_TRI_SURFACE, VTK_PARAMETRIC_QUAD_SURFACE, &
                  VTK_PARAMETRIC_TETRA_REGION, VTK_PARAMETRIC_HEX_REGION, &
                  VTK_HIGHER_ORDER_EDGE, VTK_HIGHER_ORDER_TRIANGLE, &
                  VTK_HIGHER_ORDER_QUAD, VTK_HIGHER_ORDER_POLYGON, &
                  VTK_HIGHER_ORDER_TETRAHEDRON, VTK_HIGHER_ORDER_WEDGE, &
                  VTK_HIGHER_ORDER_PYRAMID, VTK_HIGHER_ORDER_HEXAHEDRON, &
                  VTK_LAGRANGE_CURVE, VTK_LAGRANGE_TRIANGLE, &
                  VTK_LAGRANGE_QUADRILATERAL, VTK_LAGRANGE_TETRAHEDRON, &
                  VTK_LAGRANGE_HEXAHEDRON, VTK_LAGRANGE_WEDGE, &
                  VTK_LAGRANGE_PYRAMID, &
                  VTK_BEZIER_CURVE, VTK_BEZIER_TRIANGLE, &
                  VTK_BEZIER_QUADRILATERAL, VTK_BEZIER_TETRAHEDRON, &
                  VTK_BEZIER_HEXAHEDRON, VTK_BEZIER_WEDGE, &
                  VTK_BEZIER_PYRAMID)
                ! 可変次元のセルは、セルのノード数から計算
                if (self%num_nodes_in_cell > 0) then
                    order = self%num_nodes_in_cell
                else
                    order = -1
                end if
            case default
                ! それ以外のセルは、セルのノード数から計算
                if (self%num_nodes_in_cell > 0) then
                    order = self%num_nodes_in_cell
                else
                    order = -1
                end if
            end select
        end if

    end function type_vtk_cell_get_order

    subroutine get_active_region_info(self, unique_ids, ierr)
        ! --- 引数 ---
        implicit none
        class(Type_VTK), intent(in) :: self !! VTK data
        integer(int32), allocatable, intent(out) :: unique_ids(:)
        integer(int32), intent(out) :: ierr

        ! --- ローカル変数 ---
        integer(int32) :: local_max_dim
        integer(int32), allocatable :: collected_ids(:)
        integer(int32) :: i_cell, count
        logical(4) :: is_max_dim_element

#ifdef _MPI
        integer(int32) :: global_max_dim
        integer(int32), allocatable :: all_counts(:), displs(:)
        integer(int32), allocatable :: global_collected_ids(:)
        integer(int32) :: total_collected_count, j
#endif

        local_max_dim = 0
        ierr = 0

        ! --- ステップ1: メッシュ内の最大次元を判定 ---
        do i_cell = 1, self%num_total_cells
            select case (self%CELLS(i_cell)%cell_type)
            case (VTK_TETRA, VTK_HEXAHEDRON, &
                  VTK_WEDGE, VTK_PYRAMID, &
                  VTK_QUADRATIC_TETRA, VTK_QUADRATIC_HEXAHEDRON)
                local_max_dim = 3
                exit ! 3Dが見つかったら、それ以上探す必要はない
            case (VTK_TRIANGLE, VTK_PIXEL, &
                  VTK_QUAD, VTK_QUADRATIC_TRIANGLE, &
                  VTK_QUADRATIC_QUAD)
                local_max_dim = max(local_max_dim, 2)
            case (VTK_LINE, VTK_QUADRATIC_EDGE)
                local_max_dim = max(local_max_dim, 1)
            end select
        end do

#ifdef _MPI
        ! MPI: 全プロセスの最大次元を求め、グローバルな最大次元を決定
        call MPI_Allreduce(local_max_dim, global_max_dim, 1, MPI_INTEGER4, MPI_MAX, MPI_COMM_WORLD, ierr)
        if (global_max_dim == 0) then
            ierr = -1
            allocate (unique_ids(0))
            return
        end if
#else
        if (local_max_dim == 0) then
            ierr = -1
            allocate (unique_ids(0))
            return ! アクティブな要素がない
        end if
#endif

        ! --- ステップ2: 最大次元を持つ要素から、すべてのCellEntityIdを収集 ---
        allocate (collected_ids(self%num_total_cells))
        count = 0
        do i_cell = 1, self%num_total_cells
            is_max_dim_element = .false.
            select case (self%CELLS(i_cell)%cell_type)
            case (VTK_TETRA, VTK_HEXAHEDRON, &
                  VTK_WEDGE, VTK_PYRAMID, &
                  VTK_QUADRATIC_TETRA, VTK_QUADRATIC_HEXAHEDRON)
#ifdef _MPI
                if (global_max_dim == 3) is_max_dim_element = .true.
#else
                if (local_max_dim == 3) is_max_dim_element = .true.
#endif
            case (VTK_TRIANGLE, VTK_PIXEL, &
                  VTK_QUAD, VTK_QUADRATIC_TRIANGLE, &
                  VTK_QUADRATIC_QUAD)
#ifdef _MPI
                if (global_max_dim == 2) is_max_dim_element = .true.
#else
                if (local_max_dim == 2) is_max_dim_element = .true.
#endif
            case (VTK_LINE, VTK_QUADRATIC_EDGE)
#ifdef _MPI
                if (global_max_dim == 1) is_max_dim_element = .true.
#else
                if (local_max_dim == 1) is_max_dim_element = .true.
#endif
            end select

            if (is_max_dim_element) then
                count = count + 1
                collected_ids(count) = self%CELLS(i_cell)%cell_entity_id
            end if
        end do

        ! --- ステップ3: 収集したIDリストから、ユニークなものだけを抽出 ---
#ifdef _MPI
        ! MPI: 全プロセスのIDを収集し、グローバルにユニークなものを抽出
        allocate (all_counts(self%num_procs))
        allocate (displs(self%num_procs))
        call MPI_Allgather(count, 1, MPI_INTEGER4, all_counts, 1, MPI_INTEGER4, MPI_COMM_WORLD, ierr)

        total_collected_count = sum(all_counts)
        if (total_collected_count > 0) then
            displs(1) = 0
            do j = 2, self%num_procs
                displs(j) = displs(j - 1) + all_counts(j - 1)
            end do
            allocate (global_collected_ids(total_collected_count))
            call MPI_Allgatherv(collected_ids(1:count), count, MPI_INTEGER4, &
                                global_collected_ids, all_counts, displs, MPI_INTEGER4, MPI_COMM_WORLD, ierr)
            call unique(global_collected_ids, unique_ids)
            deallocate (global_collected_ids)
        else
            allocate (unique_ids(0))
        end if
        deallocate (all_counts, displs)
#else
        if (count > 0) then
            call unique(collected_ids(1:count), unique_ids)
        else
            allocate (unique_ids(0))
        end if
#endif
        deallocate (collected_ids)

    end subroutine get_active_region_info

    subroutine finalize_vtk_object(self)
        type(type_vtk), intent(inout) :: self

        if (c_associated(self%handle)) then

            select case (trim(adjustl(self%reader_type)))
            case ("vtk")
                call vtk_finalize(self%handle)
            case ("vtu")
                call vtu_finalize(self%handle)
            case default
                ! 知らないリーダータイプの場合は何もしない
            end select

            self%handle = c_null_ptr

        end if
    end subroutine finalize_vtk_object
end module core_vtk

