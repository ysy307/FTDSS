module domain_manager
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: mpi_f08
    use :: stdlib_logger
    use :: stdlib_sorting, only:sort_index
    use :: module_core, only:allocate_array, deallocate_array
    use :: module_input, only:type_input
    use :: module_properties
    use :: domain_multicoloring, only:type_coloring
    implicit none
    private

    public :: type_domain

    ! --- データ構造の定義 (変更なし) ---
    type :: type_fe_connectivity
        integer(int32), allocatable :: ind(:)
        integer(int32), allocatable :: val(:)
    end type type_fe_connectivity

    ! --- 新設：単一の境界セットを管理する型 ---
    type :: type_boundary_set
        integer(int32) :: id = 0
        integer(int32) :: num_elements = 0
        integer(int32), allocatable :: element_types(:)
        type(type_fe_connectivity) :: connectivity
    end type type_boundary_set

    type :: type_domain
        ! --- Basic Information ---
        integer(int32) :: my_rank = -1
        integer(int32) :: num_procs = -1
        integer(int32), private :: computation_dimension
        integer(int32), private :: computation_type

        ! --- Node information (Node Data) ---
        integer(int32) :: num_nodes = 0
        real(real64), allocatable :: coordinates(:, :)
        integer(int32), allocatable :: node_global_ids(:)

        ! --- Domain element information (SoA) ---
        integer(int32) :: num_elements = 0
        integer(int32) :: num_materials = 0
        integer(int32), allocatable :: fe_types(:)
        integer(int32), allocatable :: fe_material_ids(:)
        type(type_fe_connectivity) :: connectivity

        ! --- Boundary Elements (2D => Edges, 3D => Faces) ---
        integer(int32) :: num_boundary_sets = 0 ! 境界セットの種類数を保持
        type(type_boundary_set), allocatable :: boundary_sets(:)

        ! --- Coloring information for threading ---
        type(type_coloring) :: colors

        ! ... (その他のメンバ) ...
    contains
        ! --- 公開する手続き ---
        procedure, public, pass(self) :: initialize => initialize_type_domain

        ! --- 内部で使う専用サブルーチン ---
        procedure, private, pass(self) :: set_basic_info
        procedure, private, pass(self) :: set_node_data
        procedure, private, pass(self) :: set_domain_elements
        procedure, private, pass(self) :: set_boundary_elements
        ! procedure, private, pass(self) :: set_mpi_schedule      ! TBI
    end type type_domain

contains

    ! ==========================================================
    ! 司令塔となるメインの初期化サブルーチン
    ! ==========================================================
    subroutine initialize_type_domain(self, input)
        class(type_domain), intent(inout) :: self
        type(type_input), intent(in) :: input

        ! 1. 基本情報を設定
        call self%set_basic_info(input)

        ! 2. 節点データを構築
        call self%set_node_data(input)

        ! 3. 計算領域の要素データを構築
        call self%set_domain_elements(input)

        ! 4. カラーリング情報を構築
        call self%colors%initialize(input)

        ! 5. 境界要素データを構築 (将来実装)
        call self%set_boundary_elements(input)

        ! 6. MPI通信スケジュールを構築 (将来実装)
        ! call self%set_mpi_schedule(input)

    end subroutine initialize_type_domain

    ! ==========================================================
    ! 1. 基本情報を設定する専用サブルーチン
    ! ==========================================================
    subroutine set_basic_info(self, input)
        class(type_domain), intent(inout) :: self
        type(type_input), intent(in) :: input

        call MPI_Comm_rank(MPI_COMM_WORLD, self%my_rank)
        call MPI_Comm_size(MPI_COMM_WORLD, self%num_procs)
        self%computation_dimension = input%basic%simulation_settings%calculate_dimension !&
        self%computation_type      = input%basic%simulation_settings%calculate_type !&
        self%num_nodes             = input%geometry%vtk%num_points !&
        self%num_materials         = input%basic%num_materials !&
    end subroutine set_basic_info

    ! ==========================================================
    ! 2. 節点データを構築する専用サブルーチン
    ! ==========================================================
    subroutine set_node_data(self, input)
        class(type_domain), intent(inout) :: self
        type(type_input), intent(in) :: input

        allocate (self%coordinates(self%computation_dimension, self%num_nodes))
        select case (self%computation_type)
        case (1) ! 2D (XY-plane)
            self%coordinates(1, :) = input%geometry%vtk%points%x(1:self%num_nodes)
            self%coordinates(2, :) = input%geometry%vtk%points%y(1:self%num_nodes)
        case (2) ! 2D (XZ-plane)
            self%coordinates(1, :) = input%geometry%vtk%points%x(1:self%num_nodes)
            self%coordinates(2, :) = input%geometry%vtk%points%z(1:self%num_nodes)
        case (3) ! 3D
            self%coordinates(1, :) = input%geometry%vtk%points%x(1:self%num_nodes)
            self%coordinates(2, :) = input%geometry%vtk%points%y(1:self%num_nodes)
            self%coordinates(3, :) = input%geometry%vtk%points%z(1:self%num_nodes)
        end select

        allocate (self%node_global_ids(self%num_nodes))
        self%node_global_ids(:) = input%geometry%vtk%global_node_ids(1:self%num_nodes)
    end subroutine set_node_data

    ! ==========================================================
    ! 3. 計算領域の要素データを構築する専用サブルーチン
    ! ==========================================================
    subroutine set_domain_elements(self, input)
        class(type_domain), intent(inout) :: self
        type(type_input), intent(in) :: input
        integer(int32) :: i, ind, cell_dimension
        integer(int32) :: num_total_cells, num_total_connectivity

        num_total_cells = input%geometry%vtk%num_total_cells

        ! --- パス1：計測 ---
        self%num_elements = 0
        num_total_connectivity = 0
        do i = 1, num_total_cells
            cell_dimension = input%geometry%vtk%cells(i)%get_dimension()
            if (cell_dimension == self%computation_dimension) then
                self%num_elements = self%num_elements + 1
                num_total_connectivity = num_total_connectivity + input%geometry%vtk%cells(i)%num_nodes_in_cell
            end if
        end do

        ! --- メモリ確保 ---
        allocate (self%fe_types(self%num_elements))
        allocate (self%fe_material_ids(self%num_elements))
        allocate (self%connectivity%ind(self%num_elements + 1))
        allocate (self%connectivity%val(num_total_connectivity))

        ! --- パス2：格納 ---
        self%connectivity%ind(1) = 1
        ind = 0
        do i = 1, num_total_cells
            cell_dimension = input%geometry%vtk%cells(i)%get_dimension()
            if (cell_dimension == self%computation_dimension) then
                ind = ind + 1
                self%fe_types(ind) = input%geometry%vtk%cells(i)%cell_type
                self%fe_material_ids(ind) = input%geometry%vtk%cells(i)%cell_entity_id
                self%connectivity%ind(ind + 1) = self%connectivity%ind(ind) + input%geometry%vtk%cells(i)%num_nodes_in_cell
                self%connectivity%val(self%connectivity%ind(ind):self%connectivity%ind(ind + 1) - 1) = &
                    input%geometry%vtk%cells(i)%connectivity(1:input%geometry%vtk%cells(i)%num_nodes_in_cell)
            end if
        end do
    end subroutine set_domain_elements

    ! ==========================================================
    ! 5. 境界セットごとにデータを構築する専用サブルーチン
    ! ==========================================================
    subroutine set_boundary_elements(self, input)
        implicit none
        ! --- モジュールと引数の宣言 ---
        class(type_domain), intent(inout) :: self
        type(type_input), intent(in) :: input

        ! --- 変数宣言ブロック ---

        integer(int32) :: i, j, be_idx
        integer(int32) :: cell_dimension
        integer(int32) :: num_total_cells

        integer(int32) :: current_set_id
        integer(int32) :: count_elements_in_set
        integer(int32) :: conn_size_in_set

        ! --- 実行ブロック ---

        ! 境界条件の定義数を取得 (これがユニークなセット数になる)
        self%num_boundary_sets = size(input%conditions%boundary_conditions)
        if (self%num_boundary_sets == 0) return

        num_total_cells = input%geometry%vtk%num_total_cells

        ! メインのboundary_sets配列を確保
        allocate (self%boundary_sets(self%num_boundary_sets))

        ! ===================================================================
        ! ステップ1：定義された各境界条件についてループ
        ! ===================================================================
        do i = 1, self%num_boundary_sets
            current_set_id = input%conditions%boundary_conditions(i)%id
            self%boundary_sets(i)%id = current_set_id

            ! --- (a) このセットに属する要素数とコネクティビティサイズを計測 ---
            count_elements_in_set = 0
            conn_size_in_set = 0
            do j = 1, num_total_cells
                cell_dimension = input%geometry%vtk%cells(j)%get_dimension()
                if (cell_dimension == self%computation_dimension - 1 .and. &
                    input%geometry%vtk%cells(j)%cell_entity_id == current_set_id) then
                    count_elements_in_set = count_elements_in_set + 1
                    conn_size_in_set = conn_size_in_set + input%geometry%vtk%cells(j)%num_nodes_in_cell
                end if
            end do

            ! --- (b) このセットの内部配列を確保 ---
            self%boundary_sets(i)%num_elements = count_elements_in_set
            if (count_elements_in_set > 0) then
                allocate (self%boundary_sets(i)%element_types(count_elements_in_set))
                allocate (self%boundary_sets(i)%connectivity%ind(count_elements_in_set + 1))
                allocate (self%boundary_sets(i)%connectivity%val(conn_size_in_set))
            end if
        end do

        ! ===================================================================
        ! ステップ2：再度ループし、各セットの配列にデータを格納
        ! ===================================================================
        do i = 1, self%num_boundary_sets
            current_set_id = self%boundary_sets(i)%id
            if (self%boundary_sets(i)%num_elements == 0) cycle

            self%boundary_sets(i)%connectivity%ind(1) = 1
            be_idx = 0
            do j = 1, num_total_cells
                cell_dimension = input%geometry%vtk%cells(j)%get_dimension()
                if (cell_dimension == self%computation_dimension - 1 .and. &
                    input%geometry%vtk%cells(j)%cell_entity_id == current_set_id) then
                    be_idx = be_idx + 1
                    self%boundary_sets(i)%element_types(be_idx) = input%geometry%vtk%cells(j)%cell_type

                    self%boundary_sets(i)%connectivity%ind(be_idx + 1) = &
                        self%boundary_sets(i)%connectivity%ind(be_idx) + input%geometry%vtk%cells(j)%num_nodes_in_cell

                    self%boundary_sets(i)%connectivity%val(self%boundary_sets(i)%connectivity%ind(be_idx): &
                                                           self%boundary_sets(i)%connectivity%ind(be_idx + 1) - 1) = &
                        input%geometry%vtk%cells(j)%connectivity(1:input%geometry%vtk%cells(j)%num_nodes_in_cell)
                end if
            end do
        end do

    end subroutine set_boundary_elements

end module domain_manager
