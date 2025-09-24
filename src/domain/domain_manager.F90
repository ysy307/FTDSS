!>
!>  @brief Manager for computation domain and related data
!>
module domain_manager
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: mpi_f08
    use :: stdlib_logger
    use :: module_core
    use :: module_input, only:type_input
    use :: domain_multicoloring, only:type_coloring
    use :: module_fe, only:type_fe_manager
    ! use :: conditions_boundary, only:abst_bc, construct_type_bc_thermal_dirichlet, &
    !     construct_type_bc_thermal_adiabatic ! 他のconstruct_*も同様にUSE

    implicit none
    private

    public :: type_domain

    ! ==========================================================
    ! コンポーネントの型定義
    ! ==========================================================

    ! --- CSR形式コネクティビティ ---
    type :: type_fe_connectivity
        integer(int32), allocatable :: ind(:)
        integer(int32), allocatable :: val(:)
    end type type_fe_connectivity

    ! --- 単一の物理条件＋ジオメトリ＋振る舞い ---
    type :: type_single_bc
        ! class(abst_bc), allocatable :: bc_model
        integer(int32) :: num_elements = 0
        integer(int32), allocatable :: element_types(:)
        type(type_fe_connectivity) :: connectivity
    end type type_single_bc

    ! --- 単一物理のBCマネージャ ---
    type :: type_physics_bc_manager
        integer(int32) :: num_bcs = 0
        type(type_single_bc), allocatable :: bcs(:)
    end type type_physics_bc_manager

    ! --- トップレベル境界マネージャ ---
    type :: type_boundary_manager
        type(type_domain), pointer, private :: parent => null()
        type(type_physics_bc_manager) :: physics(NUM_PHYSICS_TYPES)
    contains
        procedure, public, pass(self) :: initialize => initialize_boundary_manager
        procedure, private, pass(self) :: process_single_physics_bcs
    end type type_boundary_manager

    ! --- 自由度マップ ---
    type :: type_dof_map
        integer(int32) :: num_dof_per_node = 0
        integer(int32) :: num_dof_of_physics(NUM_PHYSICS_TYPES) = 0
        integer(int32) :: start_dof_index(NUM_PHYSICS_TYPES) = 0
    end type type_dof_map

    ! --- 節点データマネージャ ---
    type :: type_node_manager
        type(type_domain), pointer, private :: parent => null()
        integer(int32) :: num_nodes = 0
        real(real64), allocatable :: coordinates(:, :)
        integer(int32), allocatable :: node_global_ids(:)
    contains
        procedure, public, pass(self) :: initialize => initialize_node_manager
    end type type_node_manager

    ! --- 要素データマネージャ ---
    type :: type_element_manager
        type(type_domain), pointer, private :: parent => null()
        integer(int32) :: num_elements = 0
        integer(int32), allocatable :: fe_types(:)
        integer(int32), allocatable :: fe_material_ids(:)
        type(type_fe_manager) :: fe_manager
        type(type_fe_connectivity) :: connectivity
        type(type_coloring) :: colors
    contains
        procedure, public, pass(self) :: initialize => initialize_element_manager
    end type type_element_manager

    ! ==========================================================
    ! 最上位のコンテナとなるdomain型
    ! ==========================================================
    type :: type_domain
        integer(int32) :: my_rank = -1
        integer(int32) :: num_procs = -1
        integer(int32) :: computation_dimension
        integer(int32), private :: computation_type
        type(type_dof_map) :: dof_map
        type(type_node_manager) :: nodes
        type(type_element_manager) :: elements
        type(type_boundary_manager) :: boundaries
    contains
        procedure, public, pass(self) :: initialize => initialize_type_domain
        procedure, private, pass(self) :: associate_parent
        procedure, private, pass(self) :: set_basic_info_and_dof_map
    end type type_domain

contains

    ! ==========================================================
    ! メインの初期化サブルーチン (司令塔)
    ! ==========================================================
    subroutine initialize_type_domain(self, input)
        implicit none
        class(type_domain), intent(inout) :: self
        type(type_input), intent(in) :: input

        call self%associate_parent(self%nodes, self%elements, self%boundaries)
        call self%set_basic_info_and_dof_map(input)

        call self%nodes%initialize(input)
        call self%elements%initialize(input)
        call self%boundaries%initialize(input)
    end subroutine initialize_type_domain

    subroutine associate_parent(self, node, element, boundary)
        implicit none
        class(type_domain), intent(inout), target :: self
        class(type_node_manager), intent(inout) :: node
        class(type_element_manager), intent(inout) :: element
        class(type_boundary_manager), intent(inout) :: boundary

        node%parent => self
        element%parent => self
        boundary%parent => self
    end subroutine associate_parent

    ! ==========================================================
    ! ヘルパー：基本情報とDOFマップを設定
    ! ==========================================================
    subroutine set_basic_info_and_dof_map(self, input)
        implicit none
        class(type_domain), intent(inout) :: self
        type(type_input), intent(in) :: input
        integer(int32) :: current_dof_index

        call MPI_Comm_rank(MPI_COMM_WORLD, self%my_rank)
        call MPI_Comm_size(MPI_COMM_WORLD, self%num_procs)
        self%computation_dimension = input%basic%simulation_settings%calculate_dimension
        self%computation_type = input%basic%simulation_settings%calculate_type

        self%dof_map%num_dof_of_physics(PHYSICS_TYPE_THERMAL) = 1
        self%dof_map%num_dof_of_physics(PHYSICS_TYPE_HYDRAULIC) = 1
        self%dof_map%num_dof_of_physics(PHYSICS_TYPE_MECHANICAL) = self%computation_dimension

        current_dof_index = 1
        if (input%basic%analysis_controls%calculate_thermal) then
            self%dof_map%start_dof_index(PHYSICS_TYPE_THERMAL) = current_dof_index
            current_dof_index = current_dof_index + 1
        end if
        if (input%basic%analysis_controls%calculate_hydraulic) then
            self%dof_map%start_dof_index(PHYSICS_TYPE_HYDRAULIC) = current_dof_index
            current_dof_index = current_dof_index + 1
        end if
        if (input%basic%analysis_controls%calculate_mechanical) then
            self%dof_map%start_dof_index(PHYSICS_TYPE_MECHANICAL) = current_dof_index
            current_dof_index = current_dof_index + self%computation_dimension
        end if
        self%dof_map%num_dof_per_node = current_dof_index - 1
    end subroutine set_basic_info_and_dof_map

    ! ==========================================================
    ! Node Managerの初期化
    ! ==========================================================
    subroutine initialize_node_manager(self, input)
        implicit none
        class(type_node_manager), intent(inout) :: self
        type(type_input), intent(in) :: input

        self%num_nodes = input%geometry%vtk%num_points

        allocate (self%coordinates(self%parent%computation_dimension, self%num_nodes))
        select case (self%parent%computation_type)
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

        call allocate_array(self%node_global_ids, self%num_nodes)
        self%node_global_ids(:) = input%geometry%vtk%global_node_ids(1:self%num_nodes)
    end subroutine initialize_node_manager

    ! ==========================================================
    ! Element Managerの初期化
    ! ==========================================================
    subroutine initialize_element_manager(self, input)
        implicit none
        class(type_element_manager), intent(inout) :: self
        type(type_input), intent(in) :: input
        integer(int32) :: i, ind, cell_dimension, num_total_cells, num_total_connectivity
        integer(int32), allocatable :: unique_fe_types(:)

        num_total_cells = input%geometry%vtk%num_total_cells

        ! パス1：計測
        self%num_elements = 0
        num_total_connectivity = 0
        do i = 1, num_total_cells
            if (input%geometry%vtk%cells(i)%get_dimension() == self%parent%computation_dimension) then
                self%num_elements = self%num_elements + 1
                num_total_connectivity = num_total_connectivity + input%geometry%vtk%cells(i)%num_nodes_in_cell
            end if
        end do

        if (self%num_elements > 0) then
            allocate (self%fe_types(self%num_elements))
            allocate (self%fe_material_ids(self%num_elements))
            allocate (self%connectivity%ind(self%num_elements + 1))
            allocate (self%connectivity%val(num_total_connectivity))
        end if

        ! パス2：格納
        if (self%num_elements > 0) then
            self%connectivity%ind(1) = 1
            ind = 0
            do i = 1, num_total_cells
                if (input%geometry%vtk%cells(i)%get_dimension() == self%parent%computation_dimension) then
                    ind = ind + 1
                    self%fe_types(ind) = input%geometry%vtk%cells(i)%cell_type
                    self%fe_material_ids(ind) = input%geometry%vtk%cells(i)%cell_entity_id
                    self%connectivity%ind(ind + 1) = self%connectivity%ind(ind) + input%geometry%vtk%cells(i)%num_nodes_in_cell
                    self%connectivity%val(self%connectivity%ind(ind):self%connectivity%ind(ind + 1) - 1) = &
                        input%geometry%vtk%cells(i)%connectivity(1:input%geometry%vtk%cells(i)%num_nodes_in_cell)
                end if
            end do
        end if

        ! FEマネージャを初期化
        call unique(self%fe_types, unique_fe_types)
        print *, unique_fe_types
        call self%fe_manager%initialize(input, self%num_elements, self%fe_types)

        ! カラーリング情報を構築
        call self%colors%initialize(input)
    end subroutine initialize_element_manager

    ! ==========================================================
    ! Boundary Managerの初期化
    ! ==========================================================
    subroutine initialize_boundary_manager(self, input)
        class(type_boundary_manager), intent(inout) :: self
        type(type_input), intent(in) :: input

        if (input%basic%analysis_controls%calculate_thermal) then
            call self%process_single_physics_bcs(PHYSICS_TYPE_THERMAL, input)
        end if
        if (input%basic%analysis_controls%calculate_hydraulic) then
            call self%process_single_physics_bcs(PHYSICS_TYPE_HYDRAULIC, input)
        end if
        if (input%basic%analysis_controls%calculate_mechanical) then
            call self%process_single_physics_bcs(PHYSICS_TYPE_MECHANICAL, input)
        end if

    end subroutine initialize_boundary_manager

    ! ==========================================================
    ! Boundary Managerのヘルパーサブルーチン
    ! ==========================================================
    subroutine process_single_physics_bcs(self, physics_type_id, input)
        class(type_boundary_manager), intent(inout) :: self
        integer(int32), intent(in) :: physics_type_id
        type(type_input), intent(in) :: input

        ! --- ローカル変数 ---
        integer(int32) :: i, bc_type, num_groups
        integer(int32) :: max_id, bc_id, group_idx
        integer(int32) :: target_dimension
        integer(int32), allocatable :: bc_sequence(:), bc_idx_list(:), bc_key(:), active_region_id(:)
        integer(int32), allocatable :: input_idx_to_group_idx_map(:)
        integer(int32), allocatable :: entity_id_to_group_idx_map(:)
        integer(int32), allocatable :: total_conn_per_group(:), current_elem_indices(:)
        integer(int32) :: num_total_cells, cell_id, current_group_idx, num_nodes

        ! 対象次元チェック
        target_dimension = self%parent%computation_dimension - 1
        if (target_dimension < 1) return

        ! --- 物理種別ごとに BC_SEQUENCE を選択
        select case (physics_type_id)
        case (PHYSICS_TYPE_THERMAL)
            allocate (bc_sequence, source=THERMAL_BC_SEQUENCE)
        case (PHYSICS_TYPE_HYDRAULIC)
            allocate (bc_sequence, source=HYDRAULIC_BC_SEQUENCE)
        case (PHYSICS_TYPE_MECHANICAL)
            return ! 未実装
        end select

        ! --- 有効な境界条件の index を収集 ---
        call input%geometry%vtk%get_active_region_info(active_region_id, target_dimension)

        ! 条件に合う境界条件 index を一時配列に集める
        allocate (bc_idx_list(0))
        do i = 1, input%conditions%num_boundaries
            select case (physics_type_id)
            case (PHYSICS_TYPE_THERMAL)
                if (input%conditions%boundary_conditions(i)%calculate_thermal .and. &
                    any(active_region_id == input%conditions%boundary_conditions(i)%id)) then
                    bc_idx_list = [bc_idx_list, i]
                end if
            case (PHYSICS_TYPE_HYDRAULIC)
                if (input%conditions%boundary_conditions(i)%calculate_hydraulic .and. &
                    any(active_region_id == input%conditions%boundary_conditions(i)%id)) then
                    bc_idx_list = [bc_idx_list, i]
                end if
            end select
        end do

        ! --- 並び替え用キー配列作成 ---
        call allocate_array(bc_key, size(bc_idx_list))
        do i = 1, size(bc_idx_list)
            select case (physics_type_id)
            case (PHYSICS_TYPE_THERMAL)
                bc_type = get_bc_type_from_string( &
                          input%conditions%boundary_conditions(bc_idx_list(i))%thermal%type, &
                          physics_type_id)
            case (PHYSICS_TYPE_HYDRAULIC)
                bc_type = get_bc_type_from_string( &
                          input%conditions%boundary_conditions(bc_idx_list(i))%hydraulic%type, &
                          physics_type_id)
            end select
            bc_key(i) = get_bc_seq_pos(bc_type, bc_sequence)
        end do

        ! --- bc_key をキーにソート ---
        call sort_by_key(bc_idx_list, bc_key)

        if (size(bc_idx_list) == 0) then
            self%physics(physics_type_id)%num_bcs = 0
            return
        end if

        ! --- ステップA: グルーピングと最終的なBC数の決定 ---
        ! ソート済みリストを走査し、隣接要素を比較してグループ数を数える
        call allocate_array(input_idx_to_group_idx_map, size(bc_idx_list))
        num_groups = 1
        input_idx_to_group_idx_map(1) = num_groups

        do i = 2, size(bc_idx_list)
            if (.not. are_bcs_identical(bc_idx_list(i), bc_idx_list(i - 1), input, physics_type_id)) then
                num_groups = num_groups + 1
            end if
            input_idx_to_group_idx_map(i) = num_groups
        end do

        self%physics(physics_type_id)%num_bcs = num_groups
        allocate (self%physics(physics_type_id)%bcs(num_groups))

        ! --- ステップB: どの入力IDがどのグループに属すかのマッピングを作成 ---
        max_id = maxval(input%conditions%boundary_conditions%id)
        call allocate_array(entity_id_to_group_idx_map, max_id)
        entity_id_to_group_idx_map = 0

        do i = 1, size(bc_idx_list)
            bc_id = input%conditions%boundary_conditions(bc_idx_list(i))%id
            group_idx = input_idx_to_group_idx_map(i)
            entity_id_to_group_idx_map(bc_id) = group_idx
        end do
        call deallocate_array(input_idx_to_group_idx_map)
        call deallocate_array(bc_idx_list)

        ! --- ステップC: 幾何情報格納 (2パス処理) ---
        call allocate_array(total_conn_per_group, num_groups)
        total_conn_per_group = 0

        ! パス1: 計測
        num_total_cells = input%geometry%vtk%num_total_cells
        do i = 1, num_total_cells
            if (input%geometry%vtk%cells(i)%cell_dimension == target_dimension) then
                cell_id = input%geometry%vtk%cells(i)%cell_entity_id
                if (cell_id > size(entity_id_to_group_idx_map) .or. entity_id_to_group_idx_map(cell_id) == 0) cycle

                current_group_idx = entity_id_to_group_idx_map(cell_id)
                self%physics(physics_type_id)%bcs(current_group_idx)%num_elements = &
                    self%physics(physics_type_id)%bcs(current_group_idx)%num_elements + 1
                total_conn_per_group(current_group_idx) = total_conn_per_group(current_group_idx) &
                                                          + input%geometry%vtk%cells(i)%num_nodes_in_cell
            end if
        end do

        ! パス2: 格納
        do i = 1, num_groups
            if (self%physics(physics_type_id)%bcs(i)%num_elements > 0) then
                call allocate_array(self%physics(physics_type_id)%bcs(i)%element_types, &
                                    self%physics(physics_type_id)%bcs(i)%num_elements)
                call allocate_array(self%physics(physics_type_id)%bcs(i)%connectivity%ind, &
                                    self%physics(physics_type_id)%bcs(i)%num_elements + 1)
                call allocate_array(self%physics(physics_type_id)%bcs(i)%connectivity%val, &
                                    total_conn_per_group(i))
                self%physics(physics_type_id)%bcs(i)%connectivity%ind(1) = 1
            end if
        end do
        call deallocate_array(total_conn_per_group)

        call allocate_array(current_elem_indices, num_groups)
        current_elem_indices = 0
        do i = 1, num_total_cells
            if (input%geometry%vtk%cells(i)%cell_dimension == target_dimension) then
                cell_id = input%geometry%vtk%cells(i)%cell_entity_id
                if (cell_id > size(entity_id_to_group_idx_map) .or. entity_id_to_group_idx_map(cell_id) == 0) cycle

                current_group_idx = entity_id_to_group_idx_map(cell_id)
                current_elem_indices(current_group_idx) = current_elem_indices(current_group_idx) + 1

                num_nodes = input%geometry%vtk%cells(i)%num_nodes_in_cell
                self%physics(physics_type_id)%bcs(current_group_idx)%element_types(current_elem_indices(current_group_idx)) &
                    = input%geometry%vtk%cells(i)%cell_type

                self%physics(physics_type_id)%bcs(current_group_idx)%connectivity%ind(current_elem_indices(current_group_idx) + 1) = &
                    self%physics(physics_type_id)%bcs(current_group_idx)%connectivity%ind(current_elem_indices(current_group_idx)) + num_nodes

                self%physics(physics_type_id)%bcs(current_group_idx)%connectivity%val( &
                    self%physics(physics_type_id)%bcs(current_group_idx)%connectivity%ind(current_elem_indices(current_group_idx)): &
                    self%physics(physics_type_id)%bcs(current_group_idx)%connectivity%ind(current_elem_indices(current_group_idx) + 1) - 1) = &
                    input%geometry%vtk%cells(i)%connectivity(1:num_nodes)
            end if
        end do

        call deallocate_array(current_elem_indices)
        call deallocate_array(entity_id_to_group_idx_map)
        call deallocate_array(bc_key)
        call deallocate_array(bc_sequence)
        call deallocate_array(active_region_id)
    end subroutine process_single_physics_bcs

    pure function get_bc_seq_pos(bc_id, bc_sequence) result(pos)
        implicit none
        integer(int32), intent(in) :: bc_id
        integer(int32), intent(in) :: bc_sequence(:)
        integer(int32) :: pos

        integer(int32) :: k

        pos = size(bc_sequence) + 1
        do k = 1, size(bc_sequence)
            if (bc_sequence(k) == bc_id) then
                pos = k
                exit
            end if
        end do
    end function get_bc_seq_pos

    subroutine sort_by_key(idx, key)
        implicit none
        integer(int32), intent(inout) :: idx(:)
        integer(int32), intent(inout) :: key(:)

        integer(int32) :: i, j, tmp_idx, tmp_key

        do i = 2, size(idx)
            j = i
            do while (j > 1 .and. key(j) < key(j - 1))
                tmp_idx = idx(j)
                idx(j) = idx(j - 1)
                idx(j - 1) = tmp_idx
                tmp_key = key(j)
                key(j) = key(j - 1)
                key(j - 1) = tmp_key
                j = j - 1
            end do
        end do
    end subroutine sort_by_key

    pure function are_bcs_identical(idx1, idx2, input, physics_type_id) result(is_identical)
        implicit none
        integer(int32), intent(in) :: idx1, idx2
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: physics_type_id
        logical :: is_identical

        integer(int32) :: bc_type1, bc_type2
        logical :: alloc1, alloc2

        is_identical = .false.

        select case (physics_type_id)
        case (PHYSICS_TYPE_THERMAL)
            ! --- 関数を使い、typeを文字列から整数IDに変換 ---
            bc_type1 = get_bc_type_from_string(input%conditions%boundary_conditions(idx1)%thermal%type, physics_type_id)
            bc_type2 = get_bc_type_from_string(input%conditions%boundary_conditions(idx2)%thermal%type, physics_type_id)

            ! --- 整数IDで比較 ---
            if (bc_type1 /= bc_type2) then
                return
            end if

            ! --- allocated() を使った安全な値の比較 ---
            alloc1 = allocated(input%conditions%boundary_conditions(idx1)%thermal%values)
            alloc2 = allocated(input%conditions%boundary_conditions(idx2)%thermal%values)

            if (alloc1 .and. alloc2) then
                if (size(input%conditions%boundary_conditions(idx1)%thermal%values) == &
                    size(input%conditions%boundary_conditions(idx2)%thermal%values)) then

                    if (all(input%conditions%boundary_conditions(idx1)%thermal%values == &
                            input%conditions%boundary_conditions(idx2)%thermal%values)) then
                        is_identical = .true.
                    end if
                end if
            else if (.not. alloc1 .and. .not. alloc2) then
                is_identical = .true.
            end if

        case (PHYSICS_TYPE_HYDRAULIC)
            ! --- 関数を使い、typeを文字列から整数IDに変換 ---
            bc_type1 = get_bc_type_from_string(input%conditions%boundary_conditions(idx1)%hydraulic%type, physics_type_id)
            bc_type2 = get_bc_type_from_string(input%conditions%boundary_conditions(idx2)%hydraulic%type, physics_type_id)

            ! --- 整数IDで比較 ---
            if (bc_type1 /= bc_type2) then
                return
            end if

            ! --- allocated() を使った安全な値の比較 ---
            alloc1 = allocated(input%conditions%boundary_conditions(idx1)%hydraulic%values)
            alloc2 = allocated(input%conditions%boundary_conditions(idx2)%hydraulic%values)

            if (alloc1 .and. alloc2) then
                if (size(input%conditions%boundary_conditions(idx1)%hydraulic%values) == &
                    size(input%conditions%boundary_conditions(idx2)%hydraulic%values)) then

                    if (all(input%conditions%boundary_conditions(idx1)%hydraulic%values == &
                            input%conditions%boundary_conditions(idx2)%hydraulic%values)) then
                        is_identical = .true.
                    end if
                end if
            else
                is_identical = .true.
            end if
        end select

    end function are_bcs_identical
end module domain_manager
