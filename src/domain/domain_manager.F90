module domain_manager
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: mpi_f08
    use :: stdlib_logger
    use :: stdlib_sorting, only:sort_index
    use :: module_core, only:allocate_array, deallocate_array
    use :: module_input, only:type_input
    use :: module_properties
    use :: domain_multicoloring, only:type_coloring
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
        procedure, pass(self) :: initialize => initialize_boundary_manager
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
        procedure, pass(self) :: initialize => initialize_node_manager
    end type type_node_manager

    ! --- 要素データマネージャ ---
    type :: type_element_manager
        type(type_domain), pointer, private :: parent => null()
        integer(int32) :: num_elements = 0
        integer(int32), allocatable :: fe_types(:)
        integer(int32), allocatable :: fe_material_ids(:)
        type(type_fe_connectivity) :: connectivity
        type(type_coloring) :: colors
    contains
        procedure, pass(self) :: initialize => initialize_element_manager
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

        ! カラーリング情報を構築
        call self%colors%initialize(input)
    end subroutine initialize_element_manager

    ! ==========================================================
    ! Boundary Managerの初期化
    ! ==========================================================
    subroutine initialize_boundary_manager(self, input)
        class(type_boundary_manager), intent(inout) :: self
        type(type_input), intent(in) :: input
        ! (このサブルーチンの完全な実装は非常に長大になりますが、
        !  これまでの議論の骨子をここに記述します)
        ! ... (ステップ1: 有効かつユニークな物理BCを抽出)
        ! ... (ステップ2: メッシュ要素を物理BCごとにグループ分け)
        ! ... (ステップ3: SEQUENCE順にファクトリ関数を呼び出し、最終構造を構築)
    end subroutine

end module domain_manager
