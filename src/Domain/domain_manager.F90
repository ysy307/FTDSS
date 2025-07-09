module domain_manager
    use, intrinsic :: iso_fortran_env, only: int32
    use :: module_core, only:type_dp_3d
    use :: Domain_Element, only:holder_elements
    use :: Domain_Side, only:holder_sides
    use :: Domain_Element_Factory, only:create_element
    use :: Domain_Side_Factory, only:create_side
    use :: domain_adjacency, only:type_node_adjacency, type_element_adjacency
    use :: domain_multicoloring, only:type_coloring, type_colored_info
    use :: domain_rcm, only:type_rcm
    use :: Inout_Input
    implicit none
    private

    public :: type_domain

    type :: type_domain
        integer(int32), private :: num_sides
        integer(int32), private :: num_elements
        integer(int32), private :: num_volumes
        integer(int32), private :: num_nodes
        integer(int32), private :: num_regions
        type(holder_elements), allocatable :: elements(:)
        type(holder_sides), allocatable :: sides(:)

        type(type_element_adjacency) :: element_adjacency
        type(type_node_adjacency) :: node_adjacency
        type(type_coloring) :: colors
        type(type_rcm) :: rcm
        ! ...
    contains
        procedure, pass(self) :: initialize

        procedure, pass(self) :: get_num_elements
        procedure, pass(self) :: get_num_sides
        procedure, pass(self) :: get_num_Nodes
        procedure, pass(self) :: get_num_Regions
    end type type_domain

contains
    subroutine initialize(self, Input, Coordinate, ierr)
        implicit none
        class(type_domain), intent(inout) :: self
        type(Type_Input), intent(in) :: Input ! Inputモジュールからデータを受け取る
        type(type_dp_3d), intent(inout), pointer :: Coordinate
        integer, intent(out) :: ierr

        integer(int32) :: count_sides, count_elements, count_volumes
        integer(int32) :: iCell, iElem, iSide
        integer(int32) :: factory_ierr
        integer(int32) :: cell_dimension

        ! --- 一時的なデータ配列 ---
        integer(int32), allocatable :: conn_data(:)
        integer(int32), allocatable :: conn_ptr(:)
        integer(int32) :: total_connec_size, i, n_nodes_in_elem

        ! -----------------------------------------------------------------------------------
        ! 初期化処理
        ! -----------------------------------------------------------------------------------
        ierr = 0
        count_sides = 0
        count_elements = 0
        count_volumes = 0

        do iCell = 1, Input%VTK%num_total_cells
            cell_dimension = Input%vtk%cells(iCell)%get_dimension()
            select case (cell_dimension)
            case (1)
                count_sides = count_sides + 1
            case (2)
                count_elements = count_elements + 1
            case (3)
                count_volumes = count_volumes + 1
            end select
        end do

        self%num_elements = count_elements
        self%num_sides = count_sides
        self%num_nodes = Input%VTK%num_points
        self%num_regions = Input%Basic%numRegion

        if (allocated(self%elements)) deallocate (self%elements)
        allocate (self%elements(self%num_elements))
        if (allocated(self%sides)) deallocate (self%sides)
        allocate (self%sides(self%num_sides))

        iElem = 1
        iSide = 1
        do iCell = 1, Input%VTK%num_total_cells
            cell_dimension = Input%vtk%cells(iCell)%get_dimension()
            select case (cell_dimension)
            case (1)
                call create_side(new_side=self%sides(iSide)%s, &
                                 id=iCell, &
                                 global_coordinate=Coordinate, &
                                 cell_info=Input%vtk%cells(iCell), &
                                 ierr=factory_ierr)
                if (factory_ierr /= 0) then
                    ierr = -1
                    return
                end if
                iSide = iSide + 1
            case (2)
                call create_element(new_element=self%elements(iElem)%e, &
                                    id=iCell, &
                                    global_coordinate=Coordinate, &
                                    cell_info=Input%vtk%cells(iCell), &
                                    ierr=factory_ierr)
                if (factory_ierr /= 0) then
                    ierr = -1
                    return
                end if
                iElem = iElem + 1
            case (3)
                !!TBI
            end select

        end do
        !===============================================================
        ! 2. 汎用モジュール用のデータ準備 (Input%vtkからCSR形式を構築)
        !===============================================================
        ! 全コネクティビティデータの合計サイズを計算
        total_connec_size = 0
        do i = 1, Input%vtk%num_total_cells
            total_connec_size = total_connec_size + size(Input%vtk%cells(i)%connectivity)
        end do

        ! CSR配列を確保
        call allocate_array(conn_ptr, Input%vtk%num_total_cells + 1_int32)
        call allocate_array(conn_data, total_connec_size)

        ! Input%vtk%cells の情報を使ってCSR配列を構築
        conn_ptr(1) = 1
        do i = 1, Input%vtk%num_total_cells
            n_nodes_in_elem = size(Input%vtk%cells(i)%connectivity)
            conn_ptr(i + 1) = conn_ptr(i) + n_nodes_in_elem

            conn_data(conn_ptr(i):conn_ptr(i + 1) - 1) = Input%vtk%cells(i)%connectivity
        end do
        print *, "Step 2: Connectivity data prepared from VTK input."

        !===============================================================
        ! 3. 隣接行列の構築
        !===============================================================
        call self%element_adjacency%initialize(self%num_nodes, conn_data, conn_ptr)
        print *, "Step 3a: Element adjacency matrix created."

        call self%node_adjacency%initialize(self%num_nodes, self%num_elements, conn_data, conn_ptr)
        print *, "Step 3b: Node adjacency matrix created."

        !===============================================================
        ! 4. RCM並べ替えの実行
        !===============================================================
        call self%rcm%reorder(self%node_adjacency)
        call self%rcm%invert()
        print *, "Step 4: RCM reordering performed."

        !===============================================================
        ! 5. グラフ彩色の実行
        !===============================================================
        call self%colors%initialize(self%element_adjacency, algorithm_name="welsh-powell")
        print *, "Step 5: Graph coloring performed."

        !===============================================================
        ! 6. 後片付け
        !===============================================================
        call deallocate_array(conn_ptr)
        call deallocate_array(conn_data)
        print *, "Initialization process completed successfully."

    end subroutine initialize

    function get_num_elements(self) result(numElement)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32) :: numElement

        numElement = self%num_elements

    end function get_num_elements

    function get_num_sides(self) result(numSide)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32) :: numSide

        numSide = self%num_sides

    end function get_num_sides

    function get_num_Nodes(self) result(numNode)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32) :: numNode

        numNode = self%num_nodes

    end function get_num_Nodes

    function get_num_Regions(self) result(numRegion)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32) :: numRegion

        numRegion = self%num_regions

    end function get_num_Regions

end module domain_manager
