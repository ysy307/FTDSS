module Domain_Module
    use, intrinsic :: iso_fortran_env, only: int32
    use :: core_core, only:type_dp_3d
    use :: Domain_Element, only:ElementHolder
    use :: Domain_Side, only:SideHolder
    use :: Domain_Element_Factory, only:Create_Element
    use :: Domain_Side_Factory, only:Create_Side
    use :: domain_adjacency, only:type_node_adjacency, type_element_adjacency
    use :: domain_multicoloring, only:type_coloring, type_colored_info
    use :: domain_rcm, only:type_rcm
    use :: Inout_Input
    implicit none
    private
    public :: type_domain

    ! type :: type_colored_info
    !     integer(int32) :: num_elements
    !     integer(int32), allocatable :: elements(:) ! 各要素のインデックス
    ! end type type_colored_info

    ! type :: type_coloring
    !     integer(int32) :: num_colors
    !     integer(int32), allocatable :: color(:)
    !     type(type_colored_info), allocatable :: colored(:) ! 各色に属する要素の情報
    ! end type type_coloring

    type :: type_domain
        integer(int32), private :: num_volumes
        integer(int32), private :: num_elements
        integer(int32), private :: num_sides
        integer(int32), private :: num_nodes
        integer(int32), private :: num_regions
        type(ElementHolder), allocatable :: elements(:)
        type(SideHolder), allocatable :: sides(:)
        integer(int32), allocatable :: RCM_perm(:)
        integer(int32), allocatable :: RCM_inv_perm(:)

        type(type_element_adjacency) :: element_adjacency
        type(type_node_adjacency) :: node_adjacency
        type(type_coloring) :: colors
        type(type_rcm) :: rcm
        ! ...
    contains
        procedure, pass(self) :: initialize

        procedure, pass(self) :: get_num_elements
        procedure, pass(self) :: get_num_Sides
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

        integer :: CountElements, CountSides
        integer :: iCell, iElem, iSide
        integer :: factory_ierr

        ! --- 一時的なデータ配列 ---
        integer(int32), allocatable :: conn_data(:)
        integer(int32), allocatable :: conn_ptr(:)
        integer(int32) :: total_conn_size, i, n_nodes_in_elem

        ierr = 0
        CountElements = 0
        CountSides = 0

        do iCell = 1, Input%VTK%num_total_cells
            if (Input%VTK%Is_In(Input%VTK%CELLS(iCell)%cell_type, 1)) then
                CountSides = CountSides + 1
            end if
            if (Input%VTK%Is_In(Input%VTK%CELLS(iCell)%cell_type, 2)) then
                CountElements = CountElements + 1
            end if
        end do

        self%num_elements = CountElements
        self%num_sides = CountSides
        self%num_nodes = Input%VTK%num_points
        self%num_regions = Input%Basic%numRegion

        allocate (self%Elements(self%num_elements))
        allocate (self%Sides(self%num_sides))

        iElem = 1
        iSide = 1
        do iCell = 1, Input%VTK%num_total_cells
            if (Input%VTK%Is_In(Input%VTK%CELLS(iCell)%cell_type, 1)) then
                call Create_Side( &
                    new_side=self%Sides(iSide)%s, &
                    shape_type=Input%VTK%CELLS(iCell)%cell_type, &
                    ierr=factory_ierr, &
                    iSide=iSide, &
                    Global_Coordinate=Coordinate, &
                    Connectivity=Input%VTK%CELLS(iCell)%CONNECTIVITY, &
                    GroupID=Input%VTK%CELLS(iCell)%cell_entity_id &
                    )
                if (factory_ierr /= 0) then
                    ierr = -1
                    return
                end if
                iSide = iSide + 1
            end if
            if (Input%VTK%Is_In(Input%VTK%CELLS(iCell)%cell_type, 2)) then
                call create_element( &
                    new_element=self%Elements(iElem)%e, &
                    shape_type=Input%VTK%CELLS(iCell)%cell_type, &
                    ierr=factory_ierr, &
                    iElem=iCell, &
                    Global_Coordinate=Coordinate, &
                    Connectivity=Input%VTK%CELLS(iCell)%CONNECTIVITY, &
                    GroupID=Input%VTK%CELLS(iCell)%cell_entity_id &
                    )
                if (factory_ierr /= 0) then
                    ierr = -1
                    return
                end if
                iElem = iElem + 1
            end if
        end do

        ! =============================================================== !
        ! ▼▼▼ ここからが隣接行列の構築処理 ▼▼▼
        ! =============================================================== !

        !===============================================================
        ! 2. 汎用モジュール用のデータ準備 (要素コネクティビティをCSR形式に)
        !===============================================================
        total_conn_size = 0
        do i = 1, self%num_elements
            total_conn_size = total_conn_size + size(self%elements(i)%e%conn)
        end do
        call allocate_array(conn_ptr, self%num_elements + 1)
        call allocate_array(conn_data, total_conn_size)
        conn_ptr(1) = 1
        do i = 1, self%num_elements
            n_nodes_in_elem = size(self%elements(i)%e%conn)
            conn_ptr(i + 1) = conn_ptr(i) + n_nodes_in_elem
            conn_data(conn_ptr(i):conn_ptr(i + 1) - 1) = self%elements(i)%e%conn
        end do
        print *, "Step 2: Raw connectivity data prepared."

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
        ! =============================================================== !
        ! ▲▲▲ 隣接行列の構築処理ここまで ▲▲▲
        ! =============================================================== !

    end subroutine initialize

    function get_num_elements(self) result(numElement)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32) :: numElement

        numElement = self%num_elements

    end function get_num_elements

    function get_num_Sides(self) result(numSide)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32) :: numSide

        numSide = self%num_sides

    end function get_num_Sides

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

end module Domain_Module
