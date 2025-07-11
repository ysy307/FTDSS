module domain_manager
    use, intrinsic :: iso_fortran_env, only: int32
    use :: stdlib_logger
    use :: module_core, only:type_dp_3d
    use :: Inout_Input
    use :: domain_element, only:holder_elements
    use :: domain_side, only:holder_sides
    use :: domain_element_factory, only:create_element
    use :: domain_side_factory, only:create_side
    use :: domain_adjacency, only:type_node_adjacency, type_crs_adjacency_element
    use :: domain_multicoloring, only:type_coloring, type_colored_info
    use :: domain_rcm, only:type_rcm
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

        type(type_crs_adjacency_element) :: element_adjacency
        type(type_node_adjacency) :: node_adjacency
        type(type_coloring) :: colors
        type(type_rcm) :: rcm

        integer(int32), private :: computaion_dimension
        ! ...
    contains
        procedure, pass(self) :: initialize

        procedure, pass(self) :: get_num_elements
        procedure, pass(self) :: get_num_sides
        procedure, pass(self) :: get_num_nodes
        procedure, pass(self) :: get_num_regions
        procedure, pass(self) :: get_computation_dimension
    end type type_domain

contains
    subroutine initialize(self, Input, Coordinate, ierr)
        implicit none
        class(type_domain), intent(inout) :: self
        type(Type_Input), intent(in) :: Input ! Inputモジュールからデータを受け取る
        type(type_dp_3d), intent(inout), pointer :: Coordinate
        integer(int32), intent(inout) :: ierr

        integer(int32) :: count_sides, count_elements, count_volumes
        integer(int32) :: iCell, iElem, iSide
        integer(int32) :: factory_ierr
        integer(int32) :: cell_dimension

        ! ! --- 一時的なデータ配列 ---
        ! integer(int32), allocatable :: conn_data(:)
        ! integer(int32), allocatable :: conn_ptr(:)
        ! integer(int32) :: total_connec_size, i, n_nodes_in_elem

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

        select case (Input%Basic%DimensionType)
        case (1, 2)
            self%computaion_dimension = 2_int32
        case (3)
            self%computaion_dimension = 3_int32
        end select
        ! !===============================================================
        ! ! 2. 汎用モジュール用のデータ準備 (Input%vtkからCSR形式を構築)
        ! !===============================================================
        ! ! 全コネクティビティデータの合計サイズを計算
        ! total_connec_size = 0
        ! do i = 1, self%num_elements
        !     total_connec_size = total_connec_size + self%elements(i)%e%get_num_nodes()
        ! end do

        ! ! CSR配列を確保
        ! call allocate_array(conn_ptr, self%num_elements + 1_int32)
        ! call allocate_array(conn_data, total_connec_size)

        ! ! Input%vtk%cells の情報を使ってCSR配列を構築
        ! conn_ptr(1) = 1
        ! do i = 1, self%num_elements
        !     n_nodes_in_elem = self%elements(i)%e%get_num_nodes()
        !     conn_ptr(i + 1) = conn_ptr(i) + n_nodes_in_elem

        !     conn_data(conn_ptr(i):conn_ptr(i + 1) - 1) = self%elements(i)%e%connectivity(1:n_nodes_in_elem)
        ! end do
        ! print *, "Step 2: Connectivity data prepared from VTK input."

        !===============================================================
        ! 3. 隣接行列の構築
        !===============================================================
        call self%element_adjacency%initialize(self%elements)
        print *, "Step 3a: Element adjacency matrix created."

        ! call self%node_adjacency%initialize(self%num_nodes, self%num_elements, conn_data, conn_ptr)
        ! print *, "Step 3b: Node adjacency matrix created."

        !===============================================================
        ! 4. RCM並べ替えの実行
        !===============================================================
        call self%rcm%reorder(self%elements)
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
        ! call deallocate_array(conn_ptr)
        ! call deallocate_array(conn_data)
        print *, "Initialization process completed successfully."

    end subroutine initialize

    function get_num_elements(self) result(num_elements)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32) :: num_elements

        num_elements = self%num_elements

    end function get_num_elements

    function get_num_sides(self) result(num_sides)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32) :: num_sides

        num_sides = self%num_sides

    end function get_num_sides

    function get_num_nodes(self) result(num_nodea)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32) :: num_nodea

        num_nodea = self%num_nodes

    end function get_num_nodes

    function get_num_regions(self) result(num_regions)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32) :: num_regions

        num_regions = self%num_regions

    end function get_num_regions

    function get_computation_dimension(self) result(computaion_dimension)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32) :: computaion_dimension

        computaion_dimension = self%computaion_dimension

    end function get_computation_dimension

end module domain_manager
