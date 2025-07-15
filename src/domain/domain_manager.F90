module domain_manager
    use, intrinsic :: iso_fortran_env, only: int32
    use :: stdlib_logger
    use :: module_core, only:type_dp_3d
    use :: module_input, only:type_input
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
        integer(int32), private :: num_materials
        type(holder_elements), allocatable :: elements(:)
        type(holder_sides), allocatable :: sides(:)

        type(type_coloring) :: colors
        type(type_rcm) :: rcm

        integer(int32), private :: computaion_dimension
        ! ...
    contains
        procedure, pass(self) :: initialize

        procedure, pass(self) :: get_num_elements
        procedure, pass(self) :: get_num_sides
        procedure, pass(self) :: get_num_nodes
        procedure, pass(self) :: get_num_materials
        procedure, pass(self) :: get_computation_dimension
    end type type_domain

contains
    subroutine initialize(self, input, Coordinate, ierr)
        implicit none
        class(type_domain), intent(inout) :: self
        type(type_input), intent(in) :: input
        type(type_dp_3d), intent(inout), pointer :: Coordinate
        integer(int32), intent(inout) :: ierr

        type(type_crs_adjacency_element) :: element_adjacency
        type(type_node_adjacency) :: node_adjacency

        integer(int32) :: count_sides, count_elements, count_volumes
        integer(int32) :: iCell, iElem, iSide
        integer(int32) :: factory_ierr
        integer(int32) :: cell_dimension

        ! -----------------------------------------------------------------------------------
        ! 初期化処理
        ! -----------------------------------------------------------------------------------
        ierr = 0
        count_sides = 0
        count_elements = 0
        count_volumes = 0

        do iCell = 1, input%geometry%vtk%num_total_cells
            cell_dimension = input%geometry%vtk%cells(iCell)%get_dimension()
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
        self%num_nodes = input%geometry%vtk%num_points
        self%num_materials = input%basic%num_materials

        if (allocated(self%elements)) deallocate (self%elements)
        allocate (self%elements(self%num_elements))
        if (allocated(self%sides)) deallocate (self%sides)
        allocate (self%sides(self%num_sides))

        iElem = 1
        iSide = 1
        do iCell = 1, input%geometry%vtk%num_total_cells
            cell_dimension = input%geometry%vtk%cells(iCell)%get_dimension()
            select case (cell_dimension)
            case (1)
                call create_side(new_side=self%sides(iSide)%s, &
                                 id=iCell, &
                                 global_coordinate=Coordinate, &
                                 cell_info=input%geometry%vtk%cells(iCell), &
                                 integration=input%basic%geometry_settings, &
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
                                    cell_info=input%geometry%vtk%cells(iCell), &
                                    integration=input%basic%geometry_settings, &
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

        self%computaion_dimension = input%basic%simulation_settings%calculate_dimension

        !===============================================================
        ! 3. 隣接行列の構築
        !===============================================================
        call element_adjacency%initialize(self%elements)
        print *, "Step 3a: Element adjacency matrix created."

        !===============================================================
        ! 4. RCM並べ替えの実行
        !===============================================================
        select case (input%basic%solver_settings%reordering)
        case ("rcm")
            call self%rcm%reorder(self%elements)
            call self%rcm%invert()
            call global_logger%log_information(message="RCM reordering completed.")
        end select

        !===============================================================
        ! 5. グラフ彩色の実行
        !===============================================================
        select case (input%basic%solver_settings%coloring)
        case ("welsh_powell", "dsatur", "lfo")
            call self%colors%initialize(element_adjacency, algorithm_name=input%basic%solver_settings%coloring)
            call global_logger%log_information(message="Graph coloring completed using " &
                                               //trim(self%colors%algorithm_name)//" algorithm.")
        end select

        !===============================================================
        ! 6. 後片付け
        !===============================================================
        call global_logger%log_information(message="Initialization process completed successfully.")

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

    function get_num_materials(self) result(num_materials)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32) :: num_materials

        num_materials = self%num_materials

    end function get_num_materials

    function get_computation_dimension(self) result(computaion_dimension)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32) :: computaion_dimension

        computaion_dimension = self%computaion_dimension

    end function get_computation_dimension

end module domain_manager
