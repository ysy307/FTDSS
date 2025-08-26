module domain_manager
    use, intrinsic :: iso_fortran_env, only: int32
    use :: stdlib_logger
    use :: module_core, only:type_dp_3d, allocate_array, deallocate_array
    use :: module_input, only:type_input
    use :: module_mesh
    use :: domain_adjacency, only:type_node_adjacency, type_crs_adjacency_element, type_map_node_to_element
    use :: domain_multicoloring, only:type_coloring, type_colored_info
    use :: domain_reordering, only:type_reordering
    implicit none
    private

    public :: type_domain

    type :: type_domain
        integer(int32), private :: num_sides
        integer(int32), private :: num_elements
        integer(int32), private :: num_volumes
        integer(int32), private :: num_nodes
        integer(int32), private :: num_materials
        type(holder_elements), allocatable :: elements(:) !&
        type(holder_sides),    allocatable :: sides(:) !&

        type(type_coloring) :: colors
        type(type_reordering) :: reordering
        type(type_node_adjacency) :: node_adjacency
        type(type_crs_adjacency_element) :: element_adjacency
        type(type_map_node_to_element) :: map_node_to_element

        integer(int32), private :: computation_dimension
        ! ...
    contains
        procedure, pass(self) :: initialize => initialize_type_domain
        procedure, pass(self) :: apply_reordering

        procedure, pass(self) :: get_num_elements
        procedure, pass(self) :: get_num_sides
        procedure, pass(self) :: get_num_nodes
        procedure, pass(self) :: get_num_materials
        procedure, pass(self) :: get_computation_dimension
    end type type_domain

contains
    subroutine initialize_type_domain(self, input, Coordinate)
        implicit none
        class(type_domain), intent(inout) :: self
        type(type_input), intent(in) :: input
        type(type_dp_3d), intent(inout), pointer :: Coordinate

        integer(int32) :: count_sides, count_elements, count_volumes
        integer(int32) :: iCell, iElem, iSide
        integer(int32) :: cell_dimension

        ! -----------------------------------------------------------------------------------
        ! 初期化処理
        ! -----------------------------------------------------------------------------------
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
                self%sides(iSide)%s = create_side(id=iCell, &
                                                  global_coordinate=Coordinate, &
                                                  input=input)
                iSide = iSide + 1
            case (2)
                self%elements(iElem)%e = create_element(id=iCell, &
                                                        global_coordinate=Coordinate, &
                                                        input=input)
                iElem = iElem + 1
            case (3)
                !!TBI
            end select

        end do

        self%computation_dimension = input%basic%simulation_settings%calculate_dimension

        !===============================================================
        ! 3. 隣接行列の構築
        !===============================================================
        call self%node_adjacency%initialize(self%num_nodes, self%computation_dimension, self%sides, self%elements)
        call self%element_adjacency%initialize(self%elements)
        call self%map_node_to_element%initialize(self%num_nodes, self%elements, "fast")

        !===============================================================
        ! 4. RCM並べ替えの実行
        !===============================================================
        call self%reordering%initialize(input%basic%solver_settings%reordering, self%node_adjacency)
        if (input%basic%solver_settings%reordering /= "none") then
            call self%apply_reordering()
            call global_logger%log_information(message="RCM reordering completed.")
        end if

        !===============================================================
        ! 5. グラフ彩色の実行
        !===============================================================
        call self%colors%initialize(input%basic%solver_settings%coloring, self%element_adjacency)
        call global_logger%log_information(message="Graph coloring completed using " &
                                           //trim(self%colors%algorithm_name)//" algorithm.")

        call global_logger%log_information(message="Initialization process completed successfully.")

    end subroutine initialize_type_domain

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

    function get_computation_dimension(self) result(computation_dimension)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32) :: computation_dimension

        computation_dimension = self%computation_dimension

    end function get_computation_dimension

    subroutine apply_reordering(self)
        implicit none
        class(type_domain), intent(inout) :: self

        integer(int32) :: iElem, iSide, i
        integer(int32), dimension(:), pointer :: ptr_connectivity => null()
        integer(int32), allocatable :: connectivity(:)
        integer(int32), allocatable :: connectivity_reordered(:)
        integer(int32) :: node_per_mesh

        if (self%computation_dimension >= 3) then
            !! TBI: Handle 3D reordering if necessary
        end if

        if (self%computation_dimension >= 2) then
            do iElem = 1, self%get_num_elements()
                ptr_connectivity => self%elements(iElem)%e%get_connectivity()
                node_per_mesh = self%elements(iElem)%e%get_num_nodes()
                call allocate_array(connectivity, node_per_mesh)
                call allocate_array(connectivity_reordered, node_per_mesh)

                do i = 1, node_per_mesh
                    connectivity(i) = ptr_connectivity(i)
                end do
                call self%reordering%to_reordered(connectivity, connectivity_reordered)
                do i = 1, node_per_mesh
                    ptr_connectivity(i) = connectivity_reordered(i)
                end do

                call deallocate_array(connectivity)
                call deallocate_array(connectivity_reordered)
            end do
        end if

        if (self%computation_dimension >= 1) then
            do iSide = 1, self%get_num_sides()
                ptr_connectivity => self%sides(iSide)%s%get_connectivity()
                node_per_mesh = self%sides(iSide)%s%get_num_nodes()
                call allocate_array(connectivity, node_per_mesh)
                call allocate_array(connectivity_reordered, node_per_mesh)

                do i = 1, node_per_mesh
                    connectivity(i) = ptr_connectivity(i)
                end do
                call self%reordering%to_reordered(connectivity, connectivity_reordered)
                do i = 1, node_per_mesh
                    ptr_connectivity(i) = connectivity_reordered(i)
                end do

                call deallocate_array(connectivity)
                call deallocate_array(connectivity_reordered)

            end do
        end if

    end subroutine apply_reordering

end module domain_manager
