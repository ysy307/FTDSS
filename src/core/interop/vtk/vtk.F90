!> Provides high-level structures and procedures for reading VTK and VTU files.
!> This module acts as a Fortran interface to a C++ backend that handles the low-level file I/O.
module core_interop_vtk
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding
    use :: mpi_f08
    use :: stdlib_strings, only:to_string, replace_all, strip
    use :: stdlib_sorting, only:sort
    use :: stdlib_logger
    use :: core_types, only:type_coordinate_array_dp
    use :: core_allocate, only:allocate_array
    use :: core_deallocate, only:deallocate_array
    use :: core_unique, only:unique
    use :: core_interop_vtk_constants
    use :: core_interop_vtk_wrapper
    use :: core_interop_vtu_wrapper

    implicit none
    private

    public :: type_vtk
    public :: type_vtk_cell

    !> Stores information about a single VTK cell.
    type :: type_vtk_cell
        !> VTK cell type identifier (e.g., VTK_TRIANGLE).
        integer(int32) :: cell_type
        !> Human-readable name of the cell type (e.g., "TRIANGLE").
        character(:), allocatable :: cell_type_name
        !> Number of nodes that define the cell.
        integer(int32) :: num_nodes_in_cell
        !> Unique identifier for the cell.
        integer(int32) :: cell_entity_id
        !> Geometric dimension of the cell (e.g., 2 for a triangle).
        integer(int32) :: cell_dimension
        !> Order of the cell (e.g., 1 for linear, 2 for quadratic).
        integer(int32) :: cell_order
        !> List of point IDs forming the cell.
        integer(int32), allocatable :: connectivity(:)
        !> MPI rank that owns this cell.
        integer(int32) :: rank
        !> Color assigned to the cell for graph coloring.
        integer(int32) :: color
    contains
        procedure :: set => type_vtk_cell_set
        procedure :: get_dimension => type_vtk_cell_get_dimension
        procedure :: get_order => type_vtk_cell_get_order
        procedure :: get_size => type_vtk_cell_get_size_connectivity
    end type type_vtk_cell

    !> Manages data read from a VTK or VTU file.
    type :: type_vtk
        !> File format, either "vtk" or "vtu".
        character(:), allocatable :: format
        !> Type of dataset (e.g., "UnstructuredGrid").
        character(:), allocatable :: dataset
        !> Number of points on the local MPI process.
        integer(int32) :: num_points = 0
        !> Coordinates of the points (x, y, z).
        type(type_coordinate_array_dp) :: points
        !> Number of cells on the local MPI process.
        integer(int32) :: num_total_cells = 0
        !> Array of cell data structures.
        type(type_vtk_cell), allocatable :: cells(:)
        !> Values of requested scalar/vector fields at points.
        real(real64), allocatable :: point_field_values(:, :)

        !> Rank of the current MPI process.
        integer(int32) :: my_rank = -1
        !> Total number of MPI processes.
        integer(int32) :: num_procs = -1
        !> Total number of points across all processes.
        integer(int32) :: global_num_points = 0
        !> Total number of cells across all processes.
        integer(int32) :: global_num_total_cells = 0
        !> Global ID for each local node.
        integer(int32), allocatable :: global_node_ids(:)
        !> Type of each node (e.g., internal, shared).
        integer(int32), allocatable :: node_type(:)
        !> Number of ranks sharing each node.
        integer(int32), allocatable :: num_sharing_ranks(:)
        !> Owning rank for each shared node.
        integer(int32), allocatable :: owner_rank(:, :)
        !> Ranks that share nodes with the current rank.
        integer(int32), allocatable :: communication_partners(:, :)

        type(c_ptr), private :: handle = c_null_ptr
        character(4), private :: reader_type = "none"
    contains
        procedure :: initialize_vtk => type_vtk_vtk_initialize
        procedure :: initialize_vtu => type_vtk_vtu_initialize
        procedure :: get_active_region_info
        final :: finalize_vtk_object
    end type type_vtk

    interface
        !> Sets the properties of a VTK cell based on its node count.
        module subroutine type_vtk_cell_set(self, num_nodes_in_cell)
            !> The vtk_cell object to modify.
            class(type_vtk_cell), intent(inout) :: self
            !> Number of nodes in the cell.
            integer(int32), intent(in) :: num_nodes_in_cell
        end subroutine type_vtk_cell_set

        !> Returns the geometric dimension of the cell.
        module function type_vtk_cell_get_dimension(self) result(dimension)
            !> The vtk_cell object to query.
            class(type_vtk_cell), intent(in) :: self
            !> Geometric dimension of the cell (0, 1, 2, or 3).
            integer(int32) :: dimension
        end function type_vtk_cell_get_dimension

        !> Returns the order of the cell (linear or quadratic).
        module function type_vtk_cell_get_order(self) result(order)
            !> The vtk_cell object to query.
            class(type_vtk_cell), intent(in) :: self
            !> Order of the cell (1 for linear, 2 for quadratic).
            integer(int32) :: order
        end function type_vtk_cell_get_order

        !> Returns the number of nodes in the cell's connectivity.
        module function type_vtk_cell_get_size_connectivity(self) result(size)
            !> The vtk_cell object to query.
            class(type_vtk_cell), intent(in) :: self
            !> The size of the connectivity array.
            integer(int32) :: size
        end function type_vtk_cell_get_size_connectivity

        !> Extracts unique node IDs from cells of a specific dimension.
        module subroutine get_active_region_info(self, unique_ids, target_dim)
            !> VTK data object.
            class(type_vtk), intent(in) :: self
            !> Output array to store the unique node IDs.
            integer(int32), allocatable, intent(inout) :: unique_ids(:)
            !> The geometric dimension of the cells to consider.
            integer(int32), intent(in), optional :: target_dim
        end subroutine get_active_region_info
    end interface

    interface
        !> Initializes the vtk object by reading a legacy VTK (.vtk) file.
        module subroutine type_vtk_vtk_initialize(self, file_name, global_node_id_key, node_type_key, num_sharing_ranks_key, &
                                                  owner_ranks_key, communication_partners_key, cell_id_key, rank_key, &
                                                  color_key, point_field_names)
            !> The vtk object to be initialized.
            class(type_vtk), intent(inout) :: self
            !> Path to the input .vtk file.
            character(*), intent(in) :: file_name
            !> Key for the point data array with global node IDs.
            character(*), intent(in), optional :: global_node_id_key
            !> Key for the point data array defining node types.
            character(*), intent(in), optional :: node_type_key
            !> Key for the point data array for the number of sharing ranks.
            character(*), intent(in), optional :: num_sharing_ranks_key
            !> Key for the point data array listing owner ranks.
            character(*), intent(in), optional :: owner_ranks_key
            !> Key for the point data array listing communication partners.
            character(*), intent(in), optional :: communication_partners_key
            !> Key for the cell data array with cell IDs.
            character(*), intent(in), optional :: cell_id_key
            !> Key for the cell data array specifying the owning rank.
            character(*), intent(in), optional :: rank_key
            !> Key for the cell data array for graph coloring.
            character(*), intent(in), optional :: color_key
            !> Names of additional point data fields to read.
            character(*), intent(in), optional :: point_field_names(:)
        end subroutine type_vtk_vtk_initialize

        !> Initializes the vtk object by reading an XML-based VTU (.vtu) file.
        module subroutine type_vtk_vtu_initialize(self, file_name, global_node_id_key, node_type_key, num_sharing_ranks_key, &
                                                  owner_ranks_key, communication_partners_key, cell_id_key, rank_key, &
                                                  color_key, point_field_names)
            !> The vtk object to be initialized.
            class(type_vtk), intent(inout) :: self
            !> Path to the input .vtu file.
            character(*), intent(in) :: file_name
            !> Key for the point data array with global node IDs.
            character(*), intent(in), optional :: global_node_id_key
            !> Key for the point data array defining node types.
            character(*), intent(in), optional :: node_type_key
            !> Key for the point data array for the number of sharing ranks.
            character(*), intent(in), optional :: num_sharing_ranks_key
            !> Key for the point data array listing owner ranks.
            character(*), intent(in), optional :: owner_ranks_key
            !> Key for the point data array listing communication partners.
            character(*), intent(in), optional :: communication_partners_key
            !> Key for the cell data array with cell IDs.
            character(*), intent(in), optional :: cell_id_key
            !> Key for the cell data array specifying the owning rank.
            character(*), intent(in), optional :: rank_key
            !> Key for the cell data array for graph coloring.
            character(*), intent(in), optional :: color_key
            !> Names of additional point data fields to read.
            character(*), intent(in), optional :: point_field_names(:)
        end subroutine type_vtk_vtu_initialize
    end interface

contains
    !> Releases memory allocated by the C++ backend.
    subroutine finalize_vtk_object(self)
        !> The vtk object to be finalized.
        type(type_vtk), intent(inout) :: self

        if (c_associated(self%handle)) then

            select case (strip(self%reader_type))
            case ("vtk")
                call vtk_finalize(self%handle)
            case ("vtu")
                call vtu_finalize(self%handle)
            case default
                ! Do nothing for unknown reader types.
            end select

            self%handle = c_null_ptr

        end if
    end subroutine finalize_vtk_object

end module core_interop_vtk
