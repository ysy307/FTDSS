module core_vtk
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding
    use :: mpi_f08
    use :: stdlib_strings, only:to_string, replace_all, strip
    use :: stdlib_sorting, only:sort
    use :: stdlib_logger
    use :: core_types, only:type_dp_3d
    use :: core_allocate, only:allocate_array
    use :: core_deallocate, only:deallocate_array
    use :: core_unique, only:unique
    use :: core_vtk_vtk_constants
    use :: core_vtk_vtk_wrapper
    use :: core_vtk_vtu_wrapper

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
        integer(int32) :: rank
        integer(int32) :: original_id
        integer(int32) :: color
    contains
        procedure :: set => type_vtk_cell_set
        procedure :: get_dimension => type_vtk_cell_get_dimension
        procedure :: get_order => type_vtk_cell_get_order
        procedure :: get_size => type_vtk_cell_get_size_connectivity
    end type type_vtk_cell

    type :: type_vtk
        character(:), allocatable :: format
        character(:), allocatable :: dataset
        ! VTK points data
        integer(int32) :: num_points = 0 ! Local number of points
        type(type_dp_3d) :: points
        ! VTK Cells data
        integer(int32) :: num_total_cells = 0 ! Local number of cells
        type(type_vtk_cell), allocatable :: cells(:)
        real(real64), allocatable :: point_field_values(:, :)

        integer(int32) :: my_rank = -1
        integer(int32) :: num_procs = -1
        integer(int32) :: global_num_points = 0
        integer(int32) :: global_num_total_cells = 0
        integer(int32), allocatable :: global_node_ids(:)
        integer(int32), allocatable :: node_type(:)
        integer(int32), allocatable :: num_sharing_ranks(:)
        integer(int32), allocatable :: owner_rank(:, :)
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
        module subroutine type_vtk_cell_set(self, num_nodes_in_cell)
            implicit none
            class(type_vtk_cell), intent(inout) :: self
            integer(int32), intent(in) :: num_nodes_in_cell
        end subroutine type_vtk_cell_set

        module function type_vtk_cell_get_dimension(self) result(dimension)
            implicit none
            class(type_vtk_cell), intent(in) :: self
            integer(int32) :: dimension
        end function type_vtk_cell_get_dimension

        module function type_vtk_cell_get_order(self) result(order)
            implicit none
            class(type_vtk_cell), intent(in) :: self
            integer(int32) :: order
        end function type_vtk_cell_get_order

        module function type_vtk_cell_get_size_connectivity(self) result(size)
            implicit none
            class(type_vtk_cell), intent(in) :: self
            integer(int32) :: size
        end function type_vtk_cell_get_size_connectivity

        module subroutine get_active_region_info(self, unique_ids, ierr)
            !> Extract unique CellEntityIds of the highest-dimensional elements
            implicit none
            class(Type_VTK), intent(in) :: self
            integer(int32), allocatable, intent(out) :: unique_ids(:)
            integer(int32), intent(out) :: ierr
        end subroutine get_active_region_info
    end interface

    interface
        module subroutine type_vtk_vtk_initialize(self, file_name, global_node_id_key, node_type_key, num_sharing_ranks_key, &
                                                  owner_ranks_key, communication_partners_key, cell_id_key, rank_key, &
                                                  original_id_key, color_key, point_field_names)
            !> Read VTK file using C++ backend with the handle pattern
            implicit none
            class(type_vtk), intent(inout) :: self
            character(*), intent(in) :: file_name
            character(*), intent(in), optional :: global_node_id_key
            character(*), intent(in), optional :: node_type_key
            character(*), intent(in), optional :: num_sharing_ranks_key
            character(*), intent(in), optional :: owner_ranks_key
            character(*), intent(in), optional :: communication_partners_key
            character(*), intent(in), optional :: cell_id_key
            character(*), intent(in), optional :: rank_key
            character(*), intent(in), optional :: original_id_key
            character(*), intent(in), optional :: color_key
            character(*), intent(in), optional :: point_field_names(:)
        end subroutine type_vtk_vtk_initialize

        module subroutine type_vtk_vtu_initialize(self, file_name, global_node_id_key, node_type_key, num_sharing_ranks_key, &
                                                  owner_ranks_key, communication_partners_key, cell_id_key, rank_key, &
                                                  original_id_key, color_key, point_field_names)
            !> Read VTU file using C++ backend with the handle pattern
            implicit none
            class(type_vtk), intent(inout) :: self
            character(*), intent(in) :: file_name
            character(*), intent(in), optional :: global_node_id_key
            character(*), intent(in), optional :: node_type_key
            character(*), intent(in), optional :: num_sharing_ranks_key
            character(*), intent(in), optional :: owner_ranks_key
            character(*), intent(in), optional :: communication_partners_key
            character(*), intent(in), optional :: cell_id_key
            character(*), intent(in), optional :: rank_key
            character(*), intent(in), optional :: original_id_key
            character(*), intent(in), optional :: color_key
            character(*), intent(in), optional :: point_field_names(:)
        end subroutine type_vtk_vtu_initialize
    end interface

contains
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
