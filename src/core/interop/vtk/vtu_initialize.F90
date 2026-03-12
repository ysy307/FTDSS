submodule(core_interop_vtk) core_vtk_vtu_initialize
    implicit none
contains
    module subroutine type_vtk_vtu_initialize(self, file_name, global_node_id_key, node_type_key, num_sharing_ranks_key, &
                                              owner_ranks_key, communication_partners_key, cell_id_key, rank_key, &
                                              color_key, point_field_names)
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
        character(*), intent(in), optional :: color_key
        character(*), intent(in), optional :: point_field_names(:)

        ! --- Local variables ---
        character(len=256) :: c_file_name
        character(len=256) :: c_array_name
        integer(c_int) :: ierr
        integer(int32) :: i
        character(50, kind=c_char) :: f_format, f_dataset
        integer(c_int) :: len_f_format, len_f_dataset
        integer(int64) :: total_conn_size
        integer(int32) :: connectivity_first, connectivity_last, num_nodes_in_cell

        ! --- Temporary arrays for raw data storage ---
        integer(int64), allocatable :: raw_connectivity(:)
        integer(int64), allocatable :: raw_offsets(:)
        integer(int32), allocatable :: raw_cell_types(:)
        integer(int32), allocatable :: raw_cell_entity_ids(:)
        integer(int32), allocatable :: raw_ranks(:)
        integer(int32) :: local_max_node_id, global_max_node_id
        integer(int32), allocatable :: raw_colors(:)

        !----------------------------------------------------------------!
        ! 1. Initialize C++ reader and obtain handle
        !----------------------------------------------------------------!
        self%reader_type = "vtu"
        if (c_associated(self%handle)) then
            call vtu_finalize(self%handle)
            self%handle = c_null_ptr
        end if

        call MPI_Comm_rank(MPI_COMM_WORLD, self%my_rank, ierr)
        call MPI_Comm_size(MPI_COMM_WORLD, self%num_procs, ierr)
        c_file_name = replace_all(strip(file_name), pattern="@RANK@", replacement=to_string(self%my_rank))//c_null_char

        self%handle = vtu_initialize(c_file_name, ierr)
        if (.not. c_associated(self%handle) .or. ierr /= 0) then
            call global_logger%log_error("C++ VTU Reader failed to initialize for file: "//strip(c_file_name))
            call global_logger%log_error("Error code: "//to_string(ierr))
            stop "VTU Initialization Failed"
        end if

        !----------------------------------------------------------------!
        ! 2. Read header information
        !----------------------------------------------------------------!
        len_f_dataset = 50
        len_f_format = 50
        call vtu_read_header(self%handle, f_format, len_f_dataset, f_dataset, len_f_format)
        allocate (character(len=len_trim(f_format)) :: self%format)
        self%format = strip(f_format)
        allocate (character(len=len_trim(f_dataset)) :: self%dataset)
        self%dataset = strip(f_dataset)

        !----------------------------------------------------------------!
        ! 3. Retrieve point data
        !----------------------------------------------------------------!
        call vtu_get_num_points(self%handle, self%num_points)
        if (self%num_points > 0) then
            call self%points%initialize(self%num_points)
            call vtu_get_points(self%handle, self%points%x, self%points%y, self%points%z)

            if (present(global_node_id_key)) then
                call allocate_array(self%global_node_ids, self%num_points)
                c_array_name = strip(global_node_id_key)//c_null_char
                call vtu_get_point_data_int32(self%handle, c_array_name, self%global_node_ids)
                self%global_node_ids = self%global_node_ids + 1 ! 0-based to 1-based
            end if

            if (present(node_type_key)) then
                call allocate_array(self%node_type, self%num_points)
                c_array_name = strip(node_type_key)//c_null_char
                call vtu_get_point_data_int32(self%handle, c_array_name, self%node_type)
            end if

            if (present(num_sharing_ranks_key)) then
                call allocate_array(self%num_sharing_ranks, self%num_points)
                c_array_name = strip(num_sharing_ranks_key)//c_null_char
                call vtu_get_point_data_int32(self%handle, c_array_name, self%num_sharing_ranks)
            end if

            if (present(owner_ranks_key)) then
                call allocate_array(self%owner_rank, self%num_procs, self%num_points)
                c_array_name = strip(owner_ranks_key)//c_null_char
                call vtu_get_point_data_int32(self%handle, c_array_name, self%owner_rank)
            end if

            if (present(communication_partners_key)) then
                call allocate_array(self%communication_partners, self%num_procs, self%num_points)
                c_array_name = strip(communication_partners_key)//c_null_char
                call vtu_get_point_data_int32(self%handle, c_array_name, self%communication_partners)
            end if

            if (present(point_field_names)) then
                if (size(point_field_names) > 0) then
                    call allocate_array(self%point_field_values, self%num_points, size(point_field_names))
                    do i = 1, size(point_field_names)
                        c_array_name = strip(point_field_names(i))//c_null_char
                        call vtu_get_point_data_float64(self%handle, c_array_name, self%point_field_values(:, i))
                    end do
                end if
            end if
        end if

        !----------------------------------------------------------------!
        ! 4. Retrieve cell data
        !----------------------------------------------------------------!
        call vtu_get_num_cells(self%handle, self%num_total_cells)
        if (self%num_total_cells > 0) then
            call vtu_get_total_connectivity_size(self%handle, total_conn_size)
            call allocate_array(raw_connectivity, total_conn_size)
            call allocate_array(raw_offsets, self%num_total_cells + 1_int64)
            call allocate_array(raw_cell_types, self%num_total_cells)
            call vtu_get_cell_info(self%handle, raw_connectivity, raw_offsets, raw_cell_types)

            call allocate_array(raw_cell_entity_ids, self%num_total_cells)
            if (present(cell_id_key)) then
                c_array_name = strip(cell_id_key)//c_null_char
                call vtu_get_cell_data_int32(self%handle, c_array_name, raw_cell_entity_ids)
            else
                raw_cell_entity_ids = 0
            end if

            if (present(rank_key)) then
                call allocate_array(raw_ranks, self%num_total_cells)
                c_array_name = strip(rank_key)//c_null_char
                call vtu_get_cell_data_int32(self%handle, c_array_name, raw_ranks)
            end if

            if (present(color_key)) then
                call allocate_array(raw_colors, self%num_total_cells)
                c_array_name = strip(color_key)//c_null_char
                call vtu_get_cell_data_int32(self%handle, c_array_name, raw_colors)
            end if

            allocate (self%cells(self%num_total_cells))
            do i = 1, self%num_total_cells
                self%cells(i)%cell_type = raw_cell_types(i)
                self%cells(i)%cell_entity_id = raw_cell_entity_ids(i)

                connectivity_first = int(raw_offsets(i), int32) + 1
                connectivity_last = int(raw_offsets(i + 1), int32)
                num_nodes_in_cell = connectivity_last - connectivity_first + 1
                call allocate_array(self%cells(i)%connectivity, num_nodes_in_cell)
                self%cells(i)%connectivity(:) = int(raw_connectivity(connectivity_first:connectivity_last), kind=int32) + 1
                call self%cells(i)%set(num_nodes_in_cell)

                if (allocated(raw_ranks)) self%cells(i)%rank = raw_ranks(i)
                if (allocated(raw_colors)) self%cells(i)%color = raw_colors(i)
            end do
        end if

        if (allocated(self%global_node_ids)) then
            if (size(self%global_node_ids) > 0) then
                local_max_node_id = maxval(self%global_node_ids)
            else
                local_max_node_id = 0
            end if
            call MPI_Allreduce(local_max_node_id, global_max_node_id, 1, MPI_INTEGER4, MPI_MAX, MPI_COMM_WORLD, ierr)
            self%global_num_points = global_max_node_id
        end if

        call MPI_Allreduce(self%num_total_cells, self%global_num_total_cells, 1, MPI_INTEGER4, MPI_SUM, MPI_COMM_WORLD, ierr)

        !----------------------------------------------------------------!
        ! 6. Cleanup: deallocate temporary arrays to prevent memory leaks
        !----------------------------------------------------------------!
        call deallocate_array(raw_connectivity)
        call deallocate_array(raw_offsets)
        call deallocate_array(raw_cell_types)
        call deallocate_array(raw_cell_entity_ids)
        call deallocate_array(raw_ranks)
        call deallocate_array(raw_colors)

    end subroutine type_vtk_vtu_initialize

end submodule core_vtk_vtu_initialize
