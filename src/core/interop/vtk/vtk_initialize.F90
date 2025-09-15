submodule(core_vtk) core_vtk_vtk_initialize
    implicit none
contains
    module subroutine type_vtk_vtk_initialize(self, file_name, global_node_id_key, node_type_key, num_sharing_ranks_key, &
                                              owner_ranks_key, communication_partners_key, cell_id_key, rank_key, &
                                              original_id_key, color_key)
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

        ! =================================================================
        ! 公開APIの実装 (内部をC++呼び出しに置き換え)
        ! =================================================================
!     subroutine type_vtk_vtk_initialize(self, file_name, cell_id_key, global_node_id_name, point_field_names, point_field_values)
!         !> Read VTK file using C++ backend with the handle pattern
!         implicit none
!         class(type_vtk), intent(inout) :: self
!         character(*), intent(in) :: file_name
!         character(*), intent(in) :: cell_id_key
!         ! MODIFIED: Added arguments for array names
!         character(*), intent(in), optional :: global_node_id_name
!         character(*), intent(in), optional :: point_field_names(:)
!         ! MODIFIED: Changed to 3D array to support multi-component data
!         real(real64), intent(inout), allocatable, optional :: point_field_values(:, :, :)

!         ! --- ローカル変数 ---
!         character(len=256) :: c_file_name
!         character(len=256) :: c_array_name
!         integer(c_int) :: ierr
!         integer(int32) :: i

!         ! --- 生データ格納用の一時配列 ---
!         integer(int64), allocatable :: raw_connectivity(:)
!         integer(int64), allocatable :: raw_offsets(:)
!         integer(int32), allocatable :: raw_cell_types(:)
!         integer(int32), allocatable :: raw_cell_entity_ids(:)
!         integer(int64) :: total_conn_size
!         integer(int32) :: connectivity_first, connectivity_last, num_nodes_in_cell
!         character(50, kind=c_char) :: f_format, f_dataset
!         integer(c_int) :: len_f_format, len_f_dataset

!         ! --- For field data reading ---
!         integer(c_int) :: num_components, max_components
!         real(real64), allocatable :: temp_field_data(:)
!         integer(int32) :: j, k

! #ifdef _MPI
!         integer(int32) :: global_max_node_id, local_max_node_id
!         integer(int32) :: my_rank
!         integer(int32) :: dot_pos
! #endif

!         !----------------------------------------------------------------!
!         ! 1. C++リーダーの初期化とハンドルの取得
!         !----------------------------------------------------------------!
!         self%reader_type = "vtk"
!         if (c_associated(self%handle)) then
!             call vtk_finalize(self%handle)
!             self%handle = c_null_ptr
!         end if

! #ifdef _MPI
!         call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)
!         c_file_name = replace_all(strip(file_name), pattern="@RANK@", replacement=to_string(my_rank))//c_null_char
! #else
!         c_file_name = trim(file_name)//c_null_char
! #endif

!         self%handle = vtk_initialize(c_file_name, ierr)
!         if (.not. c_associated(self%handle) .or. ierr /= 0) then
!             write (*, *) "C++ VTK Reader failed to initialize for file: ", trim(c_file_name)
!             write (*, *) "Error code: ", ierr
!             stop "VTK Initialization Failed"
!         end if

!         !----------------------------------------------------------------!
!         ! 2. ハンドルを使って各種データを取得
!         !----------------------------------------------------------------!
!         len_f_dataset = 50
!         len_f_format = 50
!         call vtk_read_header(self%handle, f_format, len_f_dataset, f_dataset, len_f_format)
!         allocate (character(len=len_trim(f_format)) :: self%format)
!         self%format = strip(f_format)
!         allocate (character(len=len_trim(f_dataset)) :: self%dataset)
!         self%dataset = strip(f_dataset)

!         call vtk_get_num_points(self%handle, self%num_points)
!         if (self%num_points > 0) then
!             call self%points%initialize(self%num_points)
!             call vtk_get_points(self%handle, self%points%x, self%points%y, self%points%z)
!         end if

!         call vtk_get_num_cells(self%handle, self%num_total_cells)
!         if (self%num_total_cells > 0) then
!             call vtk_get_total_connectivity_size(self%handle, total_conn_size)
!             call allocate_array(raw_connectivity, total_conn_size)
!             call allocate_array(raw_offsets, self%num_total_cells + 1_int64)
!             call allocate_array(raw_cell_types, self%num_total_cells)
!             call allocate_array(raw_cell_entity_ids, self%num_total_cells)

!             call vtk_get_cell_info(self%handle, raw_connectivity, raw_offsets, raw_cell_types)
!             c_array_name = trim(cell_id_key)//c_null_char
!             call vtk_get_cell_data_int32(self%handle, c_array_name, raw_cell_entity_ids)

!             allocate (self%cells(self%num_total_cells))
!             do i = 1, self%num_total_cells
!                 self%cells(i)%cell_type = raw_cell_types(i)
!                 self%cells(i)%cell_entity_id = raw_cell_entity_ids(i)
!                 connectivity_first = raw_offsets(i) + 1
!                 connectivity_last = raw_offsets(i + 1)
!                 num_nodes_in_cell = connectivity_last - connectivity_first + 1
!                 call allocate_array(self%cells(i)%connectivity, num_nodes_in_cell)
!                 self%cells(i)%connectivity(:) = int(raw_connectivity(connectivity_first:connectivity_last), kind=int32) + 1
!                 call self%cells(i)%set(num_nodes_in_cell)
!             end do
!         end if

!         ! MODIFIED: Reworked logic to handle multi-component point data
!         if (present(point_field_names) .and. present(point_field_values)) then
!             if (self%num_points > 0 .and. size(point_field_names) > 0) then
!                 max_components = 0
!                 do i = 1, size(point_field_names)
!                     c_array_name = trim(point_field_names(i))//c_null_char
!                     call vtk_get_num_point_data_components(self%handle, c_array_name, num_components)
!                     max_components = max(max_components, num_components)
!                 end do

!                 if (max_components > 0) then
!                     allocate (point_field_values(self%num_points, max_components, size(point_field_names)))
!                     point_field_values = 0.0_real64

!                     do i = 1, size(point_field_names)
!                         c_array_name = trim(point_field_names(i))//c_null_char
!                         call vtk_get_num_point_data_components(self%handle, c_array_name, num_components)
!                         if (num_components > 0) then
!                             allocate (temp_field_data(self%num_points * num_components))
!                             call vtk_get_point_data_float64(self%handle, c_array_name, temp_field_data)
!                             do j = 1, self%num_points
!                                 do k = 1, num_components
!                                     point_field_values(j, k, i) = temp_field_data((j - 1) * num_components + k)
!                                 end do
!                             end do
!                             deallocate (temp_field_data)
!                         end if
!                     end do
!                 else
!                     allocate (point_field_values(self%num_points, 0, size(point_field_names)))
!                 end if
!             end if
!         end if

! #ifdef _MPI
!         ! --- 3. MPI: グローバル情報の計算とインデックスの更新 ---
!         call MPI_Comm_rank(MPI_COMM_WORLD, self%my_rank, ierr)
!         call MPI_Comm_size(MPI_COMM_WORLD, self%num_procs, ierr)

!         if (present(global_node_id_name)) then
!             if (self%num_points > 0) then
!                 allocate (self%global_node_ids(self%num_points))
!                 c_array_name = trim(global_node_id_name)//c_null_char
!                 call vtk_get_point_data_int32(self%handle, c_array_name, self%global_node_ids)
!                 self%global_node_ids = self%global_node_ids + 1
!             end if
!         else
!             write (*, '(A)') "Warning: MPI mode is active but 'global_node_id_name' was not provided."
!         end if

!         if (allocated(self%global_node_ids)) then
!             if (size(self%global_node_ids) > 0) then
!                 local_max_node_id = maxval(self%global_node_ids)
!             else
!                 local_max_node_id = 0
!             end if
!         else
!             local_max_node_id = 0
!         end if

!         call MPI_Allreduce(local_max_node_id, global_max_node_id, 1, MPI_INTEGER4, MPI_MAX, MPI_COMM_WORLD, ierr)
!         self%global_num_points = global_max_node_id
!         call MPI_Allreduce(self%num_total_cells, self%global_num_total_cells, 1, MPI_INTEGER4, MPI_SUM, MPI_COMM_WORLD, ierr)

!         if (allocated(self%global_node_ids)) then
!             if (self%num_total_cells > 0 .and. self%num_points > 0) then
!                 do i = 1, self%num_total_cells
!                     self%cells(i)%connectivity(:) = self%global_node_ids(self%cells(i)%connectivity(:))
!                 end do
!             end if
!         end if
! #endif

        !     call deallocate_array(raw_connectivity)
        !     call deallocate_array(raw_offsets)
        !     call deallocate_array(raw_cell_types)
        !     call deallocate_array(raw_cell_entity_ids)

        ! end subroutine type_vtk_vtk_initialize

!

    end subroutine type_vtk_vtk_initialize
end submodule core_vtk_initialize
