submodule(inout_input) inout_input_geometry
    implicit none

contains

    module subroutine inout_read_geometry(self)
        !> Load the geometry and any required initial condition fields from the VTK/VTU file
        implicit none
        class(type_input), intent(inout) :: self

        ! --- ローカル変数 ---
        ! 読み込むべきフィールド名を一時的に格納するリスト
        character(:), allocatable, dimension(:) :: fields_to_read
        ! 読み込むフィールドの数をカウント
        integer :: num_fields_to_read
        ! 重複を避けつつフィールド名を集めるための一時的な固定長配列
        character(len=256), dimension(3) :: tmp_field_list
        integer :: i

        !----------------------------------------------------------------!
        ! 1. 初期条件の設定をチェックし、ファイルから読み込むべきフィールド名を集める
        !----------------------------------------------------------------!
        num_fields_to_read = 0

        ! 熱の初期条件をチェック
        if (allocated(self%conditions%initial_conditions%thermal%type)) then
            if (self%conditions%initial_conditions%thermal%type == "file") then
                if (allocated(self%conditions%initial_conditions%thermal%field_name)) then
                    num_fields_to_read = num_fields_to_read + 1
                    tmp_field_list(num_fields_to_read) = self%conditions%initial_conditions%thermal%field_name
                end if
            end if
        end if

        ! 水理の初期条件をチェック
        if (allocated(self%conditions%initial_conditions%hydraulic%type)) then
            if (self%conditions%initial_conditions%hydraulic%type == "file") then
                if (allocated(self%conditions%initial_conditions%hydraulic%field_name)) then
                    ! 重複を避ける
                    if (num_fields_to_read > 0) then
                        if (trim(tmp_field_list(1)) /= trim(self%conditions%initial_conditions%hydraulic%field_name)) then
                            num_fields_to_read = num_fields_to_read + 1
                            tmp_field_list(num_fields_to_read) = self%conditions%initial_conditions%hydraulic%field_name
                        end if
                    else
                        num_fields_to_read = num_fields_to_read + 1
                        tmp_field_list(num_fields_to_read) = self%conditions%initial_conditions%hydraulic%field_name
                    end if
                end if
            end if
        end if

        !----------------------------------------------------------------!
        ! 2. 読み込むべきフィールドがあれば、それを渡してジオメトリリーダーを呼び出す
        !----------------------------------------------------------------!
        if (num_fields_to_read > 0) then
            ! 渡すための可変長配列を確保
            allocate (character(len=256) :: fields_to_read(num_fields_to_read))
            do i = 1, num_fields_to_read
                fields_to_read(i) = trim(tmp_field_list(i))
            end do

            ! ファイルの拡張子に応じて適切なリーダーを呼び出す
            if (ends_with(self%geometry_file_name, '.vtk')) then
                ! field_names と field_values を渡して初期化
                call self%geometry%vtk%initialize_vtk(self%geometry_file_name, &
                                                      self%basic%geometry_settings%cell_id_array_name, &
                                                      field_names=fields_to_read, &
                                                      field_values=self%geometry%initial_values)
            else if (ends_with(self%geometry_file_name, '.vtu')) then
                call self%geometry%vtk%initialize_vtu(self%geometry_file_name, &
                                                      self%basic%geometry_settings%cell_id_array_name, &
                                                      field_names=fields_to_read, &
                                                      field_values=self%geometry%initial_values)
            else
                call error_message(906, c_opt=self%geometry_file_name)
            end if

            ! 読み込んだフィールド名を後で参照できるように保存
            allocate (self%geometry%point_data_names, source=fields_to_read)
            deallocate (fields_to_read)

        else
            ! 読み込むべきフィールドがない場合は、通常通りジオメトリだけを読み込む
            if (ends_with(self%geometry_file_name, '.vtk')) then
                call self%geometry%vtk%initialize_vtk(self%geometry_file_name, self%basic%geometry_settings%cell_id_array_name)
            else if (ends_with(self%geometry_file_name, '.vtu')) then
                call self%geometry%vtk%initialize_vtu(self%geometry_file_name, self%basic%geometry_settings%cell_id_array_name)
            else
                call error_message(906, c_opt=self%geometry_file_name)
            end if
        end if

        ! debug
        print *, "Geometry file read successfully:", self%geometry_file_name
        print *, "Number of initial condition fields read:", size(self%geometry%point_data_names)
        if (allocated(self%geometry%point_data_names)) then
            print *, "Fields:", self%geometry%point_data_names
        else
            print *, "No initial condition fields read."
        end if
        print *, "Total number of cells in geometry:", self%geometry%vtk%num_total_cells
        print *, "Total number of points in geometry:", self%geometry%vtk%num_points
        print *, "Geometry initialization complete."
        if (allocated(self%geometry%initial_values)) then
            print *, "Initial values array size:", size(self%geometry%initial_values)
        else
            print *, "No initial values array allocated."
        end if
        stop

    end subroutine inout_read_geometry
end submodule inout_input_geometry
