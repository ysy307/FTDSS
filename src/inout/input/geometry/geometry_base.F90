submodule(inout_input_geometry) inout_input_geometry_base
    use :: inout_input
    implicit none
contains
!================================================================!
    ! メインの初期化サブルーチン
    !================================================================!
    module subroutine initialize_type_input_geometry(self)
        implicit none
        class(type_input_geometry), intent(inout) :: self

        character(:), allocatable :: fields_to_read(:)

        character(len=256) :: fullpath

        select type (p => self%parent)
        type is (type_input)

            ! 1. 初期条件から、ファイルから読み込むべきフィールド名のリストを取得する
            !    (内部で basic の解析フラグをチェック)
            fields_to_read = self%collect_fields_from_conditions()

            fullpath = trim(p%input_path)//trim(p%basic%geometry_settings%file_name)

            if (allocated(fields_to_read)) then
                if (ends_with(p%basic%geometry_settings%file_name, '.vtk')) then
                    call self%vtk%initialize_vtk( &
                        file_name=strip(fullpath), &
                        global_node_id_key=strip(p%basic%geometry_settings%global_node_id_key), &
                        node_type_key=strip(p%basic%geometry_settings%node_type_key), &
                        num_sharing_ranks_key=strip(p%basic%geometry_settings%num_sharing_ranks_key), &
                        owner_ranks_key=strip(p%basic%geometry_settings%owner_ranks_key), &
                        communication_partners_key=strip(p%basic%geometry_settings%communication_partners_key), &
                        cell_id_key=strip(p%basic%geometry_settings%cell_id_key), &
                        rank_key=strip(p%basic%geometry_settings%rank_key), &
                        color_key=strip(p%basic%geometry_settings%color_key), &
                        point_field_names=fields_to_read)

                else if (ends_with(p%basic%geometry_settings%file_name, '.vtu')) then
                    call self%vtk%initialize_vtu( &
                        file_name=strip(fullpath), &
                        global_node_id_key=strip(p%basic%geometry_settings%global_node_id_key), &
                        node_type_key=strip(p%basic%geometry_settings%node_type_key), &
                        num_sharing_ranks_key=strip(p%basic%geometry_settings%num_sharing_ranks_key), &
                        owner_ranks_key=strip(p%basic%geometry_settings%owner_ranks_key), &
                        communication_partners_key=strip(p%basic%geometry_settings%communication_partners_key), &
                        cell_id_key=strip(p%basic%geometry_settings%cell_id_key), &
                        rank_key=strip(p%basic%geometry_settings%rank_key), &
                        color_key=strip(p%basic%geometry_settings%color_key), &
                        point_field_names=fields_to_read)

                end if

                ! 読み込んだフィールド名を後で参照できるように保存
                allocate (self%point_data_names, source=fields_to_read)
                deallocate (fields_to_read)
            else
                if (ends_with(p%basic%geometry_settings%file_name, '.vtk')) then
                    call self%vtk%initialize_vtk( &
                        file_name=strip(fullpath), &
                        global_node_id_key=strip(p%basic%geometry_settings%global_node_id_key), &
                        node_type_key=strip(p%basic%geometry_settings%node_type_key), &
                        num_sharing_ranks_key=strip(p%basic%geometry_settings%num_sharing_ranks_key), &
                        owner_ranks_key=strip(p%basic%geometry_settings%owner_ranks_key), &
                        communication_partners_key=strip(p%basic%geometry_settings%communication_partners_key), &
                        cell_id_key=strip(p%basic%geometry_settings%cell_id_key), &
                        rank_key=strip(p%basic%geometry_settings%rank_key), &
                        color_key=strip(p%basic%geometry_settings%color_key))

                else if (ends_with(p%basic%geometry_settings%file_name, '.vtu')) then
                    call self%vtk%initialize_vtu( &
                        file_name=strip(fullpath), &
                        global_node_id_key=strip(p%basic%geometry_settings%global_node_id_key), &
                        node_type_key=strip(p%basic%geometry_settings%node_type_key), &
                        num_sharing_ranks_key=strip(p%basic%geometry_settings%num_sharing_ranks_key), &
                        owner_ranks_key=strip(p%basic%geometry_settings%owner_ranks_key), &
                        communication_partners_key=strip(p%basic%geometry_settings%communication_partners_key), &
                        cell_id_key=strip(p%basic%geometry_settings%cell_id_key), &
                        rank_key=strip(p%basic%geometry_settings%rank_key), &
                        color_key=strip(p%basic%geometry_settings%color_key))
                end if

            end if

        end select

    end subroutine initialize_type_input_geometry

    !================================================================!
    ! 初期条件オブジェクトを解析し、ユニークなフィールド名のリストを返すプライベート関数
    !================================================================!
    module function collect_fields_from_conditions(self) result(field_list)
        implicit none
        class(type_input_geometry), intent(inout) :: self
        character(:), allocatable :: field_list(:)

        character(len=256) :: temp_list(2) ! 熱と水理の最大2つを仮定
        integer :: num_fields, i
        logical :: is_duplicate

        num_fields = 0

        ! 熱解析が有効な場合のみ、熱の初期条件をチェック
        if (basic%analysis_controls%calculate_thermal) then
            if (conditions%initial_conditions%thermal%type == "file") then
                if (allocated(conditions%initial_conditions%thermal%field_name)) then
                    num_fields = 1
                    temp_list(1) = conditions%initial_conditions%thermal%field_name
                end if
            end if
        end if

        ! 水理解析が有効な場合のみ、水理の初期条件をチェック
        if (basic%analysis_controls%calculate_hydraulic) then
            if (conditions%initial_conditions%hydraulic%type == "file") then
                if (allocated(conditions%initial_conditions%hydraulic%field_name)) then
                    ! 重複をチェック
                    is_duplicate = .false.
                    do i = 1, num_fields
                        if (trim(temp_list(i)) == trim(conditions%initial_conditions%hydraulic%field_name)) then
                            is_duplicate = .true.
                            exit
                        end if
                    end do

                    if (.not. is_duplicate) then
                        num_fields = num_fields + 1
                        temp_list(num_fields) = conditions%initial_conditions%hydraulic%field_name
                    end if
                end if
            end if
        end if

        ! 収集したフィールド名で戻り値の配列を確保
        if (num_fields > 0) then
            allocate (character(len=256) :: field_list(num_fields))
            field_list = temp_list(1:num_fields)
        end if

    end function collect_fields_from_conditions

end submodule inout_input_geometry_base
