module inout_input_geometry
    use, intrinsic :: iso_fortran_env
!$  use :: omp_lib
    use :: mpi_f08
    use :: stdlib_strings, only:to_string, strip, ends_with
    use :: stdlib_logger
    use :: json_module, only:json_file
    use :: module_core, only:join, type_vtk
    use :: inout_input_base, only:get_json_value
    use :: inout_input_basic, only:type_input_basic
    use :: inout_input_conditions, only:type_conditions
    implicit none
    private

    public :: type_input_geometry

    type :: type_input_geometry
        type(type_vtk) :: vtk
        character(:), allocatable :: point_data_names(:)
    contains
        procedure, pass(self), public :: initialize => initialize_type_input_geometry
    end type type_input_geometry

contains

    !================================================================!
    ! メインの初期化サブルーチン
    !================================================================!
    subroutine initialize_type_input_geometry(self, path, input_basic, input_conditions)
        class(type_input_geometry), intent(inout) :: self
        character(len=*), intent(in) :: path
        class(type_input_basic), intent(in) :: input_basic
        class(type_conditions), intent(in) :: input_conditions

        character(:), allocatable :: fields_to_read(:)

        character(len=256) :: fullpath

        ! 1. 初期条件から、ファイルから読み込むべきフィールド名のリストを取得する
        !    (内部で basic の解析フラグをチェック)
        fields_to_read = collect_fields_from_conditions(input_basic, input_conditions)

        fullpath = trim(path)//'/'//trim(input_basic%geometry_settings%file_name)

        if (allocated(fields_to_read)) then
            if (ends_with(input_basic%geometry_settings%file_name, '.vtk')) then
                call self%vtk%initialize_vtk( &
                    file_name=strip(fullpath), &
                    global_node_id_key=strip(input_basic%geometry_settings%global_node_id_key), &
                    node_type_key=strip(input_basic%geometry_settings%node_type_key), &
                    num_sharing_ranks_key=strip(input_basic%geometry_settings%num_sharing_ranks_key), &
                    owner_ranks_key=strip(input_basic%geometry_settings%owner_ranks_key), &
                    communication_partners_key=strip(input_basic%geometry_settings%communication_partners_key), &
                    cell_id_key=strip(input_basic%geometry_settings%cell_id_key), &
                    rank_key=strip(input_basic%geometry_settings%rank_key), &
                    color_key=strip(input_basic%geometry_settings%color_key), &
                    point_field_names=fields_to_read)

            else if (ends_with(input_basic%geometry_settings%file_name, '.vtu')) then
                call self%vtk%initialize_vtu( &
                    file_name=strip(fullpath), &
                    global_node_id_key=strip(input_basic%geometry_settings%global_node_id_key), &
                    node_type_key=strip(input_basic%geometry_settings%node_type_key), &
                    num_sharing_ranks_key=strip(input_basic%geometry_settings%num_sharing_ranks_key), &
                    owner_ranks_key=strip(input_basic%geometry_settings%owner_ranks_key), &
                    communication_partners_key=strip(input_basic%geometry_settings%communication_partners_key), &
                    cell_id_key=strip(input_basic%geometry_settings%cell_id_key), &
                    rank_key=strip(input_basic%geometry_settings%rank_key), &
                    color_key=strip(input_basic%geometry_settings%color_key), &
                    point_field_names=fields_to_read)

            end if

            ! 読み込んだフィールド名を後で参照できるように保存
            allocate (self%point_data_names, source=fields_to_read)
            deallocate (fields_to_read)
        else
            if (ends_with(input_basic%geometry_settings%file_name, '.vtk')) then
                call self%vtk%initialize_vtk( &
                    file_name=strip(fullpath), &
                    global_node_id_key=strip(input_basic%geometry_settings%global_node_id_key), &
                    node_type_key=strip(input_basic%geometry_settings%node_type_key), &
                    num_sharing_ranks_key=strip(input_basic%geometry_settings%num_sharing_ranks_key), &
                    owner_ranks_key=strip(input_basic%geometry_settings%owner_ranks_key), &
                    communication_partners_key=strip(input_basic%geometry_settings%communication_partners_key), &
                    cell_id_key=strip(input_basic%geometry_settings%cell_id_key), &
                    rank_key=strip(input_basic%geometry_settings%rank_key), &
                    color_key=strip(input_basic%geometry_settings%color_key))

            else if (ends_with(input_basic%geometry_settings%file_name, '.vtu')) then
                call self%vtk%initialize_vtu( &
                    file_name=strip(fullpath), &
                    global_node_id_key=strip(input_basic%geometry_settings%global_node_id_key), &
                    node_type_key=strip(input_basic%geometry_settings%node_type_key), &
                    num_sharing_ranks_key=strip(input_basic%geometry_settings%num_sharing_ranks_key), &
                    owner_ranks_key=strip(input_basic%geometry_settings%owner_ranks_key), &
                    communication_partners_key=strip(input_basic%geometry_settings%communication_partners_key), &
                    cell_id_key=strip(input_basic%geometry_settings%cell_id_key), &
                    rank_key=strip(input_basic%geometry_settings%rank_key), &
                    color_key=strip(input_basic%geometry_settings%color_key))
            end if

        end if

    end subroutine initialize_type_input_geometry

    !================================================================!
    ! 初期条件オブジェクトを解析し、ユニークなフィールド名のリストを返すプライベート関数
    !================================================================!
    function collect_fields_from_conditions(basic, conditions) result(field_list)
        class(type_input_basic), intent(in) :: basic
        class(type_conditions), intent(in) :: conditions
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

end module inout_input_geometry
