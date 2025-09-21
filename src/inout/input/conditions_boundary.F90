submodule(inout_input_conditions) inout_input_conditions_boundry
    implicit none
    !------------------------------------------------------------------------------------------------------------------------------
    ! JSON key names for boundary conditions
    !------------------------------------------------------------------------------------------------------------------------------
    character(*), parameter :: boundary_conditions = "boundary_conditions"
    character(*), parameter :: id = "id"
    character(*), parameter :: calculate_thermal = "calculate_thermal"
    character(*), parameter :: calculate_hydraulic = "calculate_hydraulic"
    character(*), parameter :: calculate_mechanical = "calculate_mechanical"
    character(*), parameter :: thermal = "thermal"
    character(*), parameter :: hydraulic = "hydraulic"
    character(*), parameter :: type = "type"
    character(*), parameter :: is_uniform = "is_uniform"
    character(*), parameter :: values = "values"
    character(len=16), parameter :: valid_thermal_boundary_types(8) = [character(len=16) :: &
                                                                       "dirichlet", "neumann", "flux", "robin", "adiabatic", "free", "convective", "head_radiation"]
    character(len=16), parameter :: valid_hydraulic_boundary_types(5) = [character(len=16) :: &
                                                                         "dirichlet", "neumann", "flux", "impermeable", "seepage"]

contains

    module subroutine read_conditions_boundary_conditions(self, json)
        implicit none
        class(type_conditions), intent(inout) :: self
        type(json_file), intent(inout) :: json
        logical :: found
        integer :: i
        character(256) :: buffer(2)

        call json%info(boundary_conditions, found=found, n_children=self%num_boundaries)
        if (.not. found .or. self%num_boundaries <= 0) then
            call error_message(904, c_opt=boundary_conditions)
        end if

        if (allocated(self%boundary_conditions)) deallocate (self%boundary_conditions)
        allocate (self%boundary_conditions(self%num_boundaries))

        do i = 1, self%num_boundaries
            ! 各境界条件オブジェクトへのパスを設定 e.g., "boundary_conditions(1)"
            buffer(1) = boundary_conditions//"("//to_string(i)//")"

            ! 必須のIDを読み込む
            buffer(2) = id
            call get_json_value(json, join(buffer), self%boundary_conditions(i)%id, is_required=.true.)

            ! ## 修正点: 先にT/Fフラグを読み込む ##
            buffer(2) = calculate_thermal
            call get_json_value(json, join(buffer), self%boundary_conditions(i)%calculate_thermal, is_required=.true.)

            buffer(2) = calculate_hydraulic
            call get_json_value(json, join(buffer), self%boundary_conditions(i)%calculate_hydraulic, is_required=.true.)

            buffer(2) = calculate_mechanical
            call get_json_value(json, join(buffer), self%boundary_conditions(i)%calculate_mechanical, is_required=.true.)

            ! ## 修正点: フラグの値に基づいて分岐 ##
            if (self%boundary_conditions(i)%calculate_thermal) then
                buffer(2) = thermal
                call read_conditions_boundary_conditions_local( &
                    self%boundary_conditions(i)%thermal, json, buffer, 2, &
                    valid_thermal_boundary_types, size(self%time_control%boundary_time_points))
            end if

            if (self%boundary_conditions(i)%calculate_hydraulic) then
                buffer(2) = hydraulic
                call read_conditions_boundary_conditions_local( &
                    self%boundary_conditions(i)%hydraulic, json, buffer, 2, &
                    valid_hydraulic_boundary_types, size(self%time_control%boundary_time_points))
            end if

            ! 機械特性は将来的な実装用
            if (self%boundary_conditions(i)%calculate_mechanical) then
                ! (ここに機械特性の読み込み処理を追加)
            end if
        end do
    end subroutine read_conditions_boundary_conditions

    ! ------------------------------------------------------------------
    ! NOTE: ヘルパーサブルーチン read_conditions_boundary_conditions_local
    ! は、この変更による修正は不要です。
    ! ------------------------------------------------------------------
    subroutine read_conditions_boundary_conditions_local(boundary, json, buffer, end_index, valid_types, num_time_points)
        implicit none
        type(type_boundary_local), intent(inout) :: boundary
        type(json_file), intent(inout) :: json
        character(*), intent(in) :: buffer(:)
        integer, intent(in) :: end_index
        character(len=*), intent(in) :: valid_types(:)
        integer, intent(in) :: num_time_points
        character(len=256), allocatable :: local_buffer(:)

        allocate (local_buffer(size(buffer) + 1))
        local_buffer(1:end_index) = buffer(1:end_index)

        local_buffer(end_index + 1) = type
        call get_json_value(json, join(local_buffer), boundary%type, is_required=.true., valid_list=valid_types)

        select case (trim(boundary%type))
        case ("dirichlet", "neumann", "flux", "robin", "heat_transfer", "head_radiation")
            local_buffer(end_index + 1) = values
            call get_json_value(json, join(local_buffer), boundary%values, &
                                is_required=.true., array_size=num_time_points)

        case ("adiabatic", "impermeable", "free")
            ! これらのタイプは値を必要としないため、配列が割り当てられていれば解放する
            if (allocated(boundary%values)) deallocate (boundary%values)

        case default
            call error_message(999, c_opt="Boundary condition type '"//trim(boundary%type)//"' is not yet implemented.")
        end select
    end subroutine read_conditions_boundary_conditions_local

    module subroutine display_boundary_conditions(self)
        implicit none
        class(type_boundary_conditions), intent(in) :: self

        write (*, '(a, i0, a)') "  ■ Boundary Condition (ID: ", self%id, ") -------------------"

        call display_boundary_local(self%thermal, "Thermal")
        call display_boundary_local(self%hydraulic, "Hydraulic")

    end subroutine display_boundary_conditions

    subroutine display_boundary_local(boundary, title)
        implicit none
        type(type_boundary_local), intent(in) :: boundary
        character(*), intent(in) :: title
        integer :: n_vals

        if (len(trim(boundary%type)) == 0) return ! 未定義の場合は表示しない

        write (*, '(a, a, a)') "    --- ", trim(title), " ---"
        write (*, '(a, a)') "      Type                : ", trim(boundary%type)

        select case (trim(boundary%type))
        case ("dirichlet", "neumann", "flux", "robin", "heat_transfer", "head_radiation")
            if (allocated(boundary%values)) then
                n_vals = size(boundary%values)
                if (n_vals == 0) then
                    write (*, '(a)') "      Values              : (0 points defined)"
                else if (n_vals <= 6) then
                    write (*, '(a, *(es12.4e2, :, " "))') "      Values              : ", boundary%values
                else
                    write (*, '(a, 3(es12.4e2, :, " "), a, 3(es12.4e2, :, " "))') &
                        "      Values (summary)    : ", boundary%values(1:3), " ... ", boundary%values(n_vals - 2:n_vals)
                end if
            else
                write (*, '(a)') "      Values              : Not allocated"
            end if
        case default
            ! "adiabatic"のようなタイプは追加フィールドなし
        end select
    end subroutine display_boundary_local

end submodule inout_input_conditions_boundry
