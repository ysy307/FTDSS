submodule(inout_input_conditions) inout_input_conditions_boundry
    use :: inout_input
    implicit none
    !------------------------------------------------------------------------------------------------------------------------------
    ! JSON key names for boundary conditions
    !------------------------------------------------------------------------------------------------------------------------------
    character(*), parameter :: boundary_conditions = "boundary_conditions"
    character(*), parameter :: id = "id"
    character(*), parameter :: calculate_thermal = "calculate_thermal"
    character(*), parameter :: calculate_hydraulic = "calculate_hydraulic"
    character(*), parameter :: calculate_mechanical = "calculate_mechanical"
    character(*), parameter :: type = "type"
    character(*), parameter :: is_uniform = "is_uniform"
    character(*), parameter :: values = "values"
    character(len=16), target :: valid_thermal_boundary_types(8) = [character(len=16) :: &
                                                                    "dirichlet", "neumann", "flux", "robin", "adiabatic", &
                                                                    "free", "convective", "head_radiation"]
    character(len=16), target :: valid_hydraulic_boundary_types(5) = [character(len=16) :: &
                                                                      "dirichlet", "neumann", "flux", "impermeable", "seepage"]

contains

    module subroutine read_conditions_boundary_conditions(self, json)
        implicit none
        class(type_conditions), intent(inout) :: self
        type(json_file), intent(inout) :: json
        logical :: found
        integer(int32) :: i, j
        character(256) :: buffer(2)
        character(:), dimension(:), pointer :: valid_lists

        call json%info(boundary_conditions, found=found, n_children=self%num_boundaries)
        if (.not. found .or. self%num_boundaries <= 0) then
            call error_message(904, c_opt=boundary_conditions)
        end if

        if (allocated(self%boundary_conditions)) deallocate (self%boundary_conditions)
        allocate (self%boundary_conditions(self%num_boundaries))

        select type (p => self%parent)
        type is (type_input)
            do i = 1, self%num_boundaries
                buffer(1) = boundary_conditions//"("//to_string(i)//")"
                ! 必須のIDを読み込む
                buffer(2) = id
                call get_json_value(json, join(buffer), self%boundary_conditions(i)%id, is_required=.true.)

                do j = 1, NUM_PHYSICS_TYPES
                    if (p%basic%analysis_controls%is_active(j)) then
                        select case (j)
                        case (PHYSICS_TYPE_THERMAL)
                            buffer(2) = calculate_thermal
                        case (PHYSICS_TYPE_HYDRAULIC)
                            buffer(2) = calculate_hydraulic
                        case (PHYSICS_TYPE_MECHANICAL)
                            buffer(2) = calculate_mechanical
                        end select
                        call get_json_value(json, join(buffer), self%boundary_conditions(i)%physics(j)%is_active, &
                                            is_required=.true.)
                        if (self%boundary_conditions(i)%physics(j)%is_active) then
                            select case (j)
                            case (PHYSICS_TYPE_THERMAL)
                                buffer(2) = thermal
                                valid_lists => valid_thermal_boundary_types
                            case (PHYSICS_TYPE_HYDRAULIC)
                                buffer(2) = hydraulic
                                valid_lists => valid_hydraulic_boundary_types
                            case (PHYSICS_TYPE_MECHANICAL)
                                buffer(2) = mechanical
                            end select

                            call read_conditions_boundary_conditions_local( &
                                self%boundary_conditions(i)%physics(j), json, buffer, 2, &
                                j, valid_lists, size(self%time_control%boundary_time_points))
                        end if
                    end if
                end do

            end do
        end select

    end subroutine read_conditions_boundary_conditions

    ! ------------------------------------------------------------------
    ! NOTE: ヘルパーサブルーチン read_conditions_boundary_conditions_local
    ! は、この変更による修正は不要です。
    ! ------------------------------------------------------------------
    subroutine read_conditions_boundary_conditions_local(boundary, json, buffer, end_index, physics_type_id, valid_types, num_time_points)
        implicit none
        type(type_boundary_local), intent(inout) :: boundary
        type(json_file), intent(inout) :: json
        character(*), intent(in) :: buffer(:)
        integer(int32), intent(in) :: end_index
        integer(int32), intent(in) :: physics_type_id
        character(len=*), intent(in) :: valid_types(:)
        integer(int32), intent(in) :: num_time_points
        character(len=256), allocatable :: local_buffer(:)
        character(:), allocatable :: tmp_string

        procedure(get_value), pointer :: p_get_value
        procedure(get_string), pointer :: p_get_string

        allocate (local_buffer(size(buffer) + 1))
        local_buffer(1:end_index) = buffer(1:end_index)

        select case (physics_type_id)
        case (PHYSICS_TYPE_THERMAL)
            p_get_value => get_thermal_bc_type
            p_get_string => get_thermal_bc_type_string
        case (PHYSICS_TYPE_HYDRAULIC)
            p_get_value => get_hydraulic_bc_type
            p_get_string => get_hydraulic_bc_type_string
        end select
        local_buffer(end_index + 1) = type
        call get_json_value(json, join(local_buffer), tmp_string, is_required=.true., valid_list=valid_types)
        boundary%type = p_get_value(tmp_string)
        select case (p_get_string(boundary%type))
        case ("dirichlet", "neumann", "flux", "robin", "heat_transfer", "head_radiation")
            local_buffer(end_index + 1) = values
            call get_json_value(json, join(local_buffer), boundary%values, &
                                is_required=.true., array_size=num_time_points)

        case ("adiabatic", "impermeable", "free")
            if (allocated(boundary%values)) deallocate (boundary%values)
        end select
    end subroutine read_conditions_boundary_conditions_local

    module subroutine display_boundary_conditions(self)
        implicit none
        class(type_boundary_conditions), intent(in) :: self

        write (*, '(a, i0, a)') "  ■ Boundary Condition (ID: ", self%id, ") -------------------"
        if (associated(self%parent)) then
            ! 次に、祖父母ポインタが有効かチェックする
            if (associated(self%parent%parent)) then
                select type (p => self%parent%parent)
                type is (type_input)
                    if (p%basic%analysis_controls%is_active(PHYSICS_TYPE_THERMAL)) then
                        call display_boundary_local(self%physics, "Thermal", PHYSICS_TYPE_THERMAL)
                    end if
                    if (p%basic%analysis_controls%is_active(PHYSICS_TYPE_HYDRAULIC)) then
                        call display_boundary_local(self%physics, "Hydraulic", PHYSICS_TYPE_HYDRAULIC)
                    end if
                    if (p%basic%analysis_controls%is_active(PHYSICS_TYPE_MECHANICAL)) then
                        call display_boundary_local(self%physics, "Mechanical", PHYSICS_TYPE_MECHANICAL)
                    end if

                end select

            else
                ! (任意) 祖父母ポインタが null の場合のエラー処理
                write (*, *) "Grandparent pointer is not associated."
            end if

        else
            ! (任意) 親ポインタが null の場合のエラー処理
            write (*, *) "Parent pointer is not associated."
        end if

    end subroutine display_boundary_conditions

    subroutine display_boundary_local(boundary, title, target_physics)
        implicit none
        type(type_boundary_local), intent(in) :: boundary(:)
        character(*), intent(in) :: title
        integer(int32), intent(in) :: target_physics
        integer(int32) :: n_vals

        procedure(get_string), pointer :: p_get_string => null()

        select case (target_physics)
        case (PHYSICS_TYPE_THERMAL)
            p_get_string => get_thermal_bc_type_string
        case (PHYSICS_TYPE_HYDRAULIC)
            p_get_string => get_hydraulic_bc_type_string
        end select

        write (*, '(a, a, a)') "    --- ", trim(title), " ---"
        write (*, '(a, a)') "      Type                : ", p_get_string(boundary(target_physics)%type)

        select case (p_get_string(boundary(target_physics)%type))
        case ("dirichlet", "neumann", "flux", "robin", "heat_transfer", "head_radiation")
            if (allocated(boundary(target_physics)%values)) then
                n_vals = size(boundary(target_physics)%values)
                if (n_vals == 0) then
                    write (*, '(a)') "      Values              : (0 points defined)"
                else if (n_vals <= 6) then
                    write (*, '(a, *(es12.4e2, :, " "))') "      Values              : ", boundary(target_physics)%values
                else
                    write (*, '(a, 3(es12.4e2, :, " "), a, 3(es12.4e2, :, " "))') &
                        "      Values (summary)    : ", boundary(target_physics)%values(1:3), " ... ", boundary(target_physics)%values(n_vals - 2:n_vals)
                end if
            else
                write (*, '(a)') "      Values              : Not allocated"
            end if
        case default
            ! "adiabatic"のようなタイプは追加フィールドなし
        end select
    end subroutine display_boundary_local

end submodule inout_input_conditions_boundry
