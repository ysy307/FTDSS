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
        character(:), dimension(:), pointer :: valid_list

        type(type_constant_id) :: physics_type

        call json%info(boundary_conditions, found=found, n_children=self%num_boundaries)
        if (.not. found .or. self%num_boundaries <= 0) then
            call raise_error(ERROR_CODES%VAR_INVALID, opt=boundary_conditions)
        end if

        if (allocated(self%boundary_conditions)) deallocate (self%boundary_conditions)
        allocate (self%boundary_conditions(self%num_boundaries))

        select type (p => self%parent)
        type is (type_input)
            do i = 1, self%num_boundaries
                buffer(1) = boundary_conditions//"("//to_string(i)//")"
                ! 必須のIDを読み込む
                buffer(2) = id
                call get_json_value(json, join(buffer(1:2)), self%boundary_conditions(i)%ID, is_required=.true.)

                do j = 1, PHYSICS_TYPES%NUM_ID
                    ! do j = 1, NUM_PHYSICS_TYPES
                    if (p%basic%analysis_controls%is_active(j)) then
                        select case (j)
                        case (PHYSICS_TYPES%THERMAL%ID)
                            buffer(2) = calculate_thermal
                        case (PHYSICS_TYPES%HYDRAULIC%ID)
                            buffer(2) = calculate_hydraulic
                        case (PHYSICS_TYPES%MECHANICAL%ID)
                            buffer(2) = calculate_mechanical
                        end select
                        call get_json_value(json, join(buffer(1:2)), self%boundary_conditions(i)%physics(j)%is_active, &
                                            is_required=.true.)
                        if (self%boundary_conditions(i)%physics(j)%is_active) then
                            select case (j)
                            case (PHYSICS_TYPES%THERMAL%ID)
                                physics_type = PHYSICS_TYPES%THERMAL
                                ! self%boundary_conditions(i)%physics(j)%state%physics_type = PHYSICS_TYPES%THERMAL
                                buffer(2) = thermal
                                valid_list => valid_thermal_boundary_types
                            case (PHYSICS_TYPES%HYDRAULIC%ID)
                                physics_type = PHYSICS_TYPES%HYDRAULIC
                                ! self%boundary_conditions(i)%physics(j)%state%physics_type = PHYSICS_TYPES%HYDRAULIC
                                buffer(2) = hydraulic
                                valid_list => valid_hydraulic_boundary_types
                            case (PHYSICS_TYPES%MECHANICAL%ID)
                                physics_type = PHYSICS_TYPES%MECHANICAL
                                ! self%boundary_conditions(i)%physics(j)%state%physics_type = PHYSICS_TYPES%MECHANICAL
                                buffer(2) = mechanical
                            end select

                            call read_conditions_boundary_conditions_local( &
                                self%boundary_conditions(i)%physics(j), json, buffer, 2, physics_type)

                            ! call read_conditions_boundary_conditions_local( &
                            !     self%boundary_conditions(i)%physics(j), json, buffer, 2, &
                            !     j, valid_list, size(self%time_control%boundary_time_points))
                        end if
                    end if
                end do

            end do
        end select

    end subroutine read_conditions_boundary_conditions

    subroutine read_conditions_boundary_conditions_local(boundary, json, buffer, end_index, physics_type)
        implicit none
        type(type_boundary_local), intent(inout) :: boundary
        type(json_file), intent(inout) :: json
        character(*), intent(in) :: buffer(:)
        integer(int32), intent(in) :: end_index
        type(type_constant_id), intent(in) :: physics_type

        character(len=256), allocatable :: local_buffer(:)
        character(:), allocatable :: tmp_string
        character(:), pointer, contiguous, dimension(:) :: valid_list => null()

        type(type_constant_id) :: bc_kind

        real(real64) :: tmp_value

        logical :: found
        integer(int32) :: i

        allocate (local_buffer(size(buffer) + 2))
        local_buffer(1:end_index) = buffer(1:end_index)

        if (physics_type == PHYSICS_TYPES%THERMAL) then
            valid_list => valid_thermal_boundary_types
            local_buffer(end_index) = thermal
        else if (physics_type == PHYSICS_TYPES%HYDRAULIC) then
            valid_list => valid_hydraulic_boundary_types
            local_buffer(end_index) = hydraulic
        end if

        local_buffer(end_index + 1) = type
        call get_json_value(json, join(local_buffer(1:end_index + 1)), boundary%bc_type, is_required=.true., valid_list=valid_list)
        if (physics_type == PHYSICS_TYPES%THERMAL) then
            bc_kind = THERMAL_BC_TYPES%to_object(boundary%bc_type)
        else if (physics_type == PHYSICS_TYPES%HYDRAULIC) then
            bc_kind = HYDRAULIC_BC_TYPES%to_object(boundary%bc_type)
        end if
        ! if (bc_kind == THERMAL_BC_TYPES%DIRICHLET .or. &
        !     bc_kind == THERMAL_BC_TYPES%NEUMANN .or. &
        !     bc_kind == THERMAL_BC_TYPES%FLUX .or. &
        !     bc_kind == HYDRAULIC_BC_TYPES%DIRICHLET .or. &
        !     bc_kind == HYDRAULIC_BC_TYPES%NEUMANN .or. &
        !     bc_kind == HYDRAULIC_BC_TYPES%FLUX) then

        !     ! boundary%state%num_variables = 1
        ! elseif (bc_kind == THERMAL_BC_TYPES%ROBIN .or. &
        !         bc_kind == THERMAL_BC_TYPES%CONVECTIVE .or. &
        !         bc_kind == THERMAL_BC_TYPES%RADIATION) then
        !     ! boundary%state%num_variables = 2
        ! else
        !     ! boundary%state%num_variables = 0
        ! end if

        if (bc_kind == THERMAL_BC_TYPES%DIRICHLET .or. &
            bc_kind == THERMAL_BC_TYPES%NEUMANN .or. &
            bc_kind == THERMAL_BC_TYPES%FLUX .or. &
            bc_kind == THERMAL_BC_TYPES%ROBIN .or. &
            bc_kind == THERMAL_BC_TYPES%CONVECTIVE .or. &
            bc_kind == THERMAL_BC_TYPES%RADIATION .or. &
            bc_kind == HYDRAULIC_BC_TYPES%DIRICHLET .or. &
            bc_kind == HYDRAULIC_BC_TYPES%NEUMANN .or. &
            bc_kind == HYDRAULIC_BC_TYPES%FLUX) then

            call json%info(boundary_conditions, found=found, n_children=boundary%num_time_points)
            if (.not. found .or. boundary%num_time_points <= 0) then
                call raise_error(ERROR_CODES%VAR_INVALID, opt=join(local_buffer))
            end if

            allocate (boundary%values(boundary%num_time_points))

            ! call allocate_array(boundary%state%time_points, boundary%num_time_points)
            ! call allocate_array(boundary%state%values, boundary%state%num_variables, boundary%num_time_points)

            do i = 1, boundary%num_time_points
                local_buffer(end_index + 1) = values//"("//to_string(i)//")"
                local_buffer(end_index + 2) = "time"
                call get_json_value(json, join(local_buffer(1:end_index + 2)), boundary%values(i)%time, is_required=.true.)
                local_buffer(end_index + 2) = "value"
                call get_json_value(json, join(local_buffer(1:end_index + 2)), boundary%values(i)%value, is_required=.true.)
            end do
        else
            if (allocated(boundary%values)) deallocate (boundary%values)
        end if

    end subroutine read_conditions_boundary_conditions_local

    module subroutine display_boundary_conditions(self)
        implicit none
        class(type_boundary_conditions), intent(in) :: self

        ! write (*, '(a, i0, a)') "  ■ Boundary Condition (ID: ", self%ID, ") -------------------"
        ! if (associated(self%parent)) then
        !     ! 次に、祖父母ポインタが有効かチェックする
        !     if (associated(self%parent%parent)) then
        !         select type (p => self%parent%parent)
        !         type is (type_input)
        !             if (p%basic%analysis_controls%is_active(PHYSICS_TYPE_THERMAL)) then
        !                 call display_boundary_local(self%physics, "Thermal", PHYSICS_TYPE_THERMAL)
        !             end if
        !             if (p%basic%analysis_controls%is_active(PHYSICS_TYPE_HYDRAULIC)) then
        !                 call display_boundary_local(self%physics, "Hydraulic", PHYSICS_TYPE_HYDRAULIC)
        !             end if
        !             if (p%basic%analysis_controls%is_active(PHYSICS_TYPE_MECHANICAL)) then
        !                 call display_boundary_local(self%physics, "Mechanical", PHYSICS_TYPE_MECHANICAL)
        !             end if

        !         end select

        !     else
        !         ! (任意) 祖父母ポインタが null の場合のエラー処理
        !         write (*, *) "Grandparent pointer is not associated."
        !     end if

        ! else
        !     ! (任意) 親ポインタが null の場合のエラー処理
        !     write (*, *) "Parent pointer is not associated."
        ! end if

    end subroutine display_boundary_conditions

    subroutine display_boundary_local(boundary, title, target_physics)
        implicit none
        type(type_boundary_local), intent(in) :: boundary(:)
        character(*), intent(in) :: title
        integer(int32), intent(in) :: target_physics
        integer(int32) :: n_vals
        
    end subroutine display_boundary_local

end submodule inout_input_conditions_boundry
