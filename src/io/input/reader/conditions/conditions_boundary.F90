submodule(io_input_conditions) input_conditions_boundary
    use :: io_input
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
    character(*), parameter :: value_type = "value_type"
    character(*), parameter :: is_uniform = "is_uniform"
    character(*), parameter :: values = "values"
    character(len=16), target :: valid_thermal_bc_types(8) = [character(len=16) :: &
                                                              "dirichlet", "neumann", "flux", "robin", "adiabatic", &
                                                              "free", "convective", "head_radiation"]
    character(len=16), target :: valid_hydraulic_bc_types(5) = [character(len=16) :: &
                                                                "dirichlet", "neumann", "flux", "impermeable", "seepage"]

contains

    module subroutine read_conditions_bc(self, json)
        implicit none
        class(type_conditions), intent(inout) :: self
        type(json_file), intent(inout) :: json

        logical :: found
        integer(int32) :: i, j
        character(256) :: buffer(2)
        character(:), dimension(:), pointer :: valid_list

        type(type_constant_id) :: physics_type
        integer(int32) :: idx

        idx = 1

        call json%info(boundary_conditions, found=found, n_children=self%num_boundaries)
        if (.not. found .or. self%num_boundaries <= 0) then
            call raise_error(ERROR_CODES%VAR_INVALID, opt=boundary_conditions)
        end if

        if (allocated(self%boundary_conditions)) deallocate (self%boundary_conditions)
        allocate (self%boundary_conditions(self%num_boundaries))

        select type (p => self%parent)
        type is (type_input)
            do i = 1, self%num_boundaries
                buffer(idx) = boundary_conditions//"("//to_string(i)//")"
                idx = idx + 1
                buffer(idx) = id
                call get_json_value(json, join(buffer(1:2)), self%boundary_conditions(idx)%ID, is_required=.true.)

                do j = 1, PHYSICS_TYPES%NUM_ID
                    if (p%basic%analysis_controls%is_active(j)) then
                        physics_type = PHYSICS_TYPES%to_object(j)

                        select case (physics_type%ID)
                        case (PHYSICS_TYPES%THERMAL%ID)
                            buffer(idx) = calculate_thermal
                        case (PHYSICS_TYPES%HYDRAULIC%ID)
                            buffer(idx) = calculate_hydraulic
                        case (PHYSICS_TYPES%MECHANICAL%ID)
                            buffer(idx) = calculate_mechanical
                        end select
                        call get_json_value(json, join(buffer(1:idx)), self%boundary_conditions(i)%physics(j)%is_active, &
                                            is_required=.true.)

                        if (self%boundary_conditions(i)%physics(j)%is_active) then
                            physics_type = PHYSICS_TYPES%to_object(j)
                            select case (physics_type%ID)
                            case (PHYSICS_TYPES%THERMAL%ID)
                                buffer(idx) = thermal
                            case (PHYSICS_TYPES%HYDRAULIC%ID)
                                buffer(idx) = hydraulic
                            case (PHYSICS_TYPES%MECHANICAL%ID)
                                buffer(idx) = mechanical
                            end select

                            call read_conditions_bc_local( &
                                self%boundary_conditions(i)%physics(j), json, buffer, idx, physics_type)
                        end if
                    end if
                end do

            end do
        end select

    end subroutine read_conditions_bc

    module subroutine read_conditions_bc_local(self, json, buffer_in, end_index, physics_type)
        implicit none
        class(type_boundary_local), intent(inout) :: self
        type(json_file), intent(inout) :: json
        character(*), intent(in) :: buffer_in(:)
        integer(int32), intent(in) :: end_index
        type(type_constant_id), intent(in) :: physics_type

        character(len=256), allocatable :: local_buffer(:)
        character(:), allocatable :: tmp_string
        character(:), pointer, contiguous, dimension(:) :: valid_list => null()

        type(type_constant_id) :: bc_kind
        type(type_constant_id) :: bc_value_type

        real(real64) :: tmp_value

        logical :: found
        logical :: found_time_real, found_time_string
        integer(int32) :: i
        integer(int32) :: idx

        idx = end_index

        allocate (local_buffer(size(buffer_in) + 2))
        local_buffer(1:idx) = buffer_in(1:idx)

        select case (physics_type%ID)
        case (PHYSICS_TYPES%THERMAL%ID)
            valid_list => valid_thermal_bc_types
            local_buffer(idx) = thermal
        case (PHYSICS_TYPES%HYDRAULIC%ID)
            valid_list => valid_hydraulic_bc_types
            local_buffer(idx) = hydraulic
        end select

        idx = idx + 1
        local_buffer(idx) = type
        call get_json_value(json, join(local_buffer(1:idx)), self%bc_type, is_required=.true., valid_list=valid_list)

        select case (physics_type%ID)
        case (PHYSICS_TYPES%THERMAL%ID)
            bc_kind = THERMAL_BC_TYPES%to_object(self%bc_type)
        case (PHYSICS_TYPES%HYDRAULIC%ID)
            bc_kind = HYDRAULIC_BC_TYPES%to_object(self%bc_type)
        end select

        local_buffer(idx) = value_type
        call get_json_value(json, join(local_buffer(1:idx)), self%bc_value_type, is_required=.true.)
        bc_value_type = BC_DATA_PROVIDERS%to_object(self%bc_value_type)

        if (allocated(self%values)) deallocate (self%values)
        select case (bc_value_type%ID)
        case (BC_DATA_PROVIDERS%CONSTANT%ID)
            self%num_time_points = 1
            allocate (self%values(self%num_time_points))

            idx = idx + 1
            local_buffer(idx) = values
            call get_json_value(json, join(local_buffer(1:idx)), self%values(1)%values, is_required=.true.)

        case (BC_DATA_PROVIDERS%TABLE%ID)
            idx = idx + 1
            local_buffer(idx) = values
            call json%info(join(local_buffer(1:idx)), found=found, n_children=self%num_time_points)
            if (.not. found .or. self%num_time_points <= 0) then
                call raise_error(ERROR_CODES%VAR_INVALID, opt=join(local_buffer(1:idx)))
            end if

            allocate (self%values(self%num_time_points))

            select case (bc_kind%ID)
            case (THERMAL_BC_TYPES%DIRICHLET%ID, THERMAL_BC_TYPES%NEUMANN%ID, THERMAL_BC_TYPES%FLUX%ID, &
                  HYDRAULIC_BC_TYPES%DIRICHLET%ID, HYDRAULIC_BC_TYPES%NEUMANN%ID, HYDRAULIC_BC_TYPES%FLUX%ID)
                idx = idx + 1
                do i = 1, self%num_time_points
                    local_buffer(idx) = values//"("//to_string(i)//")"
                    idx = idx + 1
                    local_buffer(idx) = "time"
                    call get_json_value(json, join(local_buffer(1:idx)), self%values(i)%time, &
                                        found=found_time_real, is_required=.false.)
                    call get_json_value(json, join(local_buffer(1:idx)), self%values(i)%time_iso, &
                                        found=found_time_string, is_required=.false.)
                    if (found_time_real .or. found_time_string) then
                        call raise_error(ERROR_CODES%VAR_INVALID, opt=join(local_buffer(1:idx)))
                    end if

                    local_buffer(idx) = "value"
                    call get_json_value(json, join(local_buffer(1:idx)), self%values(i)%value, is_required=.true.)
                    idx = idx - 1
                end do
            case (THERMAL_BC_TYPES%ROBIN%ID, THERMAL_BC_TYPES%CONVECTIVE%ID, THERMAL_BC_TYPES%RADIATION%ID)
                idx = idx + 1
                do i = 1, self%num_time_points
                    local_buffer(idx) = values//"("//to_string(i)//")"
                    idx = idx + 1
                    local_buffer(idx) = "time"
                    call get_json_value(json, join(local_buffer(1:idx)), self%values(i)%time, &
                                        found=found_time_real, is_required=.false.)
                    call get_json_value(json, join(local_buffer(1:idx)), self%values(i)%time_iso, &
                                        found=found_time_string, is_required=.false.)
                    if (found_time_real .or. found_time_string) then
                        call raise_error(ERROR_CODES%VAR_INVALID, opt=join(local_buffer(1:idx)))
                    end if

                    local_buffer(idx) = "value"
                    call get_json_value(json, join(local_buffer(1:idx)), self%values(i)%values, is_required=.true.)
                    idx = idx - 1
                end do
            end select
        end select

    end subroutine read_conditions_bc_local

    module subroutine display_boundary_conditions(self)
        implicit none
        class(type_boundary_conditions), intent(in) :: self

        !! TODO: 物理ごとに分けて表示する
    end subroutine display_boundary_conditions

    subroutine display_boundary_local(boundary, title, target_physics)
        implicit none
        type(type_boundary_local), intent(in) :: boundary(:)
        character(*), intent(in) :: title
        integer(int32), intent(in) :: target_physics

        !! TODO: 物理ごとに分けて表示する
    end subroutine display_boundary_local

end submodule input_conditions_boundary
