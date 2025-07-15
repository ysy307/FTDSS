submodule(inout_input) inout_input_conditions
    implicit none
    !!------------------------------------------------------------------------------------------------------------------------------
    ! JSON key names for time control
    !!------------------------------------------------------------------------------------------------------------------------------
    character(*), parameter :: time_control = "time_control"
    character(*), parameter :: simulation_period = "simulation_period"
    character(*), parameter :: unit = "unit"
    character(*), parameter :: time_units(5) = ["second", "minute", "hour", "day", "year"]
    character(*), parameter :: start = "start"
    character(*), parameter :: end = "end"
    character(*), parameter :: time_stepping = "time_stepping"
    character(*), parameter :: initial_step = "initial_step"
    character(*), parameter :: min_step = "min_step"
    character(*), parameter :: max_step = "max_step"
    character(*), parameter :: boundary_condition_time_points = "boundary_condition_time_points"
    !!------------------------------------------------------------------------------------------------------------------------------
    ! JSON key names for boundary conditions
    !!------------------------------------------------------------------------------------------------------------------------------
    character(*), parameter :: boundary_conditions = "boundary_conditions"
    character(*), parameter :: id = "id"
    character(*), parameter :: thermal = "thermal"
    character(*), parameter :: hydraulic = "hydraulic"
    character(*), parameter :: type = "type"
    character(*), parameter :: is_uniform = "is_uniform"
    character(*), parameter :: values = "values"
    character(*), parameter :: thermal_boundary_types(8) = ["dirichlet", "neumann", "flux", "robin", "adiabatic", &
                                                            "free", "heat_trasfer", "head_radiation"]
    character(*), parameter :: hydraulic_boundary_types(4) = ["dirichlet", "neumann", "flux", "impermeable"]
    !!------------------------------------------------------------------------------------------------------------------------------

contains
    module subroutine inout_read_conditions(self)
        !> Load the boundary/initial conditions from the JSON file
        implicit none
        class(type_input) :: self

        type(json_file) :: json
        character(:), allocatable :: key
        integer(int32) :: iRegion

        call json%initialize()
        call json%load(filename=self%conditions_file_name)
        call json%print_error_message(output_unit)

        call read_conditions_time_control(self, json)
        call read_conditions_boundary_conditions(self, json)

        ! call inout_read_conditions_BC(self, json)
        ! call inout_read_conditions_IC(self, json)

        call json%destroy()
        call json%print_error_message(output_unit)

        stop

    end subroutine inout_read_conditions

    subroutine read_conditions_time_control(self, json)
        !> Load the time control parameters from the JSON file
        implicit none
        class(type_input) :: self
        type(json_file), intent(inout) :: json !! JSON parser

        character(:), allocatable :: key
        logical :: found

        call read_conditions_time_control_simulation_period(self, json)
        call read_conditions_time_control_time_stepping(self, json)
        call read_conditions_time_control_boundary_condition_time_points(self, json)

    end subroutine read_conditions_time_control

    subroutine read_conditions_time_control_simulation_period(self, json)
        !> Load the time control parameters from the JSON file
        implicit none
        class(type_input) :: self
        type(json_file), intent(inout) :: json !! JSON parser

        character(:), allocatable :: key
        logical :: found

        key = join([time_control, simulation_period, unit])
        call json%get(key, self%conditions%time_control%simulation_period%unit, found=found)
        if (.not. found) then
            call json%destroy()
            call error_message(904, c_opt=key)
        end if
        if (.not. any(time_units(:) == self%conditions%time_control%simulation_period%unit)) then
            call json%destroy()
            call error_message(905, c_opt=key)
        end if

        key = join([time_control, simulation_period, start])
        call json%get(key, self%conditions%time_control%simulation_period%start, found=found)
        if (.not. found) then
            call json%destroy()
            call error_message(904, c_opt=key)
        end if

        key = join([time_control, simulation_period, end])
        call json%get(key, self%conditions%time_control%simulation_period%end, found=found)
        if (.not. found) then
            call json%destroy()
            call error_message(904, c_opt=key)
        end if

        if (self%conditions%time_control%simulation_period%start >= self%conditions%time_control%simulation_period%end) then
            call json%destroy()
            call error_message(905, c_opt=key)
        end if

    end subroutine read_conditions_time_control_simulation_period

    subroutine read_conditions_time_control_time_stepping(self, json)
        !> Load the time stepping parameters from the JSON file
        implicit none
        class(type_input) :: self
        type(json_file), intent(inout) :: json !! JSON parser

        character(:), allocatable :: key
        logical :: found

        key = join([time_control, time_stepping, unit])
        call json%get(key, self%conditions%time_control%time_stepping%unit, found=found)
        if (.not. found) then
            call json%destroy()
            call error_message(904, c_opt=key)
        end if
        if (.not. any(time_units(:) == self%conditions%time_control%time_stepping%unit)) then
            call json%destroy()
            call error_message(905, c_opt=key)
        end if

        key = join([time_control, time_stepping, initial_step])
        call json%get(key, self%conditions%time_control%time_stepping%initial_step, found=found)
        if (.not. found) then
            call json%destroy()
            call error_message(904, c_opt=key)
        end if
        if (self%conditions%time_control%time_stepping%initial_step <= 0.0) then
            call json%destroy()
            call error_message(905, c_opt=key)
        end if

        key = join([time_control, time_stepping, min_step])
        call json%get(key, self%conditions%time_control%time_stepping%min_step, found=found)
        if (.not. found) then
            call json%destroy()
            call error_message(904, c_opt=key)
        end if
        if (self%conditions%time_control%time_stepping%min_step <= 0.0) then
            call json%destroy()
            call error_message(905, c_opt=key)
        end if

        key = join([time_control, time_stepping, max_step])
        call json%get(key, self%conditions%time_control%time_stepping%max_step, found=found)
        if (.not. found) then
            call json%destroy()
            call error_message(904, c_opt=key)
        end if
        if (self%conditions%time_control%time_stepping%max_step <= 0.0) then
            call json%destroy()
            call error_message(905, c_opt=key)
        end if

        if (self%conditions%time_control%time_stepping%min_step > self%conditions%time_control%time_stepping%max_step) then
            call json%destroy()
            call error_message(905, c_opt=key)
        end if

        if (self%conditions%time_control%time_stepping%initial_step < self%conditions%time_control%time_stepping%min_step .or. &
            self%conditions%time_control%time_stepping%initial_step > self%conditions%time_control%time_stepping%max_step) then
            call json%destroy()
            call error_message(905, c_opt=key)
        end if

    end subroutine read_conditions_time_control_time_stepping

    subroutine read_conditions_time_control_boundary_condition_time_points(self, json)
        !> Load the boundary condition time points from the JSON file
        implicit none
        class(type_input) :: self
        type(json_file), intent(inout) :: json !! JSON parser

        character(:), allocatable :: key
        logical :: found
        integer(int32) :: i
        real(real64), parameter :: machine_epsilon = 1.0d-9

        key = join([time_control, boundary_condition_time_points])
        call json%get(key, self%conditions%time_control%boundary_time_points, found=found)
        if (.not. found) then
            call json%destroy()
            call error_message(904, c_opt=key)
        end if

        ! --- Check if the time points array is sorted ---
        if (size(self%conditions%time_control%boundary_time_points) > 1) then
            do i = 1, size(self%conditions%time_control%boundary_time_points) - 1
                if (self%conditions%time_control%boundary_time_points(i) >= &
                    self%conditions%time_control%boundary_time_points(i + 1)) then
                    ! The array is not sorted in strictly ascending order.
                    call json%destroy()
                    call error_message(905, c_opt=key)
                end if
            end do
        end if

        if (size(self%conditions%time_control%boundary_time_points) < 2) then
            ! At least two time points are required.
            call json%destroy()
            call error_message(905, c_opt=key)
        end if

        ! --- Check if the time points are within the simulation period ---
        if (size(self%conditions%time_control%boundary_time_points) > 0) then
            ! 開始時刻のチェック
            if (abs(self%conditions%time_control%boundary_time_points(1) - &
                    self%conditions%time_control%simulation_period%start) > machine_epsilon) then
                call json%destroy()
                call error_message(905, c_opt=key)
            end if

            ! 終了時刻のチェック
            if (abs(self%conditions%time_control%boundary_time_points(size(self%conditions%time_control%boundary_time_points)) - &
                    self%conditions%time_control%simulation_period%end) > machine_epsilon) then
                call json%destroy()
                call error_message(905, c_opt=key)
            end if
        end if

    end subroutine read_conditions_time_control_boundary_condition_time_points

    subroutine read_conditions_boundary_conditions(self, json)
        implicit none
        class(type_input) :: self
        type(json_file), intent(inout) :: json !! JSON parser

        character(:), allocatable :: key
        character(:), allocatable :: key_base
        logical :: found
        integer(int32) :: i

        call json%info(boundary_conditions, found=found, n_children=self%conditions%num_boundaries)
        call json%print_error_message(output_unit)
        if (.not. found .or. self%conditions%num_boundaries <= 0) then
            call json%destroy()
            call error_message(904, c_opt=boundary_conditions)
        end if

        if (allocated(self%conditions%boundary_conditions)) deallocate (self%conditions%boundary_conditions)
        allocate (self%conditions%boundary_conditions(self%conditions%num_boundaries))

        do i = 1, self%conditions%num_boundaries
            key_base = boundary_conditions//"("//to_string(i)//")"
            key = join([key_base, id])
            call json%get(key, self%conditions%boundary_conditions(i)%id, found=found)
            if (.not. found) then
                call json%destroy()
                call error_message(904, c_opt=key)
            end if
            print *, "Boundary ID: ", self%conditions%boundary_conditions(i)%id

            if (self%basic%analysis_controls%calculate_thermal) then
                key = join([key_base, thermal])
                call read_conditions_boundary_conditions_thermal(self%conditions%boundary_conditions(i)%thermal, json, key, &
                                                                 size(self%conditions%time_control%boundary_time_points(:)))
            end if

            if (self%basic%analysis_controls%calculate_hydraulic) then
                key = join([key_base, hydraulic])
                call read_conditions_boundary_conditions_hydraulic(self%conditions%boundary_conditions(i)%hydraulic, json, key, &
                                                                   size(self%conditions%time_control%boundary_time_points(:)))
            end if

        end do

    end subroutine read_conditions_boundary_conditions

    subroutine read_conditions_boundary_conditions_thermal(boundary, json, key_base, num_time_points)
        implicit none
        type(type_boundary_local), intent(inout) :: boundary
        type(json_file), intent(inout) :: json !! JSON parser
        character(*), intent(in) :: key_base !! Base key for the boundary condition
        integer(int32), intent(in), optional :: num_time_points !! Number of time points for the boundary condition

        character(:), allocatable :: key
        logical :: found

        key = join([key_base, type])
        call json%get(key, boundary%type, found=found)
        if (.not. found) then
            call json%destroy()
            call error_message(904, c_opt=key)
        end if

        if (.not. any(thermal_boundary_types(:) == boundary%type)) then
            call json%destroy()
            call error_message(905, c_opt=key)
        end if

        select case (boundary%type)
        case (thermal_boundary_types(1))
            key = join([key_base, is_uniform])
            call json%get(key, boundary%is_uniform, found=found)
            if (.not. found) then
                call json%destroy()
                call error_message(904, c_opt=key)
            end if

            if (boundary%is_uniform) then
                key = join([key_base, values])

                call json%get(key, boundary%values, found=found)
                if (.not. found) then
                    call json%destroy()
                    call error_message(904, c_opt=key)
                end if

                if (size(boundary%values(:)) < 2) then
                    call json%destroy()
                    call error_message(905, c_opt=key)
                end if
                if (present(num_time_points)) then
                    if (size(boundary%values(:)) /= num_time_points) then
                        call json%destroy()
                        call error_message(905, c_opt=key)
                    end if
                end if
            end if
        end select

    end subroutine read_conditions_boundary_conditions_thermal

    subroutine read_conditions_boundary_conditions_hydraulic(boundary, json, key_base, num_time_points)
        implicit none
        type(type_boundary_local), intent(inout) :: boundary
        type(json_file), intent(inout) :: json !! JSON parser
        character(*), intent(in) :: key_base !! Base key for the boundary condition
        integer(int32), intent(in), optional :: num_time_points !! Number of time points for the boundary condition

        character(:), allocatable :: key
        logical :: found

        key = join([key_base, type])
        call json%get(key, boundary%type, found=found)
        if (.not. found) then
            call json%destroy()
            call error_message(904, c_opt=key)
        end if

        if (.not. any(hydraulic_boundary_types(:) == boundary%type)) then
            call json%destroy()
            call error_message(905, c_opt=key)
        end if

        select case (boundary%type)
        case (hydraulic_boundary_types(1))
            key = join([key_base, is_uniform])
            call json%get(key, boundary%is_uniform, found=found)
            if (.not. found) then
                call json%destroy()
                call error_message(904, c_opt=key)
            end if

            if (boundary%is_uniform) then
                key = join([key_base, values])
                call json%get(key, boundary%values, found=found)
                if (.not. found) then
                    call json%destroy()
                    call error_message(904, c_opt=key)
                end if

                if (size(boundary%values(:)) < 2) then
                    call json%destroy()
                    call error_message(905, c_opt=key)
                end if
                if (present(num_time_points)) then
                    if (size(boundary%values(:)) /= num_time_points) then
                        call json%destroy()
                        call error_message(905, c_opt=key)
                    end if
                end if
            end if
        end select

    end subroutine read_conditions_boundary_conditions_hydraulic

    ! subroutine inout_input_conditions_JSON_IC(self, json)
    !     !> Load the initialy conditions from the JSON file
    !     implicit none
    !     class(type_input) :: self
    !     type(json_file), intent(inout) :: json !! JSON parser

    !     character(:), allocatable :: key
    !     character(:), allocatable :: tmp

    !     character(2) :: cICGroup
    !     integer(int32) :: i, count
    !     logical(4) :: isFind

    !     ! key = Connect_Dot(ICName, ThermalName, TypeName)
    !     ! call json%get(key, self%IC%Heat%type)
    !     ! call json%print_error_message(output_unit)

    !     ! select case (self%IC%Heat%type)
    !     ! case (ConstantName)
    !     !     key = Connect_Dot(ICName, ThermalName, ValueName)
    !     !     call json%get(key, self%IC%Heat%value)
    !     !     call json%print_error_message(output_unit)
    !     ! case (LaplaceName)
    !     !     stop 'Laplace type is not supported yet, sorry'

    !     ! end select

    !     ! key = Connect_Dot(ICName, HydraulicName, TypeName)
    !     ! call json%get(key, self%conditions%IC_Hydraulic%type)
    !     ! call json%print_error_message(output_unit)

    !     ! select case (self%conditions%IC_Hydraulic%type)
    !     ! case (ConstantName)
    !     !     key = Connect_Dot(ICName, HydraulicName, ValueName)
    !     !     call json%get(key, self%conditions%IC_Hydraulic%value)
    !     !     call json%print_error_message(output_unit)
    !     ! case (LaplaceName)
    !     !     count = 0
    !     !     do i = 1, size(self%conditions%BCGroup)
    !     !         write (cICGroup, '(i0)') self%conditions%BCGroup(i)
    !     !         key = Connect_Dot(ICName, HydraulicName, ValueName, cICGroup, TypeName)
    !     !         call json%get(key, tmp, found=isFind)
    !     !         if (isFind) count = count + 1
    !     !     end do
    !     !     allocate (self%conditions%IC_Hydraulic%IC_BC(count))
    !     !     count = 0
    !     !     do i = 1, size(self%conditions%BCGroup)
    !     !         write (cICGroup, '(i0)') self%conditions%BCGroup(i)
    !     !         key = Connect_Dot(ICName, HydraulicName, ValueName, cICGroup, TypeName)
    !     !         call json%get(key, tmp, found=isFind)

    !     !         if (.not. isFind) cycle
    !     !         count = count + 1

    !     !         key = Connect_Dot(ICName, HydraulicName, ValueName, cICGroup, TypeName)
    !     !         call json%get(key, self%conditions%IC_Hydraulic%IC_BC(count)%type)
    !     !         call json%print_error_message(output_unit)

    !     !         key = Connect_Dot(ICName, HydraulicName, ValueName, cICGroup, ValueName)
    !     !         call json%get(key, self%conditions%IC_Hydraulic%IC_BC(count)%value)
    !     !         call json%print_error_message(output_unit)
    !     !     end do
    !     ! end select

    ! end subroutine inout_input_conditions_JSON_IC

end submodule inout_input_conditions
