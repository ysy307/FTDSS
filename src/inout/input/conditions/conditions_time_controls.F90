submodule(inout_input_conditions) inout_input_conditions_time_controlss
    implicit none
    !!------------------------------------------------------------------------------------------------------------------------------
    ! JSON key names for time control
    !!------------------------------------------------------------------------------------------------------------------------------
    character(*), parameter :: time_controls = "time_controls"
    character(*), parameter :: simulation_period = "simulation_period"
    character(*), parameter :: unit = "unit"
    character(len=16), parameter :: valid_units(5) = [character(len=16) :: "second", "minute", "hour", "day", "year"]
    character(*), parameter :: start = "start"
    character(*), parameter :: fend = "end" ! "end" is a keyword, using "fend" for the parameter name
    character(*), parameter :: time_stepping = "time_stepping"
    character(*), parameter :: initial_step = "initial_step"
    character(*), parameter :: min_step = "min_step"
    character(*), parameter :: max_step = "max_step"
    character(*), parameter :: boundary_condition_time_points = "boundary_condition_time_points"

contains
    module subroutine read_conditions_time_controls(self, json)
        implicit none
        class(type_conditions), intent(inout) :: self
        type(json_file), intent(inout) :: json

        call read_conditions_time_controls_simulation_period(self, json)
        call read_conditions_time_controls_time_stepping(self, json)
        call read_conditions_time_controls_boundary_time_points(self, json)
    end subroutine read_conditions_time_controls

    subroutine read_conditions_time_controls_simulation_period(self, json)
        !> Load the time control parameters from the JSON file
        implicit none
        class(type_conditions), intent(inout) :: self
        type(json_file), intent(inout) :: json !! JSON parser
        character(256) :: buffer(3)
        character(:), allocatable :: tmp_string

        buffer(1) = time_controls
        buffer(2) = simulation_period

        buffer(3) = unit
        call get_json_value(json, join(buffer), tmp_string, &
                            is_required=.true., valid_list=valid_units)

        self%time_control%simulation_period%unit = get_time_unit(tmp_string)
        if (allocated(tmp_string)) deallocate (tmp_string)

        buffer(3) = start
        call get_json_value(json, join(buffer), self%time_control%simulation_period%start, &
                            is_required=.true.)

        buffer(3) = fend
        call get_json_value(json, join(buffer), self%time_control%simulation_period%end, &
                            is_required=.true.)

        ! Custom validation: Ensure start time is before end time
        if (self%time_control%simulation_period%start >= self%time_control%simulation_period%end) then
            call raise_error(ERROR_CODES%VAR_INVALID, opt=start//" and "//fend)
        end if
    end subroutine read_conditions_time_controls_simulation_period

    subroutine read_conditions_time_controls_time_stepping(self, json)
        !> Load the time stepping parameters from the JSON file
        implicit none
        class(type_conditions), intent(inout) :: self
        type(json_file), intent(inout) :: json !! JSON parser
        character(256) :: buffer(3)
        character(:), allocatable :: tmp_string

        buffer(1) = time_controls
        buffer(2) = time_stepping

        buffer(3) = unit
        call get_json_value(json, join(buffer), tmp_string, &
                            is_required=.true., valid_list=valid_units)

        self%time_control%time_stepping%unit = get_time_unit(tmp_string)
        if (allocated(tmp_string)) deallocate (tmp_string)

        buffer(3) = initial_step
        call get_json_value(json, join(buffer), self%time_control%time_stepping%initial_step, &
                            is_required=.true., valid_range=[epsilon(0.0d0), huge(0.0d0)])

        buffer(3) = min_step
        call get_json_value(json, join(buffer), self%time_control%time_stepping%min_step, &
                            is_required=.true., valid_range=[epsilon(0.0d0), huge(0.0d0)])

        buffer(3) = max_step
        call get_json_value(json, join(buffer), self%time_control%time_stepping%max_step, &
                            is_required=.true., valid_range=[epsilon(0.0d0), huge(0.0d0)])

        ! Custom validation for time step consistency
        if (self%time_control%time_stepping%min_step > self%time_control%time_stepping%max_step) then
            call raise_error(ERROR_CODES%VAR_INVALID, opt="In "//join(buffer(1:2))//", 'min_step' cannot be greater than 'max_step'.")
        end if
        if (self%time_control%time_stepping%initial_step < self%time_control%time_stepping%min_step .or. &
            self%time_control%time_stepping%initial_step > self%time_control%time_stepping%max_step) then
            call raise_error(ERROR_CODES%VAR_INVALID, opt="In "//join(buffer(1:2))//", 'initial_step' must be between 'min_step' and 'max_step'.")
        end if
    end subroutine read_conditions_time_controls_time_stepping

    subroutine read_conditions_time_controls_boundary_time_points(self, json)
        !> Load the boundary condition time points from the JSON file
        implicit none
        class(type_conditions), intent(inout) :: self
        type(json_file), intent(inout) :: json !! JSON parser
        character(256) :: buffer(2)
        integer :: i
        real(real64), parameter :: machine_epsilon = 1.0d-9

        buffer(1) = time_controls
        buffer(2) = boundary_condition_time_points

        call get_json_value(json, join(buffer), self%time_control%boundary_time_points, &
                            is_required=.true.)

        ! --- Custom Validations for the time points array ---
        if (size(self%time_control%boundary_time_points) < 2) then
            call raise_error(ERROR_CODES%VAR_INVALID, opt="Key '"//join(buffer)//"' must contain at least two time points.")
        end if

        ! Check if the time points are sorted in strictly ascending order
        do i = 1, size(self%time_control%boundary_time_points) - 1
            if (self%time_control%boundary_time_points(i) >= self%time_control%boundary_time_points(i + 1)) then
                call raise_error(ERROR_CODES%VAR_INVALID, opt="Time points for key '"//join(buffer)//"' must be in strictly ascending order.")
            end if
        end do

        ! Check if the first and last time points match the simulation period
        if (abs(self%time_control%boundary_time_points(1) - &
                self%time_control%simulation_period%start) > machine_epsilon) then
            call raise_error(ERROR_CODES%VAR_INVALID, opt="The first boundary time point must match the simulation start time.")
        end if
        if (abs(self%time_control%boundary_time_points(size(self%time_control%boundary_time_points)) - &
                self%time_control%simulation_period%end) > machine_epsilon) then
            call raise_error(ERROR_CODES%VAR_INVALID, opt="The last boundary time point must match the simulation end time.")
        end if

    end subroutine read_conditions_time_controls_boundary_time_points

    module subroutine display_time_controls(self)
        !> Displays all settings for this time_control object.
        !> This is a type-bound procedure for 'type_time_control'.
        implicit none
        class(type_time_controls), intent(in) :: self
        integer(int32) :: n_points

        write (*, '(/a)') "======================================================================"
        write (*, '(a)') "                     Time Control Settings"
        write (*, '(a)') "======================================================================"

        ! --- Simulation Period ---
        write (*, '(/a)') "  --- Simulation Period ---"
        write (*, '(a, a)') "    Unit                : ", strip(get_time_unit_string(self%simulation_period%unit))
        write (*, '(a, es12.4e2)') "    Start Time          : ", self%simulation_period%start
        write (*, '(a, es12.4e2)') "    End Time            : ", self%simulation_period%end

        ! --- Time Stepping ---
        write (*, '(/a)') "  --- Time Stepping ---"
        write (*, '(a, a)') "    Unit                : ", strip(get_time_unit_string(self%time_stepping%unit))
        write (*, '(a, es12.4e2)') "    Initial Step        : ", self%time_stepping%initial_step
        write (*, '(a, es12.4e2)') "    Min Step            : ", self%time_stepping%min_step
        write (*, '(a, es12.4e2)') "    Max Step            : ", self%time_stepping%max_step

        ! --- Boundary Condition Time Points ---
        write (*, '(/a)') "  --- Boundary Condition Time Points ---"
        if (.not. allocated(self%boundary_time_points)) then
            write (*, '(a)') "    Points              : Not allocated"
        else
            n_points = size(self%boundary_time_points)
            write (*, '(a, i0, a)') "    Total Points        : ", n_points, " points defined."
            if (n_points > 0) then
                if (n_points <= 6) then
                    write (*, '(a, *(es12.4e2, :, ", "))') "    Values              : ", self%boundary_time_points
                else
                    write (*, '(a, 3(es12.4e2, :, ", "), a, 3(es12.4e2, :, ", "))') &
                        "    Values (summary)    : ", self%boundary_time_points(1:3), " ... ", &
                        self%boundary_time_points(n_points - 2:n_points)
                end if
            end if
        end if

        write (*, '(a)') "======================================================================"
    end subroutine display_time_controls

end submodule inout_input_conditions_time_controlss
