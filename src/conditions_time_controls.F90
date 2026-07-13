submodule(io_input_conditions) input_conditions_time_controls
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
    character(*), parameter :: adaptive_stepping = "adaptive_stepping"
    character(*), parameter :: is_active = "is_active"
    character(*), parameter :: iter_min = "iter_min"
    character(*), parameter :: iter_max = "iter_max"
    character(*), parameter :: scale_up = "scale_up"
    character(*), parameter :: scale_down = "scale_down"
    character(*), parameter :: scale_retry = "scale_retry"
    character(*), parameter :: iteration_control = "iteration_control"
    character(*), parameter :: iteration_count_min = "iteration_count_min"
    character(*), parameter :: iteration_count_max = "iteration_count_max"
    character(*), parameter :: time_step_scale_up = "time_step_scale_up"
    character(*), parameter :: time_step_scale_down = "time_step_scale_down"
    character(*), parameter :: time_step_scale_retry = "time_step_scale_retry"
    character(*), parameter :: error_control = "error_control"
    character(*), parameter :: error_control_is_active = "is_active"
    character(*), parameter :: error_relative_tolerance = "error_relative_tolerance"
    character(*), parameter :: proportional_gain = "proportional_gain"
    character(*), parameter :: integral_gain = "integral_gain"
    character(*), parameter :: safety_factor = "safety_factor"
    character(*), parameter :: max_growth_rate = "max_growth_rate"
    character(*), parameter :: solution_change_control = "solution_change_control"
    character(*), parameter :: max_temperature_change_per_step = "max_temperature_change_per_step"
    character(*), parameter :: max_relative_change_per_step = "max_relative_change_per_step"
    !!------------------------------------------------------------------------------------------------------------------------------

contains
    module subroutine read_conditions_time_controls(self, json)
        implicit none
        class(type_conditions), intent(inout) :: self
        type(json_file), intent(inout) :: json

        call read_time_ctrl_sim_period(self, json)
        call read_time_ctrl_stepping(self, json)
        call read_time_ctrl_adaptive(self, json)
        ! call read_conditions_time_controls_boundary_time_points(self, json)
    end subroutine read_conditions_time_controls

    subroutine read_time_ctrl_sim_period(self, json)
        !> Load the time control parameters from the JSON file
        implicit none
        class(type_conditions), intent(inout) :: self
        type(json_file), intent(inout) :: json !! JSON parser
        character(256) :: buffer(3)

        buffer(1) = time_controls
        buffer(2) = simulation_period

        buffer(3) = unit
        call get_json_value(json, join(buffer), self%time_control%simulation_period%unit, &
                            is_required=.true., valid_list=valid_units)

        ! self%time_control%simulation_period%unit = get_time_unit(tmp_string)
        ! if (allocated(tmp_string)) deallocate (tmp_string)

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
    end subroutine read_time_ctrl_sim_period

    subroutine read_time_ctrl_stepping(self, json)
        !> Load the time stepping parameters from the JSON file
        implicit none
        class(type_conditions), intent(inout) :: self
        type(json_file), intent(inout) :: json !! JSON parser
        character(256) :: buffer(3)

        buffer(1) = time_controls
        buffer(2) = time_stepping

        buffer(3) = unit
        call get_json_value(json, join(buffer), self%time_control%time_stepping%unit, &
                            is_required=.true., valid_list=valid_units)

        ! self%time_control%time_stepping%unit = get_time_unit(tmp_string)
        ! if (allocated(tmp_string)) deallocate (tmp_string)

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
            call raise_error(ERROR_CODES%VAR_INVALID, &
                             opt="In "//join(buffer(1:2))//", 'min_step' cannot be greater than 'max_step'.")
        end if
        if (self%time_control%time_stepping%initial_step < self%time_control%time_stepping%min_step .or. &
            self%time_control%time_stepping%initial_step > self%time_control%time_stepping%max_step) then
            call raise_error(ERROR_CODES%VAR_INVALID, &
                             opt="In "//join(buffer(1:2))//", 'initial_step' must be between 'min_step' and 'max_step'.")
        end if
    end subroutine read_time_ctrl_stepping

    subroutine read_time_ctrl_adaptive(self, json)
        !> Load the adaptive time stepping parameters from the JSON file
        implicit none
        class(type_conditions), intent(inout) :: self
        type(json_file), intent(inout) :: json !! JSON parser
        character(256) :: buffer(5)

        buffer(1) = time_controls
        buffer(2) = adaptive_stepping
        buffer(3) = ""
        buffer(4) = ""
        buffer(5) = ""

        buffer(3) = is_active
        call get_json_value(json, join(buffer(1:3)), self%time_control%adaptive_stepping%is_active, &
                            is_required=.true.)

        ! Legacy flat keys (backward compatible)
        buffer(3) = iter_min
        call get_json_value(json, join(buffer(1:3)), self%time_control%adaptive_stepping%iter_min, &
                            is_required=.false., default_value=4, valid_range=[1, huge(0)])

        buffer(3) = iter_max
        call get_json_value(json, join(buffer(1:3)), self%time_control%adaptive_stepping%iter_max, &
                            is_required=.false., default_value=8, valid_range=[1, huge(0)])

        buffer(3) = scale_up
        call get_json_value(json, join(buffer(1:3)), self%time_control%adaptive_stepping%scale_up, &
                            is_required=.false., default_value=1.2d0, valid_range=[1.0d0, huge(0.0d0)])

        buffer(3) = scale_down
        call get_json_value(json, join(buffer(1:3)), self%time_control%adaptive_stepping%scale_down, &
                            is_required=.false., default_value=0.8d0, valid_range=[epsilon(0.0d0), 1.0d0])

        buffer(3) = scale_retry
        call get_json_value(json, join(buffer(1:3)), self%time_control%adaptive_stepping%scale_retry, &
                            is_required=.false., default_value=0.5d0, valid_range=[epsilon(0.0d0), 1.0d0])

        ! Hierarchical explicit keys (preferred)
        buffer(3) = iteration_control
        buffer(4) = iteration_count_min
        call get_json_value(json, join(buffer(1:4)), self%time_control%adaptive_stepping%iter_min, &
                            is_required=.false., default_value=self%time_control%adaptive_stepping%iter_min, &
                            valid_range=[1, huge(0)])

        buffer(4) = iteration_count_max
        call get_json_value(json, join(buffer(1:4)), self%time_control%adaptive_stepping%iter_max, &
                            is_required=.false., default_value=self%time_control%adaptive_stepping%iter_max, &
                            valid_range=[1, huge(0)])

        buffer(4) = time_step_scale_up
        call get_json_value(json, join(buffer(1:4)), self%time_control%adaptive_stepping%scale_up, &
                            is_required=.false., default_value=self%time_control%adaptive_stepping%scale_up, &
                            valid_range=[1.0d0, huge(0.0d0)])

        buffer(4) = time_step_scale_down
        call get_json_value(json, join(buffer(1:4)), self%time_control%adaptive_stepping%scale_down, &
                            is_required=.false., default_value=self%time_control%adaptive_stepping%scale_down, &
                            valid_range=[epsilon(0.0d0), 1.0d0])

        buffer(4) = time_step_scale_retry
        call get_json_value(json, join(buffer(1:4)), self%time_control%adaptive_stepping%scale_retry, &
                            is_required=.false., default_value=self%time_control%adaptive_stepping%scale_retry, &
                            valid_range=[epsilon(0.0d0), 1.0d0])

        buffer(3) = error_control
        buffer(4) = error_control_is_active
        call get_json_value(json, join(buffer(1:4)), self%time_control%adaptive_stepping%use_error_control, &
                            is_required=.false., default_value=.true.)

        buffer(4) = error_relative_tolerance
        call get_json_value(json, join(buffer(1:4)), self%time_control%adaptive_stepping%error_relative_tolerance, &
                            is_required=.false., default_value=1.0d-2, valid_range=[0.0d0, huge(0.0d0)])

        buffer(4) = proportional_gain
        call get_json_value(json, join(buffer(1:4)), self%time_control%adaptive_stepping%proportional_gain, &
                            is_required=.false., default_value=0.10d0, valid_range=[0.0d0, huge(0.0d0)])

        buffer(4) = integral_gain
        call get_json_value(json, join(buffer(1:4)), self%time_control%adaptive_stepping%integral_gain, &
                            is_required=.false., default_value=0.15d0, valid_range=[0.0d0, huge(0.0d0)])

        buffer(4) = safety_factor
        call get_json_value(json, join(buffer(1:4)), self%time_control%adaptive_stepping%safety_factor, &
                            is_required=.false., default_value=0.9d0, valid_range=[0.0d0, huge(0.0d0)])

        buffer(4) = max_growth_rate
        call get_json_value(json, join(buffer(1:4)), self%time_control%adaptive_stepping%max_growth_rate, &
                            is_required=.false., default_value=2.0d0, valid_range=[1.0d0, huge(0.0d0)])

        buffer(3) = solution_change_control
        buffer(4) = max_temperature_change_per_step
        call get_json_value(json, join(buffer(1:4)), self%time_control%adaptive_stepping%max_temperature_change_per_step, &
                            is_required=.false., default_value=5.0d0, valid_range=[0.0d0, huge(0.0d0)])

        buffer(4) = max_relative_change_per_step
        call get_json_value(json, join(buffer(1:4)), self%time_control%adaptive_stepping%max_relative_change_per_step, &
                            is_required=.false., default_value=0.3d0, valid_range=[0.0d0, huge(0.0d0)])
    end subroutine read_time_ctrl_adaptive

    module subroutine display_time_controls(self)
        !> Displays all settings for this time_control object.
        !> This is a type-bound procedure for 'type_time_control'.
        implicit none
        class(type_time_controls), intent(in) :: self

        write (*, '(/a)') "======================================================================"
        write (*, '(a)') "                     Time Control Settings"
        write (*, '(a)') "======================================================================"
        ! --- Simulation Period ---
        write (*, '(/a)') "  --- Simulation Period ---"
        write (*, '(a, a)') "    Unit                : ", strip(self%simulation_period%unit)
        write (*, '(a, es12.4e2)') "    Start Time          : ", self%simulation_period%start
        write (*, '(a, es12.4e2)') "    End Time            : ", self%simulation_period%end
        ! --- Time Stepping ---
        write (*, '(/a)') "  --- Time Stepping ---"
        write (*, '(a, a)') "    Unit                : ", strip(self%time_stepping%unit)
        write (*, '(a, es12.4e2)') "    Initial Step        : ", self%time_stepping%initial_step
        write (*, '(a, es12.4e2)') "    Min Step            : ", self%time_stepping%min_step
        write (*, '(a, es12.4e2)') "    Max Step            : ", self%time_stepping%max_step
        write (*, '(a)') "======================================================================"
    end subroutine display_time_controls

end submodule input_conditions_time_controls
