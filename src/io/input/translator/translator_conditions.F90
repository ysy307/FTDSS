submodule(io_input_translator) translator_conditions
    implicit none
contains

    module subroutine execute_condition_boundary(self, input, target_physics, configs)
        implicit none
        class(type_input_translator), intent(in) :: self
        class(type_input), intent(in) :: input
        type(type_constant_id), intent(in) :: target_physics
        type(type_config_bc), intent(inout), allocatable :: configs(:)

        integer(int32) :: i, j
        integer(int32) :: index

        allocate (configs(input%conditions%num_boundaries))

        do index = 1, input%conditions%num_boundaries
            configs(index)%boundary_id = input%conditions%boundary_conditions(index)%id

            associate (physics_data => input%conditions%boundary_conditions(index)%physics(target_physics%ID))

                if (.not. physics_data%is_active) return

                configs(index)%physics_type = target_physics
                configs(index)%num_time_points = physics_data%num_time_points

                select case (target_physics%ID)
                case (PHYSICS_TYPES%THERMAL%ID)
                    configs(index)%bc_kind = THERMAL_BC_TYPES%to_object(physics_data%bc_type)
                case (PHYSICS_TYPES%HYDRAULIC%ID)
                    configs(index)%bc_kind = HYDRAULIC_BC_TYPES%to_object(physics_data%bc_type)
                end select

                configs(index)%bc_data_kind = BC_DATA_PROVIDERS%to_object(physics_data%bc_value_type)

                select case (configs(index)%bc_kind%ID)
                case (THERMAL_BC_TYPES%DIRICHLET%ID, THERMAL_BC_TYPES%FLUX%ID, &
                      HYDRAULIC_BC_TYPES%DIRICHLET%ID, HYDRAULIC_BC_TYPES%FLUX%ID)
                    configs(index)%num_variables = 1
                    call allocate_array(configs(index)%time_points, configs(index)%num_time_points)
                    call allocate_array(configs(index)%values, configs(index)%num_variables, configs(index)%num_time_points)

                    do i = 1, configs(index)%num_time_points
                        configs(index)%time_points(i) = physics_data%values(i)%time
                        configs(index)%values(1, i) = physics_data%values(i)%value
                    end do

                case (THERMAL_BC_TYPES%ROBIN%ID, THERMAL_BC_TYPES%CONVECTIVE%ID, THERMAL_BC_TYPES%RADIATION%ID)
                    configs(index)%num_variables = 2
                    call allocate_array(configs(index)%time_points, configs(index)%num_time_points)
                    call allocate_array(configs(index)%values, configs(index)%num_variables, configs(index)%num_time_points)

                    ! Assign values for 2 variables here
                case default
                    configs(index)%num_variables = 0
                end select

            end associate

        end do

    end subroutine execute_condition_boundary

    module subroutine execute_condition_initial(self, input, ic_target, config)
        implicit none
        class(type_input_translator), intent(in) :: self
        class(type_input), intent(in) :: input
        type(type_constant_id), intent(in) :: ic_target
        type(type_config_ic), intent(inout) :: config

        if (.not. IC_TARGETS%is_valid(ic_target)) then
            call config%reset()
            return
        end if

        config%physics_type = ic_target
        associate (condition_data => input%conditions%initial_conditions%physics(ic_target%ID))
            if (ic_target == IC_TARGETS%POROSITY) then
                config%active = .true. ! Porosity IC might always be needed
            else
                config%active = input%basic%analysis_controls%is_active(ic_target%ID)
            end if
            config%ic_kind = IC_METHODS%to_object(condition_data%type)
            if (config%ic_kind == IC_METHODS%UNIFORM) then
                config%value = condition_data%value
            else
                ! Handle other IC methods if needed
            end if
        end associate

    end subroutine execute_condition_initial

    module subroutine execute_condition_acceleration(self, input, config)
        implicit none
        class(type_input_translator), intent(in) :: self
        class(type_input), intent(in) :: input
        type(type_config_acceleration), intent(inout) :: config

        config%num_dofs = input%geometry%vtk%num_points
        config%method = ACCELERATION_METHODS%AITKEN
        config%min_relaxation = 0.1d0
        config%max_relaxation = 1.0d0

    end subroutine execute_condition_acceleration

    module subroutine execute_condition_time(self, input, config)
        implicit none
        class(type_input_translator), intent(in) :: self
        class(type_input), intent(in) :: input
        type(type_config_time), intent(inout) :: config

        config%target_bdf_order = input%basic%solver_settings%bdf_order

        associate (time_control => input%conditions%time_control)
            config%time_stepping_unit = TIME_UNITS%to_object(time_control%time_stepping%unit)
            config%initial_step = time_control%time_stepping%initial_step * config%time_stepping_unit%value

            config%simulation_period_unit = TIME_UNITS%to_object(time_control%simulation_period%unit)
            config%start_time = time_control%simulation_period%start * config%simulation_period_unit%value
            config%end_time = time_control%simulation_period%end * config%simulation_period_unit%value

        end associate

    end subroutine execute_condition_time

    module subroutine execute_condition_time_ats(self, input, config)
        implicit none
        class(type_input_translator), intent(in) :: self
        class(type_input), intent(in) :: input
        type(type_config_time_ats), intent(inout) :: config

        type(type_constant_value) :: time_unit
        real(real64) :: time_conv_coeff

        associate (time_control => input%conditions%time_control)
            config%active = time_control%adaptive_stepping%is_active

            if (config%active) then
                call config%reset()
                config%iter_min = time_control%adaptive_stepping%iter_min
                config%iter_max = time_control%adaptive_stepping%iter_max
                config%scale_up = time_control%adaptive_stepping%scale_up
                config%scale_down = time_control%adaptive_stepping%scale_down
                config%scale_retry = time_control%adaptive_stepping%scale_retry

                time_unit = TIME_UNITS%to_object(time_control%time_stepping%unit)
                time_conv_coeff = time_unit%value
                config%dt_min = time_control%time_stepping%min_step * time_conv_coeff
                config%dt_max = time_control%time_stepping%max_step * time_conv_coeff
            else
                call config%reset()
            end if
        end associate

    end subroutine execute_condition_time_ats
end submodule translator_conditions
