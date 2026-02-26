submodule(io_input_translator) translator_conditions
    implicit none
contains

    module subroutine execute_condition_boundary(self, input, index, target_physics, config)
        implicit none
        class(type_input_translator), intent(in) :: self
        class(type_input), intent(in) :: input
        integer(int32), intent(in) :: index
        type(type_constant_id), intent(in) :: target_physics
        class(abst_config), intent(inout) :: config

        integer(int32) :: i

        select type (config)
        type is (type_config_bc)

            config%boundary_id = input%conditions%boundary_conditions(index)%id

            associate (physics_data => input%conditions%boundary_conditions(index)%physics(target_physics%ID))

                if (.not. physics_data%is_active) return

                config%physics_type = target_physics
                config%num_time_points = physics_data%num_time_points

                if (target_physics == PHYSICS_TYPES%THERMAL) then
                    config%bc_kind = THERMAL_BC_TYPES%to_object(physics_data%bc_type)
                else if (target_physics == PHYSICS_TYPES%HYDRAULIC) then
                    config%bc_kind = HYDRAULIC_BC_TYPES%to_object(physics_data%bc_type)
                end if

                if (config%bc_kind == THERMAL_BC_TYPES%DIRICHLET .or. &
                    config%bc_kind == THERMAL_BC_TYPES%NEUMANN .or. &
                    config%bc_kind == THERMAL_BC_TYPES%FLUX .or. &
                    config%bc_kind == HYDRAULIC_BC_TYPES%DIRICHLET .or. &
                    config%bc_kind == HYDRAULIC_BC_TYPES%NEUMANN .or. &
                    config%bc_kind == HYDRAULIC_BC_TYPES%FLUX) then

                    config%num_variables = 1
                    call allocate_array(config%time_points, config%num_time_points)
                    call allocate_array(config%values, config%num_variables, config%num_time_points)

                    do i = 1, config%num_time_points
                        config%time_points(i) = physics_data%values(i)%time
                        config%values(1, i) = physics_data%values(i)%value
                    end do

                else if (config%bc_kind == THERMAL_BC_TYPES%ROBIN .or. &
                         config%bc_kind == THERMAL_BC_TYPES%CONVECTIVE .or. &
                         config%bc_kind == THERMAL_BC_TYPES%RADIATION) then

                    config%num_variables = 2
                    call allocate_array(config%time_points, config%num_time_points)
                    call allocate_array(config%values, config%num_variables, config%num_time_points)

                    ! Assign values for 2 variables here
                else
                    config%num_variables = 0
                end if

            end associate
        end select

    end subroutine execute_condition_boundary

    module subroutine execute_condition_initial(self, input, target_physics, config)
        implicit none
        class(type_input_translator), intent(in) :: self
        class(type_input), intent(in) :: input
        type(type_constant_id), intent(in) :: target_physics
        class(abst_config), intent(inout) :: config

        select type (config)
        type is (type_config_ic)
            if (.not. PHYSICS_TYPES%is_valid(target_physics)) then
                call config%reset()
                return
            end if

            config%physics_type = target_physics
            associate (condition_data => input%conditions%initial_conditions%physics(target_physics%ID))
                config%ic_kind = IC_METHODS%to_object(condition_data%type)
                if (config%ic_kind == IC_METHODS%UNIFORM) then
                    config%value = condition_data%value
                else
                    ! Handle other IC methods if needed
                end if
            end associate

        end select

    end subroutine execute_condition_initial

    module subroutine execute_condition_acceleration(self, input, config)
        implicit none
        class(type_input_translator), intent(in) :: self
        class(type_input), intent(in) :: input
        class(type_config_acceleration), intent(inout) :: config

        select type (config)
        type is (type_config_acceleration)

            config%num_dofs = input%geometry%vtk%num_points
            config%method = ACCELERATION_METHODS%AITKEN
            config%min_relaxation = 0.1d0
            config%max_relaxation = 1.0d0

        end select

    end subroutine execute_condition_acceleration

    module subroutine execute_condition_time(self, input, config)
        implicit none
        class(type_input_translator), intent(in) :: self
        class(type_input), intent(in) :: input
        class(type_config_time), intent(inout) :: config

        select type (config)
        type is (type_config_time)
            config%target_bdf_order = input%basic%solver_settings%bdf_order

            associate (time_control => input%conditions%time_control)
                config%time_stepping_unit = TIME_UNITS%to_object(time_control%time_stepping%unit)
                config%initial_step = time_control%time_stepping%initial_step * config%time_stepping_unit%value

                config%simulation_period_unit = TIME_UNITS%to_object(time_control%simulation_period%unit)
                config%start_time = time_control%simulation_period%start * config%simulation_period_unit%value
                config%end_time = time_control%simulation_period%end * config%simulation_period_unit%value

            end associate

        end select

    end subroutine execute_condition_time

    module subroutine execute_condition_time_ats(self, input, config)
        implicit none
        class(type_input_translator), intent(in) :: self
        class(type_input), intent(in) :: input
        class(type_config_time_ats), intent(inout) :: config

        type(type_constant_value) :: time_unit
        real(real64) :: time_conv_coeff

        select type (config)
        type is (type_config_time_ats)
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
        end select

    end subroutine execute_condition_time_ats
end submodule translator_conditions
