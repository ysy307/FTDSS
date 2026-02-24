submodule(inout_input_translator) input_translator_conditions
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
        class(abst_config), intent(inout) :: config

        select type (config)
        type is (type_config_acceleration)

            config%num_dofs = input%geometry%vtk%num_points
            config%method = ACCELERATION_METHODS%AITKEN
            config%min_relaxation = 0.1d0
            config%max_relaxation = 1.0d0

        end select

    end subroutine execute_condition_acceleration
end submodule input_translator_conditions
