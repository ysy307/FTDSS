submodule(inout_input_translator) input_translator_conditions
    implicit none
contains

    module subroutine execute_condition_boundary(self, input, index, target_physics, config_bc)
        implicit none
        class(type_input_translator), intent(in) :: self
        class(type_input), intent(in) :: input
        integer(int32), intent(in) :: index
        type(type_constant_id), intent(in) :: target_physics
        type(type_config_bc), intent(inout) :: config_bc

        integer(int32) :: i

        config_bc%boundary_id = input%conditions%boundary_conditions(index)%id

        associate (physics_data => input%conditions%boundary_conditions(index)%physics(target_physics%id))

            if (.not. physics_data%is_active) return

            config_bc%physics_type = target_physics
            config_bc%num_time_points = physics_data%num_time_points

            if (target_physics == PHYSICS_TYPES%THERMAL) then
                config_bc%bc_kind = THERMAL_BC_TYPES%to_object(physics_data%bc_type)
            else if (target_physics == PHYSICS_TYPES%HYDRAULIC) then
                config_bc%bc_kind = HYDRAULIC_BC_TYPES%to_object(physics_data%bc_type)
            end if

            if (config_bc%bc_kind == THERMAL_BC_TYPES%DIRICHLET .or. &
                config_bc%bc_kind == THERMAL_BC_TYPES%NEUMANN .or. &
                config_bc%bc_kind == THERMAL_BC_TYPES%FLUX .or. &
                config_bc%bc_kind == HYDRAULIC_BC_TYPES%DIRICHLET .or. &
                config_bc%bc_kind == HYDRAULIC_BC_TYPES%NEUMANN .or. &
                config_bc%bc_kind == HYDRAULIC_BC_TYPES%FLUX) then

                config_bc%num_variables = 1
                call allocate_array(config_bc%time_points, config_bc%num_time_points)
                call allocate_array(config_bc%values, config_bc%num_variables, config_bc%num_time_points)

                do i = 1, config_bc%num_time_points
                    config_bc%time_points(i) = physics_data%values(i)%time
                    config_bc%values(1, i) = physics_data%values(i)%value
                end do

            else if (config_bc%bc_kind == THERMAL_BC_TYPES%ROBIN .or. &
                     config_bc%bc_kind == THERMAL_BC_TYPES%CONVECTIVE .or. &
                     config_bc%bc_kind == THERMAL_BC_TYPES%RADIATION) then

                config_bc%num_variables = 2
                call allocate_array(config_bc%time_points, config_bc%num_time_points)
                call allocate_array(config_bc%values, config_bc%num_variables, config_bc%num_time_points)

                ! Assign values for 2 variables here
            else
                config_bc%num_variables = 0
            end if

        end associate

    end subroutine execute_condition_boundary
end submodule input_translator_conditions
