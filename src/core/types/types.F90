module core_types
    use :: core_types_coordinate, only: &
        type_coordinate_dp, &
        type_coordinate_int
    use :: core_types_coordinate_array, only: &
        type_coordinate_array_dp, &
        type_coordinate_array_int
    use :: core_types_pointer, only: &
        type_dp_pointer, &
        type_int_pointer, &
        type_logical_pointer
    use :: core_types_variable, only: &
        type_variable
    use :: core_types_math_gauss, only: &
        type_gauss_integration_rule
    use :: core_types_physics_state, only: &
        type_state
    use :: core_types_config_base, only: &
        abst_config
    use :: core_types_config_conditions, only: &
        type_config_bc, &
        type_config_ic
    use :: core_types_config_physics, only: &
        abst_config_physics_model, &
        abst_config_physics_material, &
        type_config_wrf, &
        type_config_hcf, &
        type_config_gcc, &
        type_config_constitutive
    use :: core_types_config_control, only: &
        type_config_acceleration, &
        type_config_output_manager, &
        type_config_time, &
        type_config_time_ats, &
        type_config_iteration, &
        type_config_iteration_nonlinear, &
        type_config_iteration_criterion, &
        type_config_parallel_openmp, &
        type_config_control_manager
    use :: core_types_physics_meteorology, only: &
        type_meteorology, &
        type_meteorology_data
    use :: module_type_matrix, only: &
        abst_matrix, &
        type_matrix_coo, &
        type_matrix_csr, &
        type_matrix_dense, &
        type_matrix_bsr, &
        type_matrix_dia, &
        create_matrix, &
        type_matrix_info
    use :: module_type_vector, only: &
        type_vector_dp, &
        type_vector_int
    use :: core_types_graph, only: &
        type_graph
    use :: core_types_datetime, only: &
        type_datetime
    use :: core_types_solar, only: &
        type_solar_system
    implicit none
    public

end module core_types
