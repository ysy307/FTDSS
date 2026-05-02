module core_types
    !-----------------------------------------------------------------
    ! Algebra - Matrix and Vector types
    !-----------------------------------------------------------------
    use :: core_types_matrix, only: &
        abst_matrix, &
        type_matrix_coo, &
        type_matrix_csr, &
        type_matrix_dense, &
        type_matrix_bsr, &
        type_matrix_dia, &
        create_matrix, &
        type_matrix_info
    use :: core_types_vector, only: &
        type_vector_dp, &
        type_vector_int

    !-----------------------------------------------------------------
    ! Config - Base, Conditions, Physics types
    !-----------------------------------------------------------------
    use :: core_types_config_base, only: &
        abst_config
    use :: types_config_conditions, only: &
        type_config_bc, &
        type_config_ic
    use :: types_config_control, only: &
        type_config_acceleration, &
        type_config_output_manager, &
        type_config_time, &
        type_config_time_ats, &
        type_config_iteration, &
        type_config_iteration_nonlinear, &
        type_config_iteration_criterion, &
        type_config_parallel_openmp, &
        type_config_control_manager
    use :: types_config_physics, only: &
        abst_config_physics_model, &
        abst_config_physics_material, &
        type_config_wrf, &
        type_config_hcf, &
        type_config_gcc, &
        type_config_constitutive
    use :: types_config_domain, only: &
        type_config_elements, &
        type_config_multicoloring, &
        type_config_colored_elements, &
        type_config_nodes
    use :: types_config_output, only: &
        type_config_output, &
        type_config_observation, &
        type_config_observation_geometry, &
        type_config_overall

    !-----------------------------------------------------------------
    ! Discretization - DOF mapping types
    !-----------------------------------------------------------------
    use :: core_types_discretization_dof_map, only: &
        type_dof_map

    !-----------------------------------------------------------------
    ! Geometry - Coordinate and Coordinate Array types
    !-----------------------------------------------------------------
    use :: core_types_geometry_coordinate, only: &
        type_coordinate_dp, &
        type_coordinate_int
    use :: core_types_geometry_coordinate_array, only: &
        type_coordinate_array_dp, &
        type_coordinate_array_int

    !-----------------------------------------------------------------
    ! topology - Graph and Connectivity types
    !-----------------------------------------------------------------
    use :: core_types_topology_graph, only: &
        type_graph
    use :: core_types_topology_connectivity, only: &
        type_csr_index
    use :: core_types_topology_scatter_map, only: &
        type_scatter_map

    !-----------------------------------------------------------------
    ! Physics
    !-----------------------------------------------------------------
    use :: types_physics_variable, only: &
        type_variable
    use :: core_types_physics_state, only: &
        type_state
    use :: core_types_physics_state_thc, only: &
        type_state_thc
    use :: core_types_physics_meteorology, only: &
        type_meteorology, &
        type_meteorology_data
    use :: core_types_physics_solar, only: &
        type_solar_system

    !-----------------------------------------------------------------
    ! Utilities
    !-----------------------------------------------------------------
    use :: core_types_utils_datetime, only: &
        type_datetime
    implicit none
    public

end module core_types
