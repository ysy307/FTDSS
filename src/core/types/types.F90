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
    use :: types_config_base, only: &
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
    use :: type_config_domain, only: &
        type_config_elements, &
        type_config_multicoloring, &
        type_config_colored_elements, &
        type_config_nodes

    !-----------------------------------------------------------------
    ! Discretization - DOF mapping types
    !-----------------------------------------------------------------
    use :: types_discretization_dof_map, only: &
        type_dof_map

    !-----------------------------------------------------------------
    ! Geometry - Coordinate and Coordinate Array types
    !-----------------------------------------------------------------
    use :: types_geometry_coordinate, only: &
        type_coordinate_dp, &
        type_coordinate_int
    use :: types_geometry_coordinate_array, only: &
        type_coordinate_array_dp, &
        type_coordinate_array_int

    !-----------------------------------------------------------------
    ! topology - Graph and Connectivity types
    !-----------------------------------------------------------------
    use :: types_topology_graph, only: &
        type_graph
    use :: types_topology_connectivity, only: &
        type_csr_index

    !-----------------------------------------------------------------
    ! Physics
    !-----------------------------------------------------------------
    use :: types_physics_variable, only: &
        type_variable
    use :: types_physics_state, only: &
        type_state
    use :: types_physics_meteorology, only: &
        type_meteorology, &
        type_meteorology_data
    use :: types_physics_solar, only: &
        type_solar_system

    !-----------------------------------------------------------------
    ! Utilites
    !-----------------------------------------------------------------
    use :: types_utils_datetime, only: &
        type_datetime
    implicit none
    public

end module core_types
