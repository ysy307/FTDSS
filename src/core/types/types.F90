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
    use :: core_types_physics_config_bc, only: &
        type_config_bc
    use :: core_types_physics_info, only: &
        type_physics_info
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

end module core_types
