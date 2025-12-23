module core_types
    use :: core_types_coordinate, only:type_coordinate_dp, type_coordinate_int
    use :: core_types_coordinate_array, only:type_coordinate_array_dp, type_coordinate_array_int
    use :: core_types_pointer, only:type_dp_pointer, type_int_pointer, type_logical_pointer
    use :: core_types_variable, only:type_variable
    use :: core_types_physics, only:type_state, type_physics_info
    use :: module_type_matrix, only:abst_matrix, type_matrix_coo, type_matrix_csr, type_matrix_dense, type_matrix_bsr, &
        create_matrix, type_matrix_info
    use :: module_type_vector, only:type_vector_dp, type_vector_int
    use :: core_types_graph, only:type_graph
    implicit none

end module core_types
