module core_types
    use :: core_types_vector, only:type_dp_vector_2d, type_dp_vector_3d, type_int_vector_2d, type_int_vector_3d, & !&
                                   assignment(=), operator(+), operator(-)
    use :: core_types_array, only:type_dp_2d, type_dp_3d, type_int_2d, type_int_3d, assignment(=)
    use :: core_types_pointer, only:type_dp_pointer
    use :: core_types_variable, only:type_variable
    use :: core_types_gauss, only:type_state
    implicit none

end module core_types
