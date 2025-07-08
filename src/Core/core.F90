module core_core
    use :: core_fortran_utils
    use :: core_error, only:error_message
    use :: core_check_nan, only:has_nan
    use :: core_check_range, only:value_in_range
    use :: core_unique, only:unique
    use :: core_allocate, only:allocate_array, allocate_pointer
    use :: core_deallocate, only:deallocate_array

    use :: core_types_vector, only:type_dp_vector_2d, type_dp_vector_3d, type_int_vector_2d, type_int_vector_3d, assignment(=)
    use :: core_types_array, only:type_dp_2d, type_dp_3d, type_int_2d, type_int_3d, assignment(=)
    use :: core_types_pointer, only:type_dp_pointer
    use :: core_types_variable, only:type_variable
    use :: core_types_gauss, only:type_gauss_point_state

    use :: core_vtk, only:type_vtk, type_vtk_cells
    use :: core_vtk_vtk_constants
    implicit none

#ifdef _MPI
    include 'mpif.h'
#endif

contains

end module core_core
