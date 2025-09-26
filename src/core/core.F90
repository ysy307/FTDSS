module module_core
    use :: core_error, only:error_message
    use :: core_check_nan, only:has_nan
    use :: core_check_range, only:value_in_range
    use :: core_unique, only:unique
    use :: core_allocate, only:allocate_array, allocate_pointer
    use :: core_deallocate, only:deallocate_array
    use :: core_string_utils, only:join, filter, modify_path_format, get_bc_type_from_string
    use :: core_system_env, only:get_env_string
    use :: core_findings, only:binary_find
    use :: core_fortran_utils_signal_flag_wrapper
    use :: core_fortran_utils_memory_stats_wrapper
    use :: core_fortran_utils_system_info_wrapper

    use :: core_types
    use :: core_vtk
    use :: core_vtk_vtk_constants
    use :: core_constants
    use :: core_parallel_constants
    implicit none
    public

end module module_core
