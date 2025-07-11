module core_fortran_utils
    use :: core_fortran_utils_system_info_wrapper, only:get_username, get_hostname, & !&
                                                        get_compiler_name, get_compiler_version, & !&
                                                        get_os, get_cpu_architecture, get_openmp_version !&
    use :: core_fortran_utils_memory_stats_wrapper, only:get_memory_usage
    use :: core_fortran_utils_signal_flag_wrapper, only:setup_handler, was_interrupted
    implicit none

end module core_fortran_utils
