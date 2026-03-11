module core_interop
    use :: core_interop_signal_flag_wrapper, only: &
        setup_handler, &
        was_interrupted
    use :: core_interop_memory_stats_wrapper, only: &
        get_memory_usage
    use :: core_interop_system_info_wrapper, only: &
        get_username, &
        get_hostname, &
        get_compiler_name, &
        get_compiler_version, &
        get_cpu_architecture, &
        get_os, &
        get_cpu_architecture, &
        get_openmp_version

    use :: core_interop_vtk, only: &
        type_vtk, &
        type_vtk_cell
    use :: core_interop_vtk_constants, only: &
        vtk_constants
    use :: core_interop_vtu_writer_wrapper, only: &
        type_vtu_writer
    implicit none
    public

end module core_interop
