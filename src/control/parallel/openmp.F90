module control_parallel_openmp
    use, intrinsic :: iso_fortran_env
    use :: stdlib_strings, only:strip
    use :: omp_lib
    use :: module_core
    implicit none
    private

    public :: initialize_openmp

contains
    subroutine initialize_openmp(config)
        implicit none
        type(type_config_parallel_openmp), intent(in) :: config

        if (config%is_parallel) then
            call omp_set_num_threads(config%num_threads)
            ! Always chunk_size is set to 0, which means the implementation will choose an appropriate chunk size 
            ! based on the system and workload characteristics.
            select case (strip(config%schedule))
            case ("auto")
                call omp_set_schedule(omp_sched_auto, 0)
            case ("dynamic")
                call omp_set_schedule(omp_sched_dynamic, 0)
            case ("guided")
                call omp_set_schedule(omp_sched_guided, 0)
            case ("static")
                call omp_set_schedule(omp_sched_static, 0)
            end select
            call omp_set_max_active_levels(config%max_active_levels)
            if (config%max_active_levels > 1) then
                call omp_set_nested(.true.)
            else
                call omp_set_nested(.false.)
            end if
        end if

    end subroutine initialize_openmp

end module control_parallel_openmp
