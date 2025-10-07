module control_openmp
    use, intrinsic :: iso_fortran_env
    use :: omp_lib
    use :: module_input, only:type_input
    implicit none
    private

    public :: initialize_openmp

contains
    subroutine initialize_openmp(input)
        implicit none
        type(type_input), intent(in) :: input

        if (input%basic%solver_settings%parallel_settings%threads%is_parallel) then
            call omp_set_num_threads(input%basic%solver_settings%parallel_settings%threads%num_threads)
            select case (input%basic%solver_settings%parallel_settings%threads%schedule)
            case ("auto")
                call omp_set_schedule(omp_sched_auto, 0)
            case ("dynamic")
                call omp_set_schedule(omp_sched_dynamic, 0)
            case ("guided")
                call omp_set_schedule(omp_sched_guided, 0)
            case ("static")
                call omp_set_schedule(omp_sched_static, 0)
            end select
            call omp_set_max_active_levels(input%basic%solver_settings%parallel_settings%threads%max_active_levels)
        end if
    end subroutine initialize_openmp

end module control_openmp
