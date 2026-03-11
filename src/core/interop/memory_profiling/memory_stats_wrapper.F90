module core_interop_memory_stats_wrapper
    use, intrinsic :: iso_fortran_env, only: real64, int64
    use :: core_interop_memory_stats, only:c_get_rss_kb
    implicit none
    private

    public :: get_memory_usage

contains
    ! Retrieve current memory usage via C interop
    function get_memory_usage() result(mem_usage)
        implicit none
        integer(int64) :: rss_kb
        real(real64) :: mem_usage

        rss_kb = c_get_rss_kb()
        ! Convert RSS memory usage from KB to MB
        mem_usage = dble(rss_kb) / 1024.0d0
    end function get_memory_usage

end module core_interop_memory_stats_wrapper
