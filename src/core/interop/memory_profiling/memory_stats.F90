module core_interop_memory_stats
    use, intrinsic :: iso_c_binding, only: c_int64_t
    implicit none
    private

    public :: c_get_rss_kb

    interface
        function c_get_rss_kb() bind(C, name="memory_stats_get_rss_kb")
            import :: c_int64_t
            integer(c_int64_t) :: c_get_rss_kb
        end function c_get_rss_kb
    end interface

end module core_interop_memory_stats
