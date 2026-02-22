module core_fortran_utils_memory_stats_wrapper
    use, intrinsic :: iso_fortran_env, only: real64, int64
    use :: core_c_utils_memory_stats, only:c_get_rss_kb
    implicit none
    private

    public :: get_memory_usage

contains
    ! Fortran で C のポインタを受け取り、メモリ使用量を取得する関数
    function get_memory_usage() result(mem_usage)
        implicit none
        integer(int64) :: rss_kb
        real(real64) :: mem_usage

        ! C の関数を呼び出してメモリ使用量を取得
        rss_kb = c_get_rss_kb()
        ! RSS メモリ使用量を MB 単位に変換
        mem_usage = dble(rss_kb) / 1024.0d0
    end function get_memory_usage

end module core_fortran_utils_memory_stats_wrapper
