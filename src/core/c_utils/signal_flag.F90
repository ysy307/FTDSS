module core_c_utils_signal_flag
    use :: iso_c_binding, only:c_int
    implicit none
    private

    ! このモジュールを使う側が呼び出せる関数を定義
    public :: c_setup_signal_handler
    public :: c_get_interrupted_flag

    ! C関数のインターフェースを定義
    interface
        subroutine c_setup_signal_handler() bind(C, name="setup_signal_handler")
        end subroutine c_setup_signal_handler

        function c_get_interrupted_flag() bind(C, name="get_interrupted_flag")
            import :: c_int
            implicit none
            integer(c_int) :: c_get_interrupted_flag
        end function c_get_interrupted_flag
    end interface

end module core_c_utils_signal_flag
