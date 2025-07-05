module Core_Signal
    use :: iso_c_binding
    use, intrinsic :: iso_fortran_env, only: logical32
    implicit none
    private

    ! このモジュールを使う側が呼び出せる関数を定義
    public :: setup_handler, was_interrupted

    ! C関数のインターフェースを定義
    interface
        subroutine c_setup_handler() bind(c, name="setup_signal_handler")
            ! 引数なし
        end subroutine c_setup_handler

        function c_get_interrupted_flag() bind(c, name="get_interrupted_flag")
            import :: c_int
            integer(c_int) :: c_get_interrupted_flag
        end function c_get_interrupted_flag
    end interface

contains

    ! C関数をFortranらしい名前でラップする
    subroutine setup_handler()
        call c_setup_handler()
    end subroutine setup_handler

    function was_interrupted() result(interrupted)
        implicit none
        logical(logical32) :: interrupted

        if (c_get_interrupted_flag() /= 0) then
            interrupted = .true.
        else
            interrupted = .false.
        end if
    end function was_interrupted

end module Core_Signal
