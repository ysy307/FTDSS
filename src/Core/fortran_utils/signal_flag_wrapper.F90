module core_fortran_utils_signal_flag_wrapper
    use, intrinsic :: iso_fortran_env, only: logical32
    use, intrinsic :: iso_c_binding, only: c_ptr, c_associated
    use :: core_c_utils_signal_flag, only:c_setup_signal_handler, c_get_interrupted_flag
    implicit none

    private

    public :: setup_handler, was_interrupted

contains

    ! C関数をFortranらしい名前でラップする
    subroutine setup_handler()
        call c_setup_signal_handler()
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
end module core_fortran_utils_signal_flag_wrapper
