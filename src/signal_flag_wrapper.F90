!>
!> Provides a Fortran wrapper for C functions that handle interrupt signals
!> (e.g., Ctrl+C), allowing for graceful program termination.
!>
module core_interop_signal_flag_wrapper
    use :: stdlib_logger
    use :: core_interop_signal_flag, only:c_setup_signal_handler, c_get_interrupted_flag
    implicit none

    private

    public :: setup_handler
    public :: was_interrupted

contains

    !>
    !> Sets up the signal handler to catch interrupt signals.
    !> This should be called once at the beginning of the program to enable
    !> interrupt detection.
    !>
    subroutine setup_handler()
        implicit none

        call c_setup_signal_handler()
    end subroutine setup_handler

    !>
    !> Checks if an interrupt signal has been received since the handler was set up.
    !>
    function was_interrupted() result(interrupted)
        implicit none
        !> Returns `.true.` if an interrupt signal was caught, `.false.` otherwise.
        logical :: interrupted

        if (c_get_interrupted_flag() /= 0) then
            interrupted = .true.
            call global_logger%log_warning(message="Program interrupted by user.")
        else
            interrupted = .false.
        end if
    end function was_interrupted
end module core_interop_signal_flag_wrapper
