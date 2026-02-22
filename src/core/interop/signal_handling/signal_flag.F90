!>
!> Provides the direct Fortran interface to C functions for handling interrupt
!> signals (e.g., SIGINT). This module is intended for interoperability and is
!> typically wrapped by a more user-friendly Fortran module.
!>
module core_c_utils_signal_flag
    use :: iso_c_binding, only:c_int
    implicit none
    private

    public :: c_setup_signal_handler
    public :: c_get_interrupted_flag

    !>
    !> Defines the Fortran interfaces for the corresponding C functions.
    !>
    interface
        !>
        !> Binds to the C function `setup_signal_handler`.
        !> This C function is responsible for registering the signal handlers.
        !>
        subroutine c_setup_signal_handler() bind(C, name="setup_signal_handler")
        end subroutine c_setup_signal_handler

        !>
        !> Binds to the C function `get_interrupted_flag`.
        !> This C function returns the current state of the global interrupt flag.
        !>
        function c_get_interrupted_flag() bind(C, name="get_interrupted_flag")
            import :: c_int
            implicit none
            !> The value of the C interrupt flag (non-zero if interrupted).
            integer(c_int) :: c_get_interrupted_flag
        end function c_get_interrupted_flag
    end interface

end module core_c_utils_signal_flag
