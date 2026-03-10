!>
!> Provides direct Fortran interfaces to C functions for retrieving basic
!> system information, such as the operating system and CPU architecture.
!>
module core_interop_system_info
    use, intrinsic :: iso_c_binding, only: c_ptr
    implicit none
    private

    public :: c_get_os, c_get_cpu_architecture

    !>
    !> Defines the Fortran interfaces for the corresponding C functions.
    !>
    interface
        !>
        !> Binds to the C function `system_info_get_os`.
        !>
        function c_get_os() bind(C, name="system_info_get_os")
            import :: c_ptr
            implicit none
            !> A C pointer to a null-terminated string containing the OS name.
            type(c_ptr) :: c_get_os
        end function c_get_os

        !>
        !> Binds to the C function `system_info_get_cpu_architecture`.
        !>
        function c_get_cpu_architecture() bind(C, name="system_info_get_cpu_architecture")
            import :: c_ptr
            implicit none
            !> A C pointer to a null-terminated string containing the CPU architecture.
            type(c_ptr) :: c_get_cpu_architecture
        end function c_get_cpu_architecture
    end interface
contains

end module core_interop_system_info
