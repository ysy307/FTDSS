module core_c_utils_system_info
    use, intrinsic :: iso_c_binding, only: c_ptr
    implicit none
    private

    public :: c_get_os, c_get_cpu_architecture

    interface
        function c_get_os() bind(C, name="system_info_get_os")
            import :: c_ptr
            implicit none
            type(c_ptr) :: c_get_os
        end function c_get_os

        function c_get_cpu_architecture() bind(C, name="system_info_get_cpu_architecture")
            import :: c_ptr
            implicit none
            type(c_ptr) :: c_get_cpu_architecture
        end function c_get_cpu_architecture
    end interface
contains

end module core_c_utils_system_info
