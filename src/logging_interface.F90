module io_output_logging
    use, intrinsic :: iso_fortran_env, only: int32, real64
#ifdef _OPENMP
    use :: omp_lib
#endif
    use :: stdlib_strings, only:to_string, strip
    use :: stdlib_io, only:open
    use :: module_core

    implicit none
    private

    public :: type_output_log

    type :: type_output_log
        logical, private :: initialized = .false.
        character(:), allocatable :: log_file_name
        integer(int32), private :: io_unit = -1
    contains
        procedure, pass(self) :: initialize => initialize_type_output_log
        procedure, pass(self) :: destroy => destroy_type_output_log
        procedure, pass(self) :: output_system_log => output_system_log_type_output_log
        procedure, pass(self) :: get_io_unit => get_log_io_unit_type_output_log
    end type type_output_log

    interface
        module subroutine initialize_type_output_log(self, dir_output)
            implicit none
            class(type_output_log), intent(inout) :: self
            character(*), intent(in) :: dir_output
        end subroutine initialize_type_output_log

        module subroutine destroy_type_output_log(self, dir_output)
            implicit none
            class(type_output_log), intent(inout) :: self
            character(*), intent(in) :: dir_output

        end subroutine destroy_type_output_log

        module subroutine output_system_log_type_output_log(self)
            implicit none
            class(type_output_log), intent(in) :: self

        end subroutine output_system_log_type_output_log

        module subroutine get_log_io_unit_type_output_log(self, io_unit)
            implicit none
            class(type_output_log), intent(in) :: self
            integer(int32), intent(inout) :: io_unit

        end subroutine get_log_io_unit_type_output_log
    end interface

end module io_output_logging
