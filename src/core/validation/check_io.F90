module core_check_io
    use, intrinsic :: iso_fortran_env, only: int32
    implicit none
    private

    public :: check_unit_writable

contains

    function check_unit_writable(io_unit) result(can_write)
        implicit none
        integer(int32), intent(in) :: io_unit
        logical :: can_write

        logical :: is_open
        character(len=10) :: write_status

        inquire (unit=io_unit, opened=is_open, write=write_status)

        can_write = (is_open .and. trim(write_status) == 'YES')
    end function check_unit_writable

end module core_check_io
