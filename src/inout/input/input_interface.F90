module inout_input
    use, intrinsic :: iso_fortran_env, only: int32, real64, output_unit
    use :: mpi_f08
!$  use :: omp_lib
    use :: stdlib_strings, only:to_string, strip, ends_with
    use :: stdlib_logger
    use :: json_module, only:json_file
    use :: inout_input_base, only:abst_input
    use :: inout_input_basic, only:type_input_basic
    use :: inout_input_conditions, only:type_conditions
    use :: inout_input_output_conditions, only:type_output_settings
    use :: inout_input_geometry, only:type_input_geometry
    use :: module_core, only:type_vtk, type_coordinate_array_dp, type_coordinate_dp, allocate_array, deallocate_array, & !&
                             error_message, join, value_in_range, filter, modify_path_format, get_env_string
    implicit none
    private

    public :: type_input

    type, extends(abst_input) :: type_input
        character(:), allocatable :: input_path

        type(type_input_basic) :: basic
        type(type_conditions) :: conditions
        type(type_output_settings) :: output_settings
        type(type_input_geometry) :: geometry
    contains
        procedure, pass(self), public :: initialize => initialize_type_input
        procedure, pass(self), public :: display => display_input
    end type type_input

contains

    subroutine initialize_type_input(self)
        implicit none
        class(type_input), intent(inout), target :: self

        character(:), allocatable :: project_path_env

        character(*), parameter :: PROJECT_ENV = "FTDSS_PROJECT_PATH"

        integer(int32) :: ierr, myrank
        integer(int32) :: error_flag = 0
        integer(int32) :: input_path_length

        call MPI_Comm_rank(MPI_COMM_WORLD, myrank, ierr)

        call get_env_string(PROJECT_ENV, project_path_env)
        call modify_path_format(project_path_env)

        if (myrank == 0) then
            self%input_path = strip(project_path_env)//"Input/"
            input_path_length = len_trim(self%input_path)
            if (.not. file_exists(self%input_path//"Basic.json")) error_flag = 2
            if (.not. file_exists(self%input_path//"Conditions.json")) error_flag = 3
            if (.not. file_exists(self%input_path//"Output.json")) error_flag = 4
        end if

        ! 2. ランク0のチェック結果(error_flag)を全員にブロードキャスト
        call MPI_Bcast(error_flag, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)

        ! 3. もしエラーがあれば全員で停止
        if (error_flag /= 0) then
            if (myrank == 0) then
                print *, "FATAL ERROR: A required input file was not found. Aborting."
                select case (error_flag)
                case (2); print *, "-> Basic.json is missing."
                case (3); print *, "-> Conditions.json is missing."
                case (4); print *, "-> Output.json is missing."
                end select
            end if
            call MPI_Abort(MPI_COMM_WORLD, 1, ierr)
        end if

        call MPI_Bcast(input_path_length, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
        if (myrank /= 0) then
            allocate (character(len=input_path_length) :: self%input_path)
        end if
        call MPI_Bcast(self%input_path, input_path_length, MPI_CHARACTER, 0, MPI_COMM_WORLD, ierr)

        self%basic%file_name = self%input_path//"Basic.json"
        self%conditions%file_name = self%input_path//"Conditions.json"
        self%output_settings%file_name = self%input_path//"Output.json"

        self%basic%parent => self
        self%conditions%parent => self
        self%output_settings%parent => self
        self%geometry%parent => self

        call self%basic%initialize()
        call self%conditions%initialize()
        call self%output_settings%initialize()
        call self%geometry%initialize()

    end subroutine initialize_type_input

    subroutine display_input(self)
        implicit none
        class(type_input), intent(in) :: self

        integer(int32) :: ierr, myrank
        integer(int32) :: i

        call MPI_Comm_rank(MPI_COMM_WORLD, myrank, ierr)
        if (myrank == 0) then
            write (*, '(A)') "=== Simulation Settings ==="
            call self%basic%display()
            write (*, '(A)') "=== Conditions ==="
            call self%conditions%display()
            write (*, '(A)') "=== Output Settings ==="
            call self%output_settings%display()
        end if

    end subroutine display_input

    !>
    !> Checks if a file exists by attempting to open it.
    !> This method is more reliable than using INQUIRE(FILE=...) in some environments.
    !>
    function file_exists(file_path) result(found)
        implicit none
        !> The path of the file to check.
        character(*), intent(in) :: file_path
        !> `.true.` if the file exists and is readable, `.false.` otherwise.
        logical :: found
        integer(int32) :: unit_num, io_status

        open (newunit=unit_num, file=trim(file_path), status="old", action="read", iostat=io_status)

        if (io_status == 0) then
            ! If opened successfully, the file exists. Close it immediately.
            close (unit_num)
            found = .true.
        else
            ! If opened failed, the file does not exist or is not accessible.
            found = .false.
        end if

    end function file_exists

end module inout_input
