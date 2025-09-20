module inout_input
    use, intrinsic :: iso_fortran_env, only: int32, real64, output_unit
    use :: mpi_f08
!$  use :: omp_lib
    use :: stdlib_strings, only:to_string, strip, ends_with
    use :: stdlib_logger
    use :: json_module, only:json_file
    use :: inout_input_basic, only:type_input_basic
    use :: inout_input_conditions, only:type_conditions
    use :: inout_output_conditions, only:type_output_settings
    use :: module_core, only:type_vtk, type_dp_3d, type_dp_vector_3d, allocate_array, deallocate_array, & !&
                             error_message, join, value_in_range, filter, modify_path_format, get_env_string
    implicit none
    private

    public :: type_input

    !!------------------------------------------------------------------------------------------------------------------------------
    type :: type_geometry
        type(type_vtk) :: vtk
        character(:), allocatable :: point_data_names(:)
        real(real64), allocatable :: initial_values(:, :)
    end type type_geometry
    !!------------------------------------------------------------------------------------------------------------------------------

    type :: type_input
        character(:), allocatable, private :: project_path
        character(:), allocatable, private :: basic_file_name
        character(:), allocatable, private :: conditions_file_name
        character(:), allocatable, private :: geometry_file_name
        character(:), allocatable, private :: output_file_name

        type(type_input_basic) :: basic
        type(type_conditions) :: conditions
        type(type_output_settings) :: output_settings
        type(type_geometry) :: geometry
    contains
        procedure, pass(self), public :: initialize => initialize_type_input
    end type type_input

contains

    subroutine initialize_type_input(self)
        implicit none
        class(type_input), intent(inout) :: self

        character(len=:), allocatable :: fullpath
        character(len=:), allocatable :: local_input_path
        character(len=:), allocatable :: project_path_env

        character(*), parameter :: PROJECT_ENV = "FTDSS_PROJECT_PATH"

        integer(int32) :: ierr, myrank
        integer(int32) :: error_flag = 0

        call MPI_Comm_rank(MPI_COMM_WORLD, myrank, ierr)

        call get_env_string(PROJECT_ENV, project_path_env)
        call modify_path_format(project_path_env)
        self%project_path = project_path_env

        if (myrank == 0) then

            fullpath = strip(self%project_path)//"Input/"

            if (.not. file_exists(fullpath//"Basic.json")) error_flag = 2
            if (.not. file_exists(fullpath//"Conditions.json")) error_flag = 3
            if (.not. file_exists(fullpath//"Output.json")) error_flag = 4
        end if

        ! 2. ランク0のチェック結果(error_flag)を全員にブロードキャスト
        call MPI_Bcast(error_flag, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)

        ! 3. もしエラーがあれば全員で停止
        if (error_flag /= 0) then
            if (myrank == 0) then
                print *, "FATAL ERROR: A required input file was not found. Aborting."
                ! ここでどのファイルがなかったかを示すメッセージを追加すると、さらに親切
                select case (error_flag)
                case (2); print *, "-> Basic.json is missing."
                case (3); print *, "-> Conditions.json is missing."
                case (4); print *, "-> Output.json is missing."
                end select
            end if
            call MPI_Abort(MPI_COMM_WORLD, 1, ierr)
        end if

        local_input_path = strip(self%project_path)//"Input/"

        self%basic%file_name = local_input_path//"Basic.json"
        self%conditions%file_name = local_input_path//"Conditions.json"
        self%output_settings%file_name = local_input_path//"Output.json"

        call self%basic%initialize()
        call self%conditions%initialize()
        call self%output_settings%initialize()
        ! call self%read_output_settings()
        ! call self%read_geometry()

    end subroutine initialize_type_input

    !-----------------------------------------------------------------------
    ! 機能: ファイルの存在を open/close を使って確実にチェックする
    ! 引数: file_path - チェックしたいファイルのパス
    ! 戻り値: .true. (ファイルが存在し、読み取り可能), .false. (それ以外)
    !-----------------------------------------------------------------------
    function file_exists(file_path) result(found)
        implicit none
        character(len=*), intent(in) :: file_path
        logical :: found
        integer(int32) :: unit_num, io_status

        ! status='old' でファイルを開こうと試みる
        open (newunit=unit_num, file=trim(file_path), status="old", action="read", iostat=io_status)

        if (io_status == 0) then
            ! openに成功した場合: ファイルは存在する。すぐに閉じる。
            close (unit_num)
            found = .true.
        else
            ! openに失敗した場合: ファイルは存在しないか、アクセスできない。
            found = .false.
        end if

    end function file_exists

end module inout_input
