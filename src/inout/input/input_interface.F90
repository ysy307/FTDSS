module inout_input
    use, intrinsic :: iso_fortran_env, only: int32, real64, output_unit
#ifdef _MPI
    use :: mpi_f08
#endif
!$  use :: omp_lib
    use :: stdlib_strings, only:to_string, ends_with
    use :: stdlib_logger
    use :: json_module, only:json_file
    use :: inout_project_settings, only:get_project_path
    use :: module_core, only:type_vtk, type_dp_3d, type_dp_vector_3d, allocate_array, deallocate_array, & !&
                             error_message, join, value_in_range, filter
    implicit none
    private

    public :: type_input
    public :: type_convergence

    !!------------------------------------------------------------------------------------------------------------------------------
    type :: type_simulation_settings
        character(:), allocatable :: title
        integer(int32) :: calculate_type
        integer(int32) :: calculate_dimension
    end type type_simulation_settings
    !!------------------------------------------------------------------------------------------------------------------------------
    type :: type_analysis_controls
        logical :: calculate_thermal
        logical :: calculate_hydraulic
        logical :: calculate_mechanical
        character(:), allocatable :: coupling_mode
    end type type_analysis_controls
    !!------------------------------------------------------------------------------------------------------------------------------
    type :: type_geometry_settings
        character(:), allocatable :: file_name
        character(:), allocatable :: cell_id_array_name
        character(:), allocatable :: integration_type
        real(real64) :: integration_points
    end type type_geometry_settings
    !!------------------------------------------------------------------------------------------------------------------------------
    type :: type_materials_wrf
        integer(int32) :: model_number
        real(real64) :: theta_s
        real(real64) :: theta_r
        real(real64) :: alpha1
        real(real64) :: n1
        real(real64) :: m1
        real(real64) :: w1
        real(real64) :: h_crit
        real(real64) :: alpha2
        real(real64) :: n2
        real(real64) :: m2
        real(real64) :: w2
    end type type_materials_wrf

    type :: type_materials_gcc
        logical :: is_segregation
        character(:), allocatable :: unit
    end type type_materials_gcc

    type :: type_materials_phase_change
        real(real64) :: latent_heat_fusion
        real(real64) :: freezing_temperature
        type(type_materials_wrf) :: wrf
        type(type_materials_gcc) :: gcc
    end type type_materials_phase_change

    type :: type_materials_thermal
        real(real64), allocatable :: density(:)
        real(real64), allocatable :: specific_heat(:)
        real(real64), allocatable :: thermal_conductivity(:)
        real(real64), allocatable :: thermal_conductivity_dispersity(:)
        type(type_materials_phase_change) :: phase_change
    end type type_materials_thermal

    type, extends(type_materials_wrf) :: type_materials_hcf
        real(real64) :: l
    end type type_materials_hcf

    type :: type_materials_hydraulic
        integer(int32) :: model_number
        real(real64) :: impedance_factor
        real(real64) :: hydraulic_conductivity
        type(type_materials_hcf) :: hcf
        integer(int32) :: water_viscosity_model
    end type type_materials_hydraulic

    type :: type_material_settings
        integer(int32) :: id
        character(:), allocatable :: name
        integer(int32) :: phase
        logical :: is_frozen
        logical :: is_dispersed
        logical :: calculate_thermal
        logical :: calculate_hydraulic
        logical :: calculate_mechanical
        type(type_materials_thermal) :: thermal
        type(type_materials_hydraulic) :: hydraulic
    end type type_material_settings
    !!------------------------------------------------------------------------------------------------------------------------------
    type :: type_convergence_criteria
        character(:), allocatable :: criteria
        character(:), allocatable :: logic
        real(real64) :: absolute_tolerance
        real(real64) :: relative_tolerance
    end type type_convergence_criteria

    type :: type_convergence
        character(:), allocatable :: use_criteria
        character(:), allocatable :: use_logic
        type(type_convergence_criteria) :: residual
        type(type_convergence_criteria) :: update
    end type type_convergence

    type :: type_nonlinear_solver
        character(:), allocatable :: method
        integer(int32) :: update_frequency
        integer(int32) :: max_iterations
        type(type_convergence) :: convergence
    end type type_nonlinear_solver

    type :: type_linear_solver_iterative
        integer(int32) :: solver_type
        integer(int32) :: preconditioner_type
        integer(int32) :: max_iterations
        real(real64) :: tolerance
    end type type_linear_solver_iterative

    type :: type_linear_solver_settings
        character(:), allocatable :: method
        type(type_linear_solver_iterative) :: iterative_solver
    end type type_linear_solver_settings

    type :: type_linear_solver
        type(type_linear_solver_settings) :: thermal
        type(type_linear_solver_settings) :: hydraulic
        type(type_linear_solver_settings) :: mechanical
    end type type_linear_solver

    type :: type_parallel_threads
        logical :: is_parallel
        integer(int32) :: num_threads
        character(:), allocatable :: schedule
        integer(int32) :: max_active_levels
    end type type_parallel_threads

    type :: type_parallel_settings
        type(type_parallel_threads) :: threads
    end type type_parallel_settings

    type :: type_solver_settings
        integer(int32) :: bdf_order
        character(:), allocatable :: reordering
        character(:), allocatable :: coloring
        type(type_nonlinear_solver) :: nonlinear_solver
        type(type_linear_solver) :: linear_solver
        type(type_parallel_settings) :: parallel_settings
    end type
    !!------------------------------------------------------------------------------------------------------------------------------
    type :: input_basic
        type(type_simulation_settings) :: simulation_settings
        type(type_analysis_controls) :: analysis_controls
        type(type_geometry_settings) :: geometry_settings
        integer(int32) :: num_materials
        type(type_material_settings), allocatable :: materials(:)
        type(type_solver_settings) :: solver_settings
    end type input_basic
    !!------------------------------------------------------------------------------------------------------------------------------
    type :: type_time_control_simulation_period
        character(:), allocatable :: unit
        real(real64) :: start
        real(real64) :: end
    end type type_time_control_simulation_period

    type :: type_time_control_time_stepping
        character(:), allocatable :: unit
        real(real64) :: initial_step
        real(real64) :: min_step
        real(real64) :: max_step
    end type type_time_control_time_stepping

    type :: type_time_control
        type(type_time_control_simulation_period) :: simulation_period
        type(type_time_control_time_stepping) :: time_stepping
        real(real64), allocatable :: boundary_time_points(:)
    end type type_time_control
    !!------------------------------------------------------------------------------------------------------------------------------
    type :: type_boundary_local
        character(:), allocatable :: type
        logical :: is_uniform
        real(real64), allocatable :: values(:)
    end type type_boundary_local

    type :: type_boundary_conditions
        integer(int32) :: id
        type(type_boundary_local) :: thermal
        type(type_boundary_local) :: hydraulic
    end type type_boundary_conditions
    !!------------------------------------------------------------------------------------------------------------------------------
    type, extends(type_boundary_local) :: type_boundary_local_initial
        integer(int32) :: id
    end type

    type :: type_initial_local
        character(:), allocatable :: type
        real(real64) :: value
        type(type_boundary_local_initial), allocatable :: boundary(:)
        character(:), allocatable :: field_name
    end type type_initial_local

    type :: type_initail_conditions
        type(type_initial_local) :: thermal
        type(type_initial_local) :: hydraulic
        type(type_initial_local) :: porosity
    end type type_initail_conditions
    !!------------------------------------------------------------------------------------------------------------------------------
    type :: type_conditions
        type(type_time_control) :: time_control
        type(type_boundary_conditions), allocatable :: boundary_conditions(:)
        integer(int32) :: num_boundaries
        type(type_initail_conditions) :: initial_conditions
    end type type_conditions
    !!------------------------------------------------------------------------------------------------------------------------------
    type :: type_field_output
        character(:), allocatable :: file_format
        logical :: coloring
        character(:), allocatable :: output_interval_unit
        real(real64) :: output_interval_step
        character(:), allocatable :: variable_names(:)
    end type type_field_output
    !!------------------------------------------------------------------------------------------------------------------------------
    type :: types_history_output
        character(:), allocatable :: file_format
        character(:), allocatable :: observation_type
        character(:), allocatable :: output_interval_unit
        real(real64) :: output_interval_step
        character(:), allocatable :: variable_names(:)
        integer(int32) :: num_observations
        type(type_dp_vector_3d), allocatable :: coordinates(:)
        integer(int32), allocatable :: node_ids(:)
    end type types_history_output
    !!------------------------------------------------------------------------------------------------------------------------------
    type :: type_standard_output
        logical :: print_progress
        character(:), allocatable :: print_progress_unit
        real(real64) :: print_progress_interval
    end type type_standard_output
    !!------------------------------------------------------------------------------------------------------------------------------
    type :: type_output_settings
        type(type_field_output) :: field_output
        type(types_history_output) :: history_output
        type(type_standard_output) :: standard_output
    end type type_output_settings
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

        type(input_basic) :: basic
        type(type_conditions) :: conditions
        type(type_output_settings) :: output_settings
        type(type_geometry) :: geometry
    contains
        procedure, pass(self), public :: initialize => initialize_type_input

        procedure :: read_parameters => inout_read_basic_parameters
        procedure :: read_conditions => inout_read_conditions
        procedure :: read_output_settings => inout_read_output_settings
        procedure :: read_geometry => inout_read_geometry

    end type type_input

    interface
        module subroutine inout_read_basic_parameters(self)
            implicit none
            class(type_input), intent(inout) :: self

        end subroutine inout_read_basic_parameters

        module subroutine inout_read_conditions(self)
            implicit none
            class(type_input), intent(inout) :: self

        end subroutine inout_read_conditions

        module subroutine inout_read_output_settings(self)
            implicit none
            class(type_input), intent(inout) :: self

        end subroutine inout_read_output_settings

        module subroutine inout_read_geometry(self)
            implicit none
            class(type_input), intent(inout) :: self

        end subroutine inout_read_geometry

    end interface

contains

    subroutine initialize_type_input(self)
        implicit none
        class(type_input), intent(inout) :: self

        character(len=:), allocatable :: fullpath
        character(len=:), allocatable :: local_input_path

#ifdef _MPI
        integer(int32) :: ierr, myrank
        integer(int32) :: error_flag = 0
#endif

        ! project_pathの取得は全員が行う
        self%project_path = get_project_path()

#ifdef _MPI
        call MPI_Comm_rank(MPI_COMM_WORLD, myrank, ierr)

        if (myrank == 0) then

            fullpath = trim(self%project_path)//"Input/"

            ! 1. ランク0が、新しい関数を使って各ファイルの存在をチェック
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

#else
        ! --- シリアル実行時の処理 ---
        fullpath = trim(self%project_path)//"Input/"
        if (.not. file_exists(fullpath//"Basic.json")) call error_message(902, c_opt=fullpath//"Basic.json")
        if (.not. file_exists(fullpath//"Conditions.json")) call error_message(902, c_opt=fullpath//"Conditions.json")
        if (.not. file_exists(fullpath//"Output.json")) call error_message(902, c_opt=fullpath//"Output.json")
#endif

        ! --- ここから先の処理は、全プロセスが同じファイル名を持っており、
        ! --- そのファイルが存在することが保証された状態で実行される ---

        local_input_path = trim(self%project_path)//"Input/"

        self%basic_file_name = local_input_path//"Basic.json"
        self%conditions_file_name = local_input_path//"Conditions.json"
        self%output_file_name = local_input_path//"Output.json"

        call self%read_parameters()
        call self%read_conditions()
        call self%read_output_settings()
        call self%read_geometry()

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
