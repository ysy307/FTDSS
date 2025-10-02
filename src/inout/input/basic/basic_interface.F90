module inout_input_basic
    use, intrinsic :: iso_fortran_env
!$  use :: omp_lib
    use :: mpi_f08
    use :: stdlib_strings, only:to_string, strip, ends_with
    use :: stdlib_logger
    use :: json_module, only:json_file
    use :: module_core
    use :: inout_input_base, only:get_json_value, abst_input
    implicit none
    private

    public :: type_input_basic

    character(*), parameter :: thermal = "thermal"
    character(*), parameter :: hydraulic = "hydraulic"
    character(*), parameter :: mechanical = "mechanical"

    !!------------------------------------------------------------------------------------------------------------------------------
    type :: type_simulation_settings
        character(:), allocatable :: title
        integer(int32) :: calculate_type
        integer(int32) :: calculate_dimension
    contains
        procedure, pass(self) :: display => display_simulation_settings
    end type type_simulation_settings

    interface
        module subroutine display_simulation_settings(self)
            implicit none
            class(type_simulation_settings) :: self
        end subroutine display_simulation_settings
    end interface
    !!------------------------------------------------------------------------------------------------------------------------------
    type :: type_analysis_controls
        logical :: is_active(NUM_INITIAL_CONDITIONS)
        integer(int32) :: coupling_mode
        logical :: partitioning
    contains
        procedure, pass(self) :: display => display_analysis_controls
    end type type_analysis_controls

    interface
        module subroutine display_analysis_controls(self)
            implicit none
            class(type_analysis_controls) :: self
        end subroutine display_analysis_controls
    end interface
    !!------------------------------------------------------------------------------------------------------------------------------
    type :: type_geometry_settings
        character(:), allocatable :: file_name
        character(:), allocatable :: global_node_id_key
        character(:), allocatable :: node_type_key
        character(:), allocatable :: num_sharing_ranks_key
        character(:), allocatable :: owner_ranks_key
        character(:), allocatable :: communication_partners_key
        character(:), allocatable :: cell_id_key
        character(:), allocatable :: rank_key
        character(:), allocatable :: color_key
        character(:), allocatable :: integration_type
        real(real64), allocatable :: integration_points(:)
    contains
        procedure, pass(self) :: display => display_geometry_settings
    end type type_geometry_settings

    interface
        module subroutine display_geometry_settings(self)
            implicit none
            class(type_geometry_settings) :: self

        end subroutine display_geometry_settings
    end interface
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
    contains
        procedure, pass(self) :: display => display_material_settings
    end type type_material_settings

    interface
        module subroutine display_material_settings(self)
            implicit none
            class(type_material_settings), intent(in) :: self
        end subroutine display_material_settings
    end interface
    !!------------------------------------------------------------------------------------------------------------------------------
    type :: type_convergence_criteria
        character(:), allocatable :: criteria
        character(:), allocatable :: logic
        real(real64) :: absolute_tolerance
        real(real64) :: relative_tolerance
    end type type_convergence_criteria

    type :: type_convergence
        character(:), allocatable :: use_criteria
        character(:), allocatable :: norm_type
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
    contains
        procedure, pass(self) :: display => display_parameters_solver_settings
    end type

    interface
        module subroutine display_parameters_solver_settings(self)
            implicit none
            class(type_solver_settings), intent(in) :: self
        end subroutine display_parameters_solver_settings
    end interface
    !!------------------------------------------------------------------------------------------------------------------------------
    type :: type_input_basic
        class(abst_input), pointer :: parent => null()
        character(:), allocatable :: file_name
        type(type_simulation_settings) :: simulation_settings
        type(type_analysis_controls) :: analysis_controls
        type(type_geometry_settings) :: geometry_settings
        integer(int32) :: num_materials
        type(type_material_settings), allocatable :: materials(:)
        type(type_solver_settings) :: solver_settings
    contains
        procedure, pass(self) :: initialize => initialize_type_input_basic
    end type type_input_basic

    interface
        module subroutine read_parameters_simulation_settings(self, json)
            implicit none
            class(type_input_basic), intent(inout) :: self
            type(json_file), intent(inout) :: json
        end subroutine read_parameters_simulation_settings

        module subroutine read_parameters_analysis_controls(self, json)
            implicit none
            class(type_input_basic), intent(inout) :: self
            type(json_file), intent(inout) :: json
        end subroutine read_parameters_analysis_controls

        module subroutine read_parameters_geometry_settings(self, json)
            implicit none
            class(type_input_basic), intent(inout) :: self
            type(json_file), intent(inout) :: json
        end subroutine read_parameters_geometry_settings

        module subroutine read_parameters_materials(self, json)
            implicit none
            class(type_input_basic), intent(inout) :: self
            type(json_file), intent(inout) :: json
        end subroutine read_parameters_materials

        module subroutine read_parameters_solver_settings(self, json)
            implicit none
            class(type_input_basic), intent(inout) :: self
            type(json_file), intent(inout) :: json
        end subroutine read_parameters_solver_settings
    end interface

contains
    subroutine initialize_type_input_basic(self)
        !< Load the input parameters from the JSON file
        implicit none
        class(type_input_basic), intent(inout) :: self
        type(json_file) :: json

        call json%initialize()

        call json%load(filename=self%file_name)
        call json%print_error_message(output_unit)

        call read_parameters_simulation_settings(self, json)
        call read_parameters_analysis_controls(self, json)
        call read_parameters_geometry_settings(self, json)
        call read_parameters_materials(self, json)
        call read_parameters_solver_settings(self, json)

        ! call MPI_Comm_rank(MPI_COMM_WORLD, myrank, ierr)
        ! if (myrank == 0) then
        !     write (*, '(A)') "=== Simulation Settings ==="
        !     call self%simulation_settings%display()
        !     write (*, '(A)') "=== Analysis Controls ==="
        !     call self%analysis_controls%display()
        !     write (*, '(A)') "=== Geometry Settings ==="
        !     call self%geometry_settings%display()
        !     write (*, '(A)') "=== Material Settings ==="
        !     do i = 1, self%num_materials
        !         call self%materials(i)%display()
        !     end do
        !     write (*, '(A)') "=== Solver Settings ==="
        !     call self%solver_settings%display()
        ! end if
        call json%destroy()
        call json%print_error_message(output_unit)

    end subroutine initialize_type_input_basic
    !!------------------------------------------------------------------------------------------------------------------------------

end module inout_input_basic
