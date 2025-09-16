module inout_input_basic
    use, intrinsic :: iso_fortran_env
!$  use :: omp_lib
    use :: mpi_f08
    use :: stdlib_strings, only:to_string, strip, ends_with
    use :: stdlib_logger
    use :: json_module, only:json_file
    use :: module_core, only:join, error_message
    use :: inout_input_base, only:get_json_value
    implicit none
    private

    public :: type_input_basic

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
        logical :: calculate_thermal
        logical :: calculate_hydraulic
        logical :: calculate_mechanical
        character(:), allocatable :: coupling_mode
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
    type :: type_input_basic
        character(:), allocatable, private :: file_name
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

        call json%destroy()
        call json%print_error_message(output_unit)

    end subroutine initialize_type_input_basic
    !!------------------------------------------------------------------------------------------------------------------------------

end module inout_input_basic

!        !!------------------------------------------------------------------------------------------------------------------------------
!     ! JSON key names for analysis controls
!     !!------------------------------------------------------------------------------------------------------------------------------
!     character(*), parameter :: analysis_controls = "analysis_controls"
!     character(*), parameter :: calculate_thermal = "calculate_thermal"
!     character(*), parameter :: calculate_hydraulic = "calculate_hydraulic"
!     character(*), parameter :: calculate_mechanical = "calculate_mechanical"
!     character(*), parameter :: coupling_mode = "coupling_mode"
!     character(*), parameter :: coupling_modes(3) = ["none", "weak", "strong"]
!     !!------------------------------------------------------------------------------------------------------------------------------
!     ! JSON key names for geometry settings
!     !!------------------------------------------------------------------------------------------------------------------------------
!     character(*), parameter :: geometry_settings = "geometry_settings"
!     character(*), parameter :: file_name = "file_name"
!     character(*), parameter :: cell_id_array_name = "cell_id_array_name"
!     character(*), parameter :: integration = "integration"
!     character(*), parameter :: integration_type = "type"
!     character(*), parameter :: integration_types(3) = ["full", "reduced", "free"]
!     character(*), parameter :: integration_points = "points"
!     !!------------------------------------------------------------------------------------------------------------------------------
!     ! JSON key names for materials
!     !!------------------------------------------------------------------------------------------------------------------------------
!     character(*), parameter :: materials = "materials"
!     character(*), parameter :: id = "id"
!     character(*), parameter :: name = "name"
!     character(*), parameter :: phase = "phase"
!     character(*), parameter :: is_frozen = "is_frozen"
!     character(*), parameter :: is_dispersed = "is_dispersed"
!     character(*), parameter :: thermal = "thermal"
!     character(*), parameter :: denstiy = "density"
!     character(*), parameter :: specific_heat = "specific_heat"
!     character(*), parameter :: thermal_conductivity = "thermal_conductivity"
!     character(*), parameter :: dispersivity = "dispersivity"
!     character(*), parameter :: phase_change = "phase_change"
!     character(*), parameter :: latent_heat = "latent_heat"
!     character(*), parameter :: fusion = "fusion"
!     character(*), parameter :: freezeing_temperature = "freezing_temperature"
!     character(*), parameter :: unfrozen_water_model = "unfrozen_water_model"
!     character(*), parameter :: model_number = "model_number"
!     character(*), parameter :: theta_s = "theta_s"
!     character(*), parameter :: theta_r = "theta_r"
!     character(*), parameter :: n1 = "n1"
!     character(*), parameter :: n2 = "n2"
!     character(*), parameter :: alpha1 = "alpha1"
!     character(*), parameter :: alpha2 = "alpha2"
!     character(*), parameter :: w1 = "w1"
!     character(*), parameter :: h_crit = "h_crit"
!     character(*), parameter :: equilibrium_model = "equilibrium_model"
!     character(*), parameter :: segregation = "segregation"
!     character(*), parameter :: unit = "unit"
!     character(*), parameter :: valid_gcc_units(2) = ["m", "pa"]
!     character(*), parameter :: hydraulic = "hydraulic"
!     character(*), parameter :: hydraulic_conductivity_model = "hydraulic_conductivity_model"
!     character(*), parameter :: saturated_conductivity = "saturated_conductivity"
!     character(*), parameter :: l = "l"
!     character(*), parameter :: impedance_factor = "impedance_factor"
!     character(*), parameter :: water_viscosity_model = "water_viscosity_model"
!     character(*), parameter :: water_retention_model = "water_retention_model"
!     character(*), parameter :: mechanical = "mechanical"
!     !!------------------------------------------------------------------------------------------------------------------------------
!     ! JSON key names for solver settings
!     !!------------------------------------------------------------------------------------------------------------------------------
!     character(*), parameter :: solver_settings = "solver_settings"
!     character(*), parameter :: bdf_order = "bdf_order"
!     character(*), parameter :: reordering = "reordering"
!     character(*), parameter :: valid_reordering_types(3) = [character(len=16) :: "none", "cm", "rcm"]
!     character(*), parameter :: coloring = "coloring"
!     character(*), parameter :: coloring_types(4) = [character(len=16) :: "none", "welch_powell", "lfo", "dsatur"]
!     character(*), parameter :: nonlinear_solver = "nonlinear_solver"
!     character(*), parameter :: method = "method"
!     character(*), parameter :: valid_nonlinear_solver_methods(4) = [character(len=16) :: "none", "newton", "modified_newton", "picard"]
!     character(*), parameter :: update_frequency = "update_frequency"
!     character(*), parameter :: max_iterations = "max_iterations"
!     character(*), parameter :: convergence = "convergence"
!     character(*), parameter :: use_criteria = "use_criteria"
!     character(*), parameter :: valid_criteria_types(3) = [character(len=16) :: "residual", "update", "both"]
!     character(*), parameter :: logic_between_criteria = "logic_between_criteria"
!     character(*), parameter :: valid_logic_types(2) = [character(len=16) :: "and", "or"]
!     character(*), parameter :: residual = "residual"
!     character(*), parameter :: update = "update"
!     character(*), parameter :: criteria = "criteria"
!     character(*), parameter :: valid_local_criteria_types(3) = [character(len=16) :: "absolute", "relative", "both"]
!     character(*), parameter :: logic = "logic"
!     character(*), parameter :: absolute_tolerance = "absolute_tolerance"
!     character(*), parameter :: relative_tolerance = "relative_tolerance"
!     character(*), parameter :: linear_solver = "linear_solver"
!     character(*), parameter :: valid_linear_solver_methods(2) = [character(len=16) :: "direct", "iterative"]
!     character(*), parameter :: iterative_solver = "iterative_solver"
!     character(*), parameter :: solver_type = "solver_type"
!     character(*), parameter :: preconditioner_type = "preconditioner_type"
!     character(*), parameter :: tolerance = "tolerance"
!     character(*), parameter :: parallel_settings = "parallel_settings"
!     character(*), parameter :: threads = "threads"
!     character(*), parameter :: is_parallel = "is_parallel"
!     character(*), parameter :: num_threads = "num_threads"
!     character(*), parameter :: schedule = "schedule"
!     character(*), parameter :: valid_schedule_types(6) = [character(len=16) :: &
!                                                           "affinity", "auto", "dynamic", "guided", "runtime", "static"]
!     character(*), parameter :: max_active_levels = "max_active_levels"
!     !!------------------------------------------------------------------------------------------------------------------------------

! contains
!     module subroutine inout_read_basic_parameters(self)
!         !< Load the input parameters from the JSON file
!         implicit none
!         class(type_input), intent(inout) :: self
!         type(json_file) :: json

!         call json%initialize()

!         call json%load(filename=self%basic_file_name)
!         call json%print_error_message(output_unit)

!         call read_parameters_simulation_settings(self, json)
!         call read_parameters_analysis_controls(self, json)
!         call read_parameters_geometry_settings(self, json)
!         call read_parameters_materials(self, json)
!         call read_parameters_solver_settings(self, json)

!         call json%destroy()
!         call json%print_error_message(output_unit)

!     end subroutine inout_read_basic_parameters

!     subroutine read_parameters_simulation_settings(self, json)
!         !> Load the basic input parameters from the JSON file
!         implicit none
!         class(type_input) :: self
!         type(json_file), intent(inout) :: json !! JSON parser

!         character(:), allocatable :: key
!         logical :: found

!         key = join([simulation_settins, title])
!         call json%get(key, self%basic%simulation_settings%title, found)
!         call json%print_error_message(output_unit)
!         if (.not. found) self%basic%simulation_settings%title = "FTDSS Simulation"

!         key = join([simulation_settins, calculate_type])
!         call json%get(key, self%basic%simulation_settings%calculate_type, found)
!         call json%print_error_message(output_unit)
!         if (.not. found) then
!             call json%destroy()
!             call error_message(904, c_opt=key)
!         else if (.not. value_in_range(self%basic%simulation_settings%calculate_type, &
!                                       min_calculation_type, max_calculation_type)) then
!             call json%destroy()
!             call error_message(905, c_opt=key)
!         end if

!         select case (self%basic%simulation_settings%calculate_type)
!         case (1:2)
!             self%basic%simulation_settings%calculate_dimension = 2
!         case (3)
!             self%basic%simulation_settings%calculate_dimension = 3
!         end select

!     end subroutine read_parameters_simulation_settings

!     subroutine read_parameters_analysis_controls(self, json)
!         !> Load the analysis control parameters from the JSON file
!         implicit none
!         class(type_input) :: self
!         type(json_file), intent(inout) :: json !! JSON parser

!         logical :: found
!         character(:), allocatable :: key

!         key = join([analysis_controls, calculate_thermal])
!         call json%get(key, self%basic%analysis_controls%calculate_thermal, found)
!         call json%print_error_message(output_unit)
!         if (.not. found) self%basic%analysis_controls%calculate_thermal = .false.

!         key = join([analysis_controls, calculate_hydraulic])
!         call json%get(key, self%basic%analysis_controls%calculate_hydraulic, found)
!         call json%print_error_message(output_unit)
!         if (.not. found) self%basic%analysis_controls%calculate_hydraulic = .false.

!         key = join([analysis_controls, calculate_mechanical])
!         call json%get(key, self%basic%analysis_controls%calculate_mechanical, found)
!         call json%print_error_message(output_unit)
!         if (.not. found) self%basic%analysis_controls%calculate_mechanical = .false.

!         if (.not. self%basic%analysis_controls%calculate_thermal .and. &
!             .not. self%basic%analysis_controls%calculate_hydraulic .and. &
!             .not. self%basic%analysis_controls%calculate_mechanical) then
!             call json%destroy()
!             call error_message(905, c_opt=analysis_controls)
!         end if

!         key = join([analysis_controls, coupling_mode])
!         call json%get(key, self%basic%analysis_controls%coupling_mode, found)
!         call json%print_error_message(output_unit)
!         if (.not. found) then
!             self%basic%analysis_controls%coupling_mode = "weak"
!         else if (.not. any(coppling_modes(:) == self%basic%analysis_controls%coupling_mode)) then
!             call json%destroy()
!             call error_message(905, c_opt=key)
!         end if

!     end subroutine read_parameters_analysis_controls

!     subroutine read_parameters_geometry_settings(self, json)
!         !> Load the geometry settings from the JSON file
!         implicit none
!         class(type_input) :: self
!         type(json_file), intent(inout) :: json !! JSON parser

!         logical :: found
!         character(:), allocatable :: key

!         key = join([geometry_settings, file_name])
!         call json%get(key, self%basic%geometry_settings%file_name, found)
!         call json%print_error_message(output_unit)
!         if (.not. found) then
!             call json%destroy()
!             call error_message(904, c_opt=key)
!         end if

!         self%geometry_file_name = self%project_path//"Input/"//trim(adjustl(self%basic%geometry_settings%file_name))
! #ifndef _MPI
!         inquire (file=self%geometry_file_name, exist=found)
!         if (.not. found) then
!             call json%destroy()
!             call error_message(902, c_opt=self%geometry_file_name)
!         end if
! #endif

!         key = join([geometry_settings, cell_id_array_name])
!         call json%get(key, self%basic%geometry_settings%cell_id_array_name, found)
!         call json%print_error_message(output_unit)
!         if (.not. found) then
!             call json%destroy()
!             call error_message(904, c_opt=key)
!         end if

!         key = join([geometry_settings, integration, integration_type])
!         call json%get(key, self%basic%geometry_settings%integration_type, found)
!         call json%print_error_message(output_unit)
!         if (.not. found) then
!             call json%destroy()
!             call error_message(904, c_opt=key)
!         else if (.not. any(integration_types(:) == self%basic%geometry_settings%integration_type)) then
!             call json%destroy()
!             call error_message(905, c_opt=key)
!         end if

!         if (self%basic%geometry_settings%integration_type == "free") then
!             key = join([geometry_settings, integration, integration_points])
!             call json%get(key, self%basic%geometry_settings%integration_points, found)
!             call json%print_error_message(output_unit)
!             if (.not. found) then
!                 call json%destroy()
!                 call error_message(904, c_opt=key)
!             else if (self%basic%geometry_settings%integration_points < 0.0d0 .or. &
!                      self%basic%geometry_settings%integration_points > 1.0d0) then
!                 call json%destroy()
!                 call error_message(905, c_opt=key)
!             end if
!         end if

!     end subroutine read_parameters_geometry_settings

!     subroutine read_parameters_materials(self, json)
!         !> Load the material parameters from the JSON file
!         implicit none
!         class(type_input) :: self
!         type(json_file), intent(inout) :: json !! JSON parser

!         logical :: found
!         character(:), allocatable :: key
!         integer(int32) :: i

!         call json%info(materials, found=found, n_children=self%basic%num_materials)
!         call json%print_error_message(output_unit)
!         if (.not. found .or. self%basic%num_materials <= 0) then
!             call json%destroy()
!             call error_message(904, c_opt=materials)
!         end if

!         if (allocated(self%basic%materials)) deallocate (self%basic%materials)
!         allocate (self%basic%materials(self%basic%num_materials))

!         do i = 1, self%basic%num_materials
!             call read_parameters_materials_basic(self, json, i)
!             if (self%basic%analysis_controls%calculate_thermal) then
!                 call read_parameters_materials_thermal(self, json, i)
!             end if
!             if (self%basic%analysis_controls%calculate_hydraulic) then
!                 call read_parameters_materials_hydrauilic(self, json, i)
!             end if
!             if (self%basic%analysis_controls%calculate_mechanical) then
!                 ! Mechanical parameters can be added here in the future
!             end if
!         end do

!     end subroutine read_parameters_materials

!     subroutine read_parameters_materials_basic(self, json, i)
!         !> Load the basic material parameters from the JSON file
!         implicit none
!         class(type_input) :: self
!         type(json_file), intent(inout) :: json !! JSON parser
!         integer(int32), intent(in) :: i !! Material index

!         logical :: found
!         character(:), allocatable :: key
!         character(:), allocatable :: key_material

!         key_material = join([materials//"("//to_string(i)//")"])

!         key = join([key_material, id])
!         call json%get(key, self%basic%materials(i)%id, found)
!         call json%print_error_message(output_unit)
!         if (.not. found) then
!             call json%destroy()
!             call error_message(904, c_opt=key)
!         end if

!         key = join([key_material, name])
!         call json%get(key, self%basic%materials(i)%name, found)
!         call json%print_error_message(output_unit)
!         if (.not. found) self%basic%materials(i)%name = "Material_"//to_string(i)
!         self%basic%materials(i)%name = trim(adjustl(self%basic%materials(i)%name))

!         key = join([key_material, phase])
!         call json%get(key, self%basic%materials(i)%phase, found)
!         call json%print_error_message(output_unit)
!         if (.not. found) then
!             call json%destroy()
!             call error_message(904, c_opt=key)
!         else if (.not. value_in_range(self%basic%materials(i)%phase, 1, 4)) then
!             call json%destroy()
!             call error_message(905, c_opt=key)
!         end if

!         key = join([key_material, is_frozen])
!         call json%get(key, self%basic%materials(i)%is_frozen, found)
!         call json%print_error_message(output_unit)
!         if (.not. found) self%basic%materials(i)%is_frozen = .false.

!         key = join([key_material, is_dispersed])
!         call json%get(key, self%basic%materials(i)%is_dispersed, found)
!         call json%print_error_message(output_unit)
!         if (.not. found) self%basic%materials(i)%is_dispersed = .false.

!         if (self%basic%analysis_controls%calculate_thermal) then
!             key = join([key_material, calculate_thermal])
!             call json%get(key, self%basic%materials(i)%calculate_thermal, found)
!             call json%print_error_message(output_unit)
!             if (.not. found) self%basic%materials(i)%calculate_thermal = .false.
!         else
!             self%basic%materials(i)%calculate_thermal = .false.
!         end if

!         if (self%basic%analysis_controls%calculate_hydraulic) then
!             key = join([key_material, calculate_hydraulic])
!             call json%get(key, self%basic%materials(i)%calculate_hydraulic, found)
!             call json%print_error_message(output_unit)
!             if (.not. found) self%basic%materials(i)%calculate_hydraulic = .false.
!         else
!             self%basic%materials(i)%calculate_hydraulic = .false.
!         end if

!         if (self%basic%analysis_controls%calculate_mechanical) then
!             key = join([key_material, calculate_mechanical])
!             call json%get(key, self%basic%materials(i)%calculate_mechanical, found)
!             call json%print_error_message(output_unit)
!             if (.not. found) self%basic%materials(i)%calculate_mechanical = .false.
!         else
!             self%basic%materials(i)%calculate_mechanical = .false.
!         end if

!     end subroutine read_parameters_materials_basic

!     subroutine read_parameters_materials_thermal(self, json, i)
!         implicit none
!         class(type_input) :: self
!         type(json_file), intent(inout) :: json !! JSON parser
!         integer(int32), intent(in) :: i !! Material index

!         logical :: found
!         character(:), allocatable :: key
!         character(:), allocatable :: key_material

!         key_material = join([materials//"("//to_string(i)//")", thermal])

!         key = join([key_material, denstiy])
!         call json%get(key, self%basic%materials(i)%thermal%density, found)
!         call json%print_error_message(output_unit)
!         if (.not. found) then
!             call json%destroy()
!             call error_message(904, c_opt=key)
!         else if (any(self%basic%materials(i)%thermal%density(:) <= 0.0d0) .or. &
!                  size(self%basic%materials(i)%thermal%density(:)) /= self%basic%materials(i)%phase) then
!             call json%destroy()
!             call error_message(905, c_opt=key)
!         end if

!         key = join([key_material, specific_heat])
!         call json%get(key, self%basic%materials(i)%thermal%specific_heat, found)
!         call json%print_error_message(output_unit)
!         if (.not. found) then
!             call json%destroy()
!             call error_message(904, c_opt=key)
!         else if (any(self%basic%materials(i)%thermal%specific_heat(:) <= 0.0d0) .or. &
!                  size(self%basic%materials(i)%thermal%specific_heat(:)) /= self%basic%materials(i)%phase) then
!             call json%destroy()
!             call error_message(905, c_opt=key)
!         end if

!         key = join([key_material, thermal_conductivity])
!         call json%get(key, self%basic%materials(i)%thermal%thermal_conductivity, found)
!         call json%print_error_message(output_unit)
!         if (.not. found) then
!             call json%destroy()
!             call error_message(904, c_opt=key)
!         else if (any(self%basic%materials(i)%thermal%thermal_conductivity(:) <= 0.0d0) .or. &
!                  size(self%basic%materials(i)%thermal%thermal_conductivity(:)) /= self%basic%materials(i)%phase) then
!             call json%destroy()
!             call error_message(905, c_opt=key)
!         end if

!         if (self%basic%materials(i)%is_dispersed) then
!             key = join([key_material, dispersivity])
!             call json%get(key, self%basic%materials(i)%thermal%thermal_conductivity_dispersity, found)
!             call json%print_error_message(output_unit)
!             if (.not. found) then
!                 call json%destroy()
!                 call error_message(904, c_opt=key)
!             else if (any(self%basic%materials(i)%thermal%thermal_conductivity_dispersity(:) < 0.0d0) .or. &
!                      size(self%basic%materials(i)%thermal%thermal_conductivity_dispersity(:)) /= 2) then
!                 call json%destroy()
!                 call error_message(905, c_opt=key)
!             end if
!         else
!             call allocate_array(self%basic%materials(i)%thermal%thermal_conductivity_dispersity, 1)
!             self%basic%materials(i)%thermal%thermal_conductivity_dispersity = 0.0d0
!         end if

!         if (self%basic%materials(i)%is_frozen) then
!             key = join([key_material, phase_change, latent_heat, fusion])
!             call json%get(key, self%basic%materials(i)%thermal%phase_change%latent_heat_fusion, found)
!             call json%print_error_message(output_unit)
!             if (.not. found) then
!                 call json%destroy()
!                 call error_message(904, c_opt=key)
!             end if
!             if (self%basic%materials(i)%thermal%phase_change%latent_heat_fusion <= 0.0d0) then
!                 call json%destroy()
!                 call error_message(905, c_opt=key)
!             end if

!             key = join([key_material, phase_change, freezeing_temperature])
!             call json%get(key, self%basic%materials(i)%thermal%phase_change%freezing_temperature, found)
!             call json%print_error_message(output_unit)
!             if (.not. found) then
!                 call json%destroy()
!                 call error_message(904, c_opt=key)
!             else if (self%basic%materials(i)%thermal%phase_change%freezing_temperature > 0.0d0) then
!                 call json%destroy()
!                 call error_message(905, c_opt=key)
!             end if

!             key = join([key_material, phase_change, unfrozen_water_model])
!             call read_parameters_materials_wrf(self%basic%materials(i)%thermal%phase_change%wrf, json, key)

!             key = join([key_material, phase_change, equilibrium_model, segregation])
!             call json%get(key, self%basic%materials(i)%thermal%phase_change%gcc%is_segregation, found)
!             call json%print_error_message(output_unit)
!             if (.not. found) then
!                 call json%destroy()
!                 call error_message(904, c_opt=key)
!             end if

!             key = join([key_material, phase_change, equilibrium_model, unit])
!             call json%get(key, self%basic%materials(i)%thermal%phase_change%gcc%unit, found)
!             call json%print_error_message(output_unit)
!             if (.not. found) then
!                 call json%destroy()
!                 call error_message(904, c_opt=key)
!             else if (.not. any(valid_gcc_units(:) == self%basic%materials(i)%thermal%phase_change%gcc%unit)) then
!                 call json%destroy()
!                 call error_message(905, c_opt=key)
!             end if

!         end if

!     end subroutine read_parameters_materials_thermal

!     subroutine read_parameters_materials_hydrauilic(self, json, i)
!         !> Load the hydraulic parameters from the JSON file
!         implicit none
!         class(type_input) :: self
!         type(json_file), intent(inout) :: json !! JSON parser
!         integer(int32), intent(in) :: i !! Material index

!         logical :: found
!         character(:), allocatable :: key
!         character(:), allocatable :: key_material

!         key_material = join([materials//"("//to_string(i)//")", hydraulic])

!         key = join([key_material, hydraulic_conductivity_model, model_number])
!         call json%get(key, self%basic%materials(i)%hydraulic%model_number, found)
!         call json%print_error_message(output_unit)
!         if (.not. found) then
!             call json%destroy()
!             call error_message(904, c_opt=key)
!         else if (.not. value_in_range(self%basic%materials(i)%hydraulic%model_number, 1, 5)) then
!             call json%destroy()
!             call error_message(905, c_opt=key)
!         end if

!         key = join([key_material, hydraulic_conductivity_model, saturated_conductivity])
!         call json%get(key, self%basic%materials(i)%hydraulic%hydraulic_conductivity, found)
!         call json%print_error_message(output_unit)
!         if (.not. found) then
!             call json%destroy()
!             call error_message(904, c_opt=key)
!         else if (self%basic%materials(i)%hydraulic%hydraulic_conductivity <= 0.0d0) then
!             call json%destroy()
!             call error_message(905, c_opt=key)
!         end if

!         select case (self%basic%materials(i)%hydraulic%model_number)
!         case (1)
!             key = join([key_material, impedance_factor])
!             call json%get(key, self%basic%materials(i)%hydraulic%impedance_factor, found)
!             call json%print_error_message(output_unit)
!             if (.not. found) then
!                 call json%destroy()
!                 call error_message(904, c_opt=key)
!             else if (self%basic%materials(i)%hydraulic%impedance_factor <= 0.0d0) then
!                 call json%destroy()
!                 call error_message(905, c_opt=key)
!             end if
!         case (2)
!             key = join([key_material, water_retention_model])
!             call read_parameters_materials_wrf(self%basic%materials(i)%hydraulic%hcf, json, key)
!         case (3)
!             key = join([key_material, water_retention_model])
!             call read_parameters_materials_wrf(self%basic%materials(i)%hydraulic%hcf, json, key)

!             key = join([key_material, water_viscosity_model])
!             call json%get(key, self%basic%materials(i)%hydraulic%water_viscosity_model, found)
!             call json%print_error_message(output_unit)
!             if (.not. found) then
!                 call json%destroy()
!                 call error_message(904, c_opt=key)
!             else if (.not. value_in_range(self%basic%materials(i)%hydraulic%water_viscosity_model, 0, 2)) then
!                 call json%destroy()
!                 call error_message(905, c_opt=key)
!             end if

!         case (4)
!             key = join([key_material, impedance_factor])
!             call json%get(key, self%basic%materials(i)%hydraulic%impedance_factor, found)
!             call json%print_error_message(output_unit)
!             if (.not. found) then
!                 call json%destroy()
!                 call error_message(904, c_opt=key)
!             else if (self%basic%materials(i)%hydraulic%impedance_factor <= 0.0d0) then
!                 call json%destroy()
!                 call error_message(905, c_opt=key)
!             end if

!             key = join([key_material, water_retention_model])
!             call read_parameters_materials_wrf(self%basic%materials(i)%hydraulic%hcf, json, key)
!         case (5)
!             key = join([key_material, impedance_factor])
!             call json%get(key, self%basic%materials(i)%hydraulic%impedance_factor, found)
!             call json%print_error_message(output_unit)
!             if (.not. found) then
!                 call json%destroy()
!                 call error_message(904, c_opt=key)
!             else if (self%basic%materials(i)%hydraulic%impedance_factor <= 0.0d0) then
!                 call json%destroy()
!                 call error_message(905, c_opt=key)
!             end if

!             key = join([key_material, water_retention_model])
!             call read_parameters_materials_wrf(self%basic%materials(i)%hydraulic%hcf, json, key)

!             key = join([key_material, water_viscosity_model])
!             call json%get(key, self%basic%materials(i)%hydraulic%water_viscosity_model, found)
!             call json%print_error_message(output_unit)
!             if (.not. found) then
!                 call json%destroy()
!                 call error_message(904, c_opt=key)
!             else if (.not. value_in_range(self%basic%materials(i)%hydraulic%water_viscosity_model, 0, 2)) then
!                 call json%destroy()
!                 call error_message(905, c_opt=key)
!             end if
!         end select

!     end subroutine read_parameters_materials_hydrauilic

!     subroutine read_parameters_materials_wrf(wrf, json, key_base)
!         ! 引数:
!         !   wrf: wrfまたはhcf型のインスタンス (多態性のためclassで宣言)
!         !   json: JSONパーサーのインスタンス
!         !   key_base: パラメータへの基底パス

!         class(type_materials_wrf), intent(inout) :: wrf
!         type(json_file), intent(inout) :: json
!         character(*), intent(in) :: key_base

!         ! ローカル変数
!         logical :: found
!         character(:), allocatable :: key

!         ! ----------------------------------------------------------------
!         ! 1. 共通パラメータの読み込み (wrfとhcfの両方に存在する)
!         ! ----------------------------------------------------------------

!         ! model_numberの読み込みとチェック
!         key = join([key_base, 'model_number'])
!         call json%get(key, wrf%model_number, found)
!         call json%print_error_message(output_unit)
!         if (.not. found) then
!             call json%destroy()
!             call error_message(904, c_opt=key)
!         else if (.not. value_in_range(wrf%model_number, 1, 6)) then
!             call json%destroy()
!             call error_message(905, c_opt=key)
!         end if

!         ! theta_sの読み込みとチェック
!         key = join([key_base, 'theta_s'])
!         call json%get(key, wrf%theta_s, found)
!         call json%print_error_message(output_unit)
!         if (.not. found) then
!             call json%destroy()
!             call error_message(904, c_opt=key)
!         end if

!         ! theta_rの読み込みとチェック
!         key = join([key_base, 'theta_r'])
!         call json%get(key, wrf%theta_r, found)
!         call json%print_error_message(output_unit)
!         if (.not. found) then
!             call json%destroy()
!             call error_message(904, c_opt=key)
!         else if (wrf%theta_s <= wrf%theta_r .or. &
!                  wrf%theta_s <= 0.0d0 .or. &
!                  wrf%theta_r < 0.0d0) then
!             call json%destroy()
!             call error_message(905, c_opt=key)
!         end if

!         ! alpha1の読み込みとチェック
!         key = join([key_base, 'alpha1'])
!         call json%get(key, wrf%alpha1, found)
!         call json%print_error_message(output_unit)
!         if (.not. found) then
!             call json%destroy()
!             call error_message(904, c_opt=key)
!         end if

!         ! n1の読み込みとチェック
!         key = join([key_base, 'n1'])
!         call json%get(key, wrf%n1, found)
!         call json%print_error_message(output_unit)
!         if (.not. found) then
!             call json%destroy()
!             call error_message(904, c_opt=key)
!         else if (wrf%n1 <= 1.0d0) then
!             call json%destroy()
!             call error_message(905, c_opt=key)
!         end if

!         wrf%m1 = 1.0d0 - 1.0d0 / wrf%n1

!         ! model_numberに応じた共通パラメータの読み込み
!         select case (wrf%model_number)
!         case (4)
!             key = join([key_base, 'h_crit'])
!             call json%get(key, wrf%h_crit, found)
!             call json%print_error_message(output_unit)
!             if (.not. found) then
!                 call json%destroy()
!                 call error_message(904, c_opt=key)
!             else if (wrf%h_crit > 0.0d0) then
!                 call json%destroy()
!                 call error_message(905, c_opt=key)
!             end if

!         case (5)
!             key = join([key_base, 'alpha2'])
!             call json%get(key, wrf%alpha2, found)
!             call json%print_error_message(output_unit)
!             if (.not. found) then
!                 call json%destroy()
!                 call error_message(904, c_opt=key)
!             else if (wrf%alpha2 <= 0.0d0) then
!                 call json%destroy()
!                 call error_message(905, c_opt=key)
!             end if

!             key = join([key_base, 'n2'])
!             call json%get(key, wrf%n2, found)
!             call json%print_error_message(output_unit)
!             if (.not. found) then
!                 call json%destroy()
!                 call error_message(904, c_opt=key)
!             else if (wrf%n2 <= 1.0d0) then
!                 call json%destroy()
!                 call error_message(905, c_opt=key)
!             end if
!             wrf%m2 = 1.0d0 - 1.0d0 / wrf%n2

!             key = join([key_base, 'w1'])
!             call json%get(key, wrf%w1, found)
!             call json%print_error_message(output_unit)
!             if (.not. found) then
!                 call json%destroy()
!                 call error_message(904, c_opt=key)
!             else if (wrf%w1 < 0.0d0 .or. wrf%w1 > 1.0d0) then
!                 call json%destroy()
!                 call error_message(905, c_opt=key)
!             end if
!             wrf%w2 = 1.0d0 - wrf%w1

!         case (6)
!             key = join([key_base, 'n2'])
!             call json%get(key, wrf%n2, found)
!             call json%print_error_message(output_unit)
!             if (.not. found) then
!                 call json%destroy()
!                 call error_message(904, c_opt=key)
!             else if (wrf%n2 <= 1.0d0) then
!                 call json%destroy()
!                 call error_message(905, c_opt=key)
!             end if
!             wrf%m2 = 1.0d0 - 1.0d0 / wrf%n2

!             key = join([key_base, 'w1'])
!             call json%get(key, wrf%w1, found)
!             call json%print_error_message(output_unit)
!             if (.not. found) then
!                 call json%destroy()
!                 call error_message(904, c_opt=key)
!             else if (wrf%w1 < 0.0d0 .or. wrf%w1 > 1.0d0) then
!                 call json%destroy()
!                 call error_message(905, c_opt=key)
!             end if
!             wrf%w2 = 1.0d0 - wrf%w1
!         end select

!         ! ----------------------------------------------------------------
!         ! 2. 型に固有のパラメータを読み込む
!         ! ----------------------------------------------------------------
!         select type (wrf)
!         type is (type_materials_hcf)
!             key = join([key_base, 'l'])
!             call json%get(key, wrf%l, found)
!             call json%print_error_message(output_unit)
!             if (.not. found) then
!                 call json%destroy()
!                 call error_message(904, c_opt=key)
!             else if (wrf%l <= 0.0d0) then
!                 call json%destroy()
!                 call error_message(905, c_opt=key)
!             end if

!         class default
!             ! do nothing
!         end select

!     end subroutine read_parameters_materials_wrf

!     subroutine read_parameters_solver_settings(self, json)
!         implicit none
!         class(type_input) :: self
!         type(json_file), intent(inout) :: json

!         logical :: found
!         character(:), allocatable :: key

!         key = join([solver_settings, bdf_order])
!         call json%get(key, self%basic%solver_settings%bdf_order, found)
!         call json%print_error_message(output_unit)
!         if (.not. found) then
!             call json%destroy()
!             call error_message(904, c_opt=key)
!         else if (.not. value_in_range(self%basic%solver_settings%bdf_order, 1, 6)) then
!             call json%destroy()
!             call error_message(905, c_opt=key)
!         end if

!         key = join([solver_settings, reordering])
!         call json%get(key, self%basic%solver_settings%reordering, found)
!         call json%print_error_message(output_unit)
!         if (.not. found) then
!             call global_logger%log_warning(message="Default reordering is set to 'none'.")
!             self%basic%solver_settings%reordering = "none"
!         else if (.not. any(valid_reordering_types(:) == self%basic%solver_settings%reordering)) then
!             call json%destroy()
!             call error_message(905, c_opt=key)
!         end if

!         key = join([solver_settings, coloring])
!         call json%get(key, self%basic%solver_settings%coloring, found)
!         call json%print_error_message(output_unit)
!         if (.not. found) then
!             call global_logger%log_warning(message="Default coloring is set to 'none'.")
!             self%basic%solver_settings%coloring = "none"
!         else if (.not. any(coloring_types(:) == self%basic%solver_settings%coloring)) then
!             call json%destroy()
!             call error_message(905, c_opt=key)
!         end if

!         call read_parameters_solver_settings_nonlinear(self, json)
!         call read_parameters_solver_settings_linear(self, json)
!         call read_parameters_solver_parallel_settings(self, json)

!     end subroutine read_parameters_solver_settings

!     subroutine read_parameters_solver_settings_nonlinear(self, json)
!         implicit none
!         class(type_input) :: self
!         type(json_file), intent(inout) :: json

!         logical :: found
!         character(:), allocatable :: key
!         character(:), allocatable :: key_base

!         key = join([solver_settings, nonlinear_solver, method])
!         call json%get(key, self%basic%solver_settings%nonlinear_solver%method, found)
!         call json%print_error_message(output_unit)
!         if (.not. found) then
!             call global_logger%log_warning(message="Default nonlinear solver method is set to 'none'.")
!             self%basic%solver_settings%nonlinear_solver%method = "none"
!         else if (.not. any(valid_nonlinear_solver_methods(:) == self%basic%solver_settings%nonlinear_solver%method)) then
!             call json%destroy()
!             call error_message(905, c_opt=key)
!         end if

!         !! If the method is modified newton then read the additional parameters
!         select case (self%basic%solver_settings%nonlinear_solver%method)
!         case (valid_nonlinear_solver_methods(3))
!             key = join([solver_settings, nonlinear_solver, update_frequency])
!             call json%get(key, self%basic%solver_settings%nonlinear_solver%update_frequency, found)
!             call json%print_error_message(output_unit)
!             if (.not. found) then
!                 call global_logger%log_warning(message="Using default update frequency of 5 for modified Newton method.")
!                 self%basic%solver_settings%nonlinear_solver%update_frequency = 5
!             end if
!         end select

!         select case (self%basic%solver_settings%nonlinear_solver%method)
!         case (valid_nonlinear_solver_methods(2), valid_nonlinear_solver_methods(3), valid_nonlinear_solver_methods(4))
!             key = join([solver_settings, nonlinear_solver, max_iterations])
!             call json%get(key, self%basic%solver_settings%nonlinear_solver%max_iterations, found)
!             call json%print_error_message(output_unit)
!             if (.not. found) then
!                 call global_logger%log_warning(message="Using default maximum iterations of 1000 for nonlinear solver.")
!                 self%basic%solver_settings%nonlinear_solver%max_iterations = 1000
!             else if (self%basic%solver_settings%nonlinear_solver%max_iterations < 0) then
!                 call json%destroy()
!                 call error_message(905, c_opt=key)
!             end if

!             key = join([solver_settings, nonlinear_solver, convergence, use_criteria])
!             call json%get(key, self%basic%solver_settings%nonlinear_solver%convergence%use_criteria, found)
!             call json%print_error_message(output_unit)
!             if (.not. found) then
!                 call json%destroy()
!                 call error_message(904, c_opt=key)
!             else if (.not. any(valid_criteria_types(:) == &
!                                self%basic%solver_settings%nonlinear_solver%convergence%use_criteria)) then
!                 call json%destroy()
!                 call error_message(905, c_opt=key)
!             end if

!             select case (self%basic%solver_settings%nonlinear_solver%convergence%use_criteria)
!             case (valid_criteria_types(3)) ! absolute
!                 key = join([solver_settings, nonlinear_solver, convergence, logic_between_criteria])
!                 call json%get(key, self%basic%solver_settings%nonlinear_solver%convergence%use_logic, found)
!                 call json%print_error_message(output_unit)
!                 if (.not. found) then
!                     call json%destroy()
!                     call error_message(904, c_opt=key)
!                 else if (.not. any(valid_logic_types(:) == self%basic%solver_settings%nonlinear_solver%convergence%use_logic)) then
!                     call json%destroy()
!                     call error_message(905, c_opt=key)
!                 end if
!             end select

!             select case (self%basic%solver_settings%nonlinear_solver%convergence%use_criteria)
!             case (valid_criteria_types(1)) ! residual
!                 key_base = join([solver_settings, nonlinear_solver, convergence, residual])
!                 call read_parameters_solver_settings_nonlinear_convergence( &
!                     self%basic%solver_settings%nonlinear_solver%convergence%residual, json, key_base)
!             case (valid_criteria_types(2)) ! update
!                 key_base = join([solver_settings, nonlinear_solver, convergence, update])
!                 call read_parameters_solver_settings_nonlinear_convergence( &
!                     self%basic%solver_settings%nonlinear_solver%convergence%update, json, key_base)
!             case (valid_criteria_types(3)) ! residual and update
!                 key_base = join([solver_settings, nonlinear_solver, convergence, residual])
!                 call read_parameters_solver_settings_nonlinear_convergence( &
!                     self%basic%solver_settings%nonlinear_solver%convergence%residual, json, key_base)
!                 key_base = join([solver_settings, nonlinear_solver, convergence, update])
!                 call read_parameters_solver_settings_nonlinear_convergence( &
!                     self%basic%solver_settings%nonlinear_solver%convergence%update, json, key_base)
!             end select
!         end select

!     end subroutine read_parameters_solver_settings_nonlinear

!     subroutine read_parameters_solver_settings_nonlinear_convergence(convergences, json, key_base)
!         implicit none
!         type(type_convergence_criteria) :: convergences
!         type(json_file), intent(inout) :: json
!         character(*), intent(in) :: key_base

!         logical :: found
!         character(:), allocatable :: key

!         key = join([key_base, criteria])
!         call json%get(key, convergences%criteria, found)
!         call json%print_error_message(output_unit)
!         if (.not. found) then
!             call json%destroy()
!             call error_message(904, c_opt=key)
!         else if (.not. any(valid_local_criteria_types(:) == convergences%criteria)) then
!             call json%destroy()
!             call error_message(905, c_opt=key)
!         end if

!         if (convergences%criteria == valid_local_criteria_types(3)) then
!             key = join([key_base, logic])
!             call json%get(key, convergences%logic, found)
!             call json%print_error_message(output_unit)
!             if (.not. found) then
!                 call global_logger%log_warning(message="Using default convergences logic 'and'.")
!                 convergences%logic = "and"
!             else if (.not. any(valid_logic_types(:) == convergences%logic)) then
!                 call json%destroy()
!                 call error_message(905, c_opt=key)
!             end if
!         end if

!         select case (convergences%criteria)
!         case (valid_local_criteria_types(1)) ! absolute

!             key = join([key_base, absolute_tolerance])
!             call json%get(key, convergences%absolute_tolerance, found)
!             call json%print_error_message(output_unit)
!             if (.not. found) then
!                 call global_logger%log_warning(message="Using default absolute convergences of 1.0d-6.")
!                 convergences%absolute_tolerance = 1.0d-6
!             else if (convergences%absolute_tolerance < 0.0d0) then
!                 call json%destroy()
!                 call error_message(905, c_opt=key)
!             end if
!         case (valid_local_criteria_types(2)) ! relative

!             key = join([key_base, relative_tolerance])
!             call json%get(key, convergences%relative_tolerance, found)
!             call json%print_error_message(output_unit)
!             if (.not. found) then
!                 call global_logger%log_warning(message="Using default relative convergences of 1.0d-6.")
!                 convergences%relative_tolerance = 1.0d-6
!             else if (convergences%relative_tolerance < 0.0d0) then
!                 call json%destroy()
!                 call error_message(905, c_opt=key)
!             end if
!         case (valid_local_criteria_types(3)) ! absolute and relative
!             key = join([key_base, absolute_tolerance])
!             call json%get(key, convergences%absolute_tolerance, found)
!             call json%print_error_message(output_unit)
!             if (.not. found) then
!                 call global_logger%log_warning(message="Using default absolute convergences of 1.0d-6.")
!                 convergences%absolute_tolerance = 1.0d-6
!             else if (convergences%absolute_tolerance < 0.0d0) then
!                 call json%destroy()
!                 call error_message(905, c_opt=key)
!             end if
!             key = join([key_base, relative_tolerance])
!             call json%get(key, convergences%relative_tolerance, found)
!             call json%print_error_message(output_unit)
!             if (.not. found) then
!                 call global_logger%log_warning(message="Using default relative convergences of 1.0d-6.")
!                 convergences%relative_tolerance = 1.0d-6
!             else if (convergences%relative_tolerance < 0.0d0) then
!                 call json%destroy()
!                 call error_message(905, c_opt=key)
!             end if
!         end select
!     end subroutine read_parameters_solver_settings_nonlinear_convergence

!     subroutine read_parameters_solver_settings_linear(self, json)
!         implicit none
!         class(type_input) :: self
!         type(json_file), intent(inout) :: json

!         logical :: found
!         character(:), allocatable :: key

!         if (self%basic%analysis_controls%calculate_thermal) then
!             key = join([solver_settings, linear_solver, thermal])
!             call read_parameters_solver_settings_linear_local(self%basic%solver_settings%linear_solver%thermal, json, key)
!         end if

!         if (self%basic%analysis_controls%calculate_hydraulic) then
!             key = join([solver_settings, linear_solver, hydraulic])
!             call read_parameters_solver_settings_linear_local(self%basic%solver_settings%linear_solver%hydraulic, json, key)
!         end if

!         if (self%basic%analysis_controls%calculate_mechanical) then
!             key = join([solver_settings, linear_solver, mechanical])
!             call read_parameters_solver_settings_linear_local(self%basic%solver_settings%linear_solver%mechanical, json, key)
!         end if

!     end subroutine read_parameters_solver_settings_linear

!     subroutine read_parameters_solver_settings_linear_local(solver_setting, json, key_base)
!         implicit none
!         class(type_linear_solver_settings) :: solver_setting
!         type(json_file), intent(inout) :: json
!         character(*), intent(in) :: key_base

!         logical :: found
!         character(:), allocatable :: key

!         key = join([key_base, method])
!         call json%get(key, solver_setting%method, found)
!         call json%print_error_message(output_unit)
!         if (.not. found) then
!             call json%destroy()
!             call error_message(904, c_opt=key)
!         else if (.not. any(valid_linear_solver_methods(:) == solver_setting%method)) then
!             call json%destroy()
!             call error_message(905, c_opt=key)
!         end if

!         select case (solver_setting%method)
!         case (valid_linear_solver_methods(2))
!             key = join([key_base, iterative_solver, solver_type])
!             call json%get(key, solver_setting%iterative_solver%solver_type, found)
!             call json%print_error_message(output_unit)
!             if (.not. found) then
!                 call json%destroy()
!                 call error_message(904, c_opt=key)
!             end if

!             key = join([key_base, iterative_solver, preconditioner_type])
!             call json%get(key, solver_setting%iterative_solver%preconditioner_type, found)
!             call json%print_error_message(output_unit)
!             if (.not. found) then
!                 call json%destroy()
!                 call error_message(904, c_opt=key)
!             end if

!             key = join([key_base, iterative_solver, max_iterations])
!             call json%get(key, solver_setting%iterative_solver%max_iterations, found)
!             call json%print_error_message(output_unit)
!             if (.not. found) then
!                 call global_logger%log_warning(message="Using default maximum iterations of 10000 for iterative solver.")
!                 solver_setting%iterative_solver%max_iterations = 10000
!             else if (solver_setting%iterative_solver%max_iterations <= 0) then
!                 call json%destroy()
!                 call error_message(905, c_opt=key)
!             end if

!             key = join([key_base, iterative_solver, tolerance])
!             call json%get(key, solver_setting%iterative_solver%tolerance, found)
!             call json%print_error_message(output_unit)
!             if (.not. found) then
!                 call global_logger%log_warning(message="Using default tolerance of 1.0d-6 for iterative solver.")
!                 solver_setting%iterative_solver%tolerance = 1.0d-6
!             else if (solver_setting%iterative_solver%tolerance < 0.0d0) then
!                 call json%destroy()
!                 call error_message(905, c_opt=key)
!             end if

!         end select

!     end subroutine read_parameters_solver_settings_linear_local

!     subroutine read_parameters_solver_parallel_settings(self, json)
!         implicit none
!         class(type_input) :: self
!         type(json_file), intent(inout) :: json

!         logical :: found
!         character(:), allocatable :: key

!         key = join([solver_settings, parallel_settings, threads, is_parallel])
!         call json%get(key, self%basic%solver_settings%parallel_settings%threads%is_parallel, found)
!         call json%print_error_message(output_unit)
!         if (.not. found) then
!             call json%destroy()
!             call error_message(904, c_opt=key)
!         end if

!         if (self%basic%solver_settings%parallel_settings%threads%is_parallel) then
!             key = join([solver_settings, parallel_settings, threads, num_threads])
!             call json%get(key, self%basic%solver_settings%parallel_settings%threads%num_threads, found)
!             call json%print_error_message(output_unit)
!             if (.not. found) then
!                 call json%destroy()
!                 call error_message(904, c_opt=key)
!             else if (self%basic%solver_settings%parallel_settings%threads%num_threads <= 0) then
!                 call json%destroy()
!                 call error_message(905, c_opt=key)
!             end if
! !$          if (self%basic%solver_settings%parallel_settings%threads%num_threads > omp_get_max_threads()) then
! !$              call global_logger%log_warning(message="Number of threads exceeds available threads. Using maximum available threads.")
! !$              self%basic%solver_settings%parallel_settings%threads%num_threads = omp_get_max_threads()
! !$          end if

!             key = join([solver_settings, parallel_settings, threads, schedule])
!             call json%get(key, self%basic%solver_settings%parallel_settings%threads%schedule, found)
!             call json%print_error_message(output_unit)
!             if (.not. found) then
!                 call global_logger%log_warning(message="Default schedule is set to 'static'.")
!                 self%basic%solver_settings%parallel_settings%threads%schedule = "static"
!             else if (.not. any(valid_schedule_types(:) == self%basic%solver_settings%parallel_settings%threads%schedule)) then
!                 call json%destroy()
!                 call error_message(905, c_opt=key)
!             end if

!             key = join([solver_settings, parallel_settings, threads, max_active_levels])
!             call json%get(key, self%basic%solver_settings%parallel_settings%threads%max_active_levels, found)
!             call json%print_error_message(output_unit)
!             if (.not. found) then
!                 call global_logger%log_warning(message="Default maximum active levels is set to 1.")
!                 self%basic%solver_settings%parallel_settings%threads%max_active_levels = 1
!             else if (self%basic%solver_settings%parallel_settings%threads%max_active_levels < 0) then
!                 call json%destroy()
!                 call error_message(905, c_opt=key)
!             end if
!         end if

!     end subroutine read_parameters_solver_parallel_settings

! end submodule inout_input_basic
