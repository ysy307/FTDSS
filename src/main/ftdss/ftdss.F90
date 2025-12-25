module module_ftdss
    use, intrinsic :: iso_fortran_env
    use :: mpi_f08
    use :: stdlib_logger
    use :: module_core
    use :: module_input, only:type_input
    use :: module_control, only:type_controls
    ! use :: module_output, only:type_output
    use :: module_domain, only:type_domain
    ! use :: module_properties, only:type_properties_manager
    ! use :: module_boundary, only:type_bc
    use :: module_initial, only:type_ic_manager
    use :: module_field, only:type_jacobian_matrix, type_residual_vector

    use :: module_thermal, only:type_thermal
    use :: module_hydraulic, only:type_hydraulic
    implicit none

    type :: type_ftdss
        type(type_domain) :: domain

        type(type_variable) :: porosity
        type(type_variable) :: temperature
        type(type_variable) :: pressure

        type(type_variable) :: Qw
        type(type_variable) :: Qi
        type(type_variable) :: Qa
        type(type_variable) :: Qv

        type(type_jacobian_matrix) :: J
        type(type_residual_vector) :: R

        type(type_thermal) :: thermal
        type(type_hydraulic) :: hydraulic

        ! class(abst_thermal), allocatable :: thermal
        ! class(abst_hydraulic), allocatable :: hydraulic

        ! type(type_properties_manager) :: property
        ! type(type_bc) :: bc

        type(type_controls) :: controls
        ! type(type_output) :: output

    contains
        procedure, pass(self) :: initialize => initialize_type_ftdss
        procedure, pass(self) :: shift => shift_type_ftdss
    end type type_ftdss

contains
    subroutine initialize_type_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self

        type(type_input) :: input
        type(type_ic_manager) :: ic

        integer(int32) :: max_bdf_order
        integer(int32), allocatable :: active_region_ids(:)
        integer(int32) :: ierr
        integer(int32) :: num_nodes
        character(len=10), allocatable :: profiler_labels(:)

        profiler_labels = [character(len=10) :: "IO", "Setup", "Assemble", "Solve", "Total"]
        call self%controls%profiler%initialize(profiler_labels)
        call self%controls%profiler%record(TIME_RECORD_START)
        call self%controls%profiler%start("Total")
        call self%controls%profiler%start("IO")

        call setup_handler()

        call input%initialize()
        call self%controls%initialize(input)
        call ic%initialize(input)

        if (input%output_settings%standard_output%print_progress) then
            call global_logger%configure(level=information_level, &
                                         time_stamp=.true., &
                                         max_width=0)
        else
            call global_logger%configure(level=warning_level, &
                                         time_stamp=.true., &
                                         max_width=0)
        end if

        !---------------------------------------------------------------------------------------------------------------------------
        !
        !---------------------------------------------------------------------------------------------------------------------------
        num_nodes = input%geometry%vtk%num_points
        call self%domain%initialize(input, self%controls)

        max_bdf_order = input%basic%solver_settings%bdf_order
        call self%porosity%initialize(num_nodes, max_bdf_order)
        call ic%apply(IC_TARGET_POROSITY, self%domain, self%porosity)

        if (self%controls%is_physics_active(PHYSICS_TYPE_THERMAL)) then
            call self%temperature%initialize(num_nodes, max_bdf_order)
            call ic%apply(IC_TARGET_THERMAL, self%domain, self%temperature)
        end if

        if (self%controls%is_physics_active(PHYSICS_TYPE_HYDRAULIC)) then
            call self%pressure%initialize(num_nodes, max_bdf_order)
            call ic%apply(IC_TARGET_HYDRAULIC, self%domain, self%pressure)
        end if

        call self%Qw%initialize(num_nodes, max_bdf_order)
        call self%Qi%initialize(num_nodes, max_bdf_order)
        call self%Qa%initialize(num_nodes, max_bdf_order)
        call self%Qv%initialize(num_nodes, max_bdf_order)

        call input%geometry%vtk%get_active_region_info(active_region_ids, target_dim=self%domain%get_computation_dimension())

        call self%thermal%initialize(input, active_region_ids)
        call self%hydraulic%initialize(input, active_region_ids)

        ! self%thermal = type_thermal_crs(input, self%coordinate, self%domain)

        ! call self%property%initialize(input, ierr)

        ! call self%output%initialize(input, self%domain, self%coordinate)

        ! call self%output%output_coloring(self%domain)

        call self%controls%profiler%stop("IO")
        call global_logger%log_information(message="FTDSS module initialized successfully.")
    end subroutine initialize_type_ftdss

    subroutine shift_type_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self

        ! call self%phi%shift()
        ! if (self%controls%calculate_thermal) then
        !     call self%T%shift()
        !     call self%thermal%shift()
        ! end if

    end subroutine shift_type_ftdss

end module module_ftdss
