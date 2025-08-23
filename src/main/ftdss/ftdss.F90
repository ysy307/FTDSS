module Main_FTDSS
    use, intrinsic :: iso_fortran_env
    use :: stdlib_logger
    use :: module_core
    use :: module_input, only:type_input
    use :: module_control, only:type_controls
    use :: module_output, only:type_output
    use :: module_domain, only:type_domain
    use :: module_properties, only:type_properties_manager
    use :: module_boundary, only:type_bc
    use :: module_initial, only:type_ic

    use :: module_thermal
    use :: module_hydraulic
    implicit none

    type :: type_ftdss
        type(type_dp_3d), pointer :: coordinate
        type(type_domain) :: domain

        type(type_variable) :: phi
        type(type_variable) :: T
        class(abst_thermal), allocatable :: thermal
        type(type_variable) :: P
        class(abst_hydraulic), allocatable :: hydraulic

        type(type_properties_manager) :: property
        type(type_bc) :: bc
        type(type_ic) :: ic

        type(type_controls) :: controls
        type(type_output) :: output

    contains
        procedure, pass(self) :: initialize => initialize_type_ftdss
        procedure, pass(self) :: shift => shift_type_ftdss
    end type type_ftdss

contains
    subroutine initialize_type_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self

        type(type_input) :: input
        integer(int32) :: nsize
        integer(int32) :: ierr
        character(len=10), allocatable :: profiler_labels(:)

        profiler_labels = [character(len=10) :: "IO", "Setup", "Assemble", "Solve", "Total"]
        call self%controls%time%initialize(profiler_sections=profiler_labels)
        call self%controls%time%Record("Start")
        call self%controls%time%Profile_Start("Total")
        call self%controls%time%Profile_Start("IO")

        call setup_handler()

        call input%initialize()
        call self%controls%time%initialize(input=input)
        call self%controls%iteration%initialize(input)
        call initialize_openmp(input)

        if (input%output_settings%standard_output%print_progress) then
            call global_logger%configure(level=information_level, &
                                         time_stamp=.true., &
                                         max_width=0)
        else
            call global_logger%configure(level=warning_level, &
                                         time_stamp=.false., &
                                         max_width=0)
        end if

        !---------------------------------------------------------------------------------------------------------------------------
        !
        !---------------------------------------------------------------------------------------------------------------------------
        nsize = input%geometry%vtk%num_points

        ! Initialize the Structure
        allocate (self%coordinate)
        call self%coordinate%initialize(nsize)
        self%coordinate = input%geometry%vtk%POINTS

        call self%domain%initialize(input, self%coordinate, ierr)

        call self%bc%initialize(input, self%domain)
        call self%ic%initialize(input)

        call global_logger%log_information(message="Boundary and Initial Conditions set up.")

        self%thermal = type_thermal_crs(input, self%coordinate, self%domain)

        call self%property%initialize(input, ierr)

        call self%output%initialize(input, self%domain, self%coordinate)

        call self%phi%initialize(nsize, input%basic%solver_settings%bdf_order)
        call self%T%initialize(nsize, input%basic%solver_settings%bdf_order)
        call self%ic%apply("porosity", self%domain, self%phi)

        call self%output%output_coloring(self%domain)

        call self%controls%time%Profile_Stop("IO")
        call global_logger%log_information(message="FTDSS module initialized successfully.")
    end subroutine initialize_type_ftdss

    subroutine shift_type_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self

        call self%phi%shift()
        if (self%controls%calculate_thermal) then
            call self%T%shift()
            call self%thermal%shift()
        end if

    end subroutine shift_type_ftdss

end module Main_FTDSS
