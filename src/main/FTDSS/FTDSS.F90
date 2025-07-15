module Main_FTDSS
    use, intrinsic :: iso_fortran_env
    use :: stdlib_logger
    use :: module_core
    use :: Inout_Input
    use :: module_control, only:type_time, type_iteration
    use :: Inout_Output
    use :: module_domain, only:type_domain
    use :: module_properties, only:type_proereties_manager
    use :: Conditions_Boundary_Manager, only:BCManager
    use :: Conditions_Initial_Manager, only:ICManager

    use :: Main_Thermal
    implicit none

    type :: Type_FTDSS
        type(Type_Input) :: Input

        type(type_dp_3d), pointer :: Coordinate
        type(type_domain) :: Domain
        ! type(Belonging), allocatable :: NodeBelonging(:)
        class(Abstract_Thermal), allocatable :: Thermal

        type(type_proereties_manager) :: Property
        type(BCManager) :: BC
        type(ICManager) :: IC

        type(type_variable) :: phi

        type(type_time) :: time
        type(type_iteration) :: Iteration
        type(Type_Output) :: Output

    contains
        procedure, pass(self) :: initialize => FDTSS_initialize
    end type Type_FTDSS

contains
    subroutine FDTSS_initialize(self)
        implicit none
        class(Type_FTDSS), intent(inout) :: self

        integer(int32) :: nsize
        integer(int32) :: iN

        integer(int32) :: ierr

        character(len=10), allocatable :: profiler_labels(:)

        ! ★ 計測したいセクション名を定義
        profiler_labels = [character(len=10) :: "IO", "Setup", "Assemble", "Solve", "Total"]

        ! Initialize the FDTSS module
        ! This is where you would set up any necessary parameters or configurations
        call self%Input%initialize()
        self%time = type_time(self%Input, profiler_labels)
        call self%time%Record("Start")
        call self%time%Profile_Start("Total")
        call self%time%Profile_Start("IO")

        call global_logger%configure(level=information_level, &
                                     time_stamp=.true., &
                                     max_width=0)
        ! call setup_handler()

        !---------------------------------------------------------------------------------------------------------------------------
        !
        !---------------------------------------------------------------------------------------------------------------------------
        nsize = self%Input%VTK%num_points

        ! Initialize the Structure
        allocate (self%Coordinate)
        call self%Coordinate%initialize(nsize)
        self%Coordinate = self%Input%VTK%POINTS

        call self%Domain%initialize(self%Input, self%Coordinate, ierr)
        if (ierr /= 0) then
            print *, "Error initializing domain in Type_Thermal_3Phase_2D_Construct"
            return
        end if

        call self%BC%setup(self%Input, self%Domain)
        call self%IC%setup(self%Input)

        call global_logger%log_information(message="Boundary and Initial Conditions set up.")

        self%Thermal = Type_Thermal_3Phase_2D(self%Input, self%Coordinate, self%Domain)

        call self%Property%initialize(self%Input, ierr)

        self%Output = Type_Output(Input=self%Input, Domain=self%Domain, Coordinate=self%Coordinate)

        call self%phi%initialize(nsize, self%Input%Basic%Order)
        self%phi%pre = self%Input%Regions(1)%Thermal%Porosity
        self%phi%old = self%Input%Regions(1)%Thermal%Porosity

        call self%Output%Overall%Output_vtu(fc=0, &
                                            Colors=self%Domain%Colors%Color)

        call self%time%Profile_Stop("IO")
        call global_logger%log_information(message="FTDSS module initialized successfully.")
    end subroutine FDTSS_initialize

end module Main_FTDSS
