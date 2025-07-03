module Main_FTDSS
    use, intrinsic :: iso_fortran_env

    use :: Core_BaseTypes
    use :: Inout_Input
    use :: Time_Time
    use :: Inout_Output
    use :: Domain_Module, only:Domain_t
    use :: Properties_Model_Base, only:Proereties_Model_t
    use :: Conditions_Boundary_Manager, only:BCManager
    use :: Conditions_Initial_Manager, only:ICManager

    use :: Main_Thermal
    implicit none

    type :: Type_FTDSS
        type(Type_Input) :: Input

        type(DP3d), pointer :: Coordinate
        type(Domain_t) :: Domain
        ! type(Belonging), allocatable :: NodeBelonging(:)
        class(Abstract_Thermal), allocatable :: Thermal

        type(Proereties_Model_t) :: Property
        type(BCManager) :: BC
        type(ICManager) :: IC

        type(Variables) :: phi

        type(Type_Time) :: time
        type(Type_Iteration) :: Iteration
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

        ! Initialize the FDTSS module
        ! This is where you would set up any necessary parameters or configurations
        self%Input = Type_Input()
        self%time = Type_Time(self%Input)
        call self%time%Record("Start")
        nsize = self%Input%VTK%numPoints

        ! Initialize the Structure
        allocate (self%Coordinate)
        call self%Coordinate%allocate(nsize)
        self%Coordinate = self%Input%VTK%POINTS

        call self%Domain%initialize(self%Input, self%Coordinate, ierr)
        if (ierr /= 0) then
            print *, "Error initializing domain in Type_Thermal_3Phase_2D_Construct"
            return
        end if

        call self%BC%setup(self%Input, self%Domain)
        call self%IC%setup(self%Input)

        ! allocate (self%NodeBelonging(nsize))
        ! do iN = 1, nsize
        !     ! The details are to be implemented
        !     call self%NodeBelonging(iN)%allocate(1_int32)
        !     self%NodeBelonging(iN)%group(1) = 1
        !     self%NodeBelonging(iN)%nsize = 1
        ! end do

        self%Thermal = Type_Thermal_3Phase_2D(self%Input, self%Coordinate, self%Domain)

        call self%Property%Materials%initialize(self%Input, ierr)

        self%Output = Type_Output(Input=self%Input, Domain=self%Domain, Coordinate=self%Coordinate)

        call self%phi%allocate(nsize, self%Input%Basic%Order)
        self%phi%pre = self%Input%Regions(1)%Thermal%Porosity
        self%phi%old = self%Input%Regions(1)%Thermal%Porosity

    end subroutine FDTSS_initialize

end module Main_FTDSS
