! =============================================================================
! module Conditions_Initial_Manager
! Purpose: Manages all initial condition objects.
! =============================================================================
module Conditions_Initial_Manager
    use :: Condition_Initial
    use :: Inout_Input, only:Type_Input
    use :: Core_BaseTypes, only:Variables
    use :: Domain_Module, only:Domain_t
    implicit none
    private
    public :: ICManager

    type :: ICManager
        ! One allocatable holder for each physics domain
        class(Abstract_IC), allocatable :: T ! Thermal
        class(Abstract_IC), allocatable :: H ! Hydraulic
        ! ... add M for Mechanical, etc.
    contains
        procedure :: setup
        procedure :: apply
    end type

contains

    !
    ! Sets up the manager by creating the correct IC objects based on input.
    !
    subroutine setup(self, Input)
        class(ICManager), intent(inout) :: self
        type(Type_Input), intent(in) :: Input
        character(len=32) :: ic_type

        ! --- Setup Thermal Initial Condition ---
        ! if (Input%IC%has_Heat) then
        ic_type = Input%IC%Heat%type
        print *, "Setting up Thermal IC of type: ", trim(adjustl(ic_type))
        select case (trim(adjustl(ic_type)))
        case ("Constant")
            allocate (IC_Uniform :: self%T)
        case ("Laplace")
            allocate (IC_Laplace :: self%T)
            ! case ("File")
            !     allocate (IC_File :: self%T)
        case default
            ! Error handling
        end select
        ! Call the newly created object's own setup method
        call self%T%setup(Input, 'T')
        ! end if

        ! --- Setup Hydraulic Initial Condition (for the future) ---
        ! if (Input%IC%has_Hydraulic) then
        !    ... similar logic for self%H ...
        ! end if

    end subroutine setup

    !
    ! Applies the initial condition for a specific physics.
    !
    subroutine apply(self, physics, domain, var)
        class(ICManager), intent(in) :: self
        character(len=*), intent(in) :: physics
        type(Domain_t), intent(in) :: domain
        type(Variables), intent(inout) :: var

        select case (trim(adjustl(physics)))
        case ("Thermal")
            if (allocated(self%T)) then
                call self%T%apply(domain, var)
            end if
        case ("Hydraulic")
            if (allocated(self%H)) then
                call self%H%apply(domain, var)
            end if
        case default
            ! Error or no action
        end select
    end subroutine apply

end module Conditions_Initial_Manager
