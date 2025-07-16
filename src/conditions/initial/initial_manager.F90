! =============================================================================
! module conditions_initial_manager
! Purpose: Manages all initial condition objects.
! =============================================================================
module conditions_initial_manager
    use :: module_core, only:type_variable
    use :: module_domain, only:type_domain
    use :: module_input, only:type_input
    use :: conditions_initial, only:abst_ic, type_ic_uniform, type_ic_laplace
    implicit none
    private

    public :: type_ic

    type :: type_ic
        class(abst_ic), allocatable :: t
        class(abst_ic), allocatable :: h
    contains
        procedure :: initialize => initialize_type_ic
        procedure :: apply
    end type type_ic

contains

    !
    ! Sets up the manager by creating the correct IC objects based on input.
    !
    subroutine initialize_type_ic(self, input)
        implicit none
        class(type_ic), intent(inout) :: self
        type(type_input), intent(in) :: input

        if (input%basic%analysis_controls%calculate_thermal) then
            select case (trim(adjustl(input%conditions%initial_conditions%thermal%type)))
            case ("uniform")
                allocate (type_ic_uniform :: self%t)
            case ("laplace")
                allocate (type_ic_laplace :: self%t)
            case ("file")
                !     allocate (IC_File :: self%T)
            end select
            call self%t%initialize(input, 'thermal')
        end if

        if (input%basic%analysis_controls%calculate_hydraulic) then
            select case (trim(adjustl(input%conditions%initial_conditions%hydraulic%type)))
            case ("uniform")
                allocate (type_ic_uniform :: self%h)
            case ("laplace")
                allocate (type_ic_laplace :: self%h)
            case ("file")
                !     allocate (IC_File :: self%T)
            end select
            call self%t%initialize(input, 'hydraulic')
        end if

    end subroutine initialize_type_ic

    subroutine apply(self, initial_target, domain, variable)
        class(type_ic), intent(in) :: self
        character(*), intent(in) :: initial_target
        type(type_domain), intent(in) :: domain
        type(type_variable), intent(inout) :: variable

        select case (trim(adjustl(initial_target)))
        case ("thermal")
            if (allocated(self%T)) then
                call self%t%apply(domain, variable)
            end if
        case ("hydraulic")
            if (allocated(self%H)) then
                call self%h%apply(domain, variable)
            end if
        case default
            ! Error or no action
        end select
    end subroutine apply

end module conditions_initial_manager
