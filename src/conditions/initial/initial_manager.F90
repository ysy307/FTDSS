! =============================================================================
! module conditions_initial_manager
! Purpose: Manages all initial condition objects.
! =============================================================================
module conditions_initial_manager
    use, intrinsic :: iso_fortran_env
    use :: module_core
    use :: module_domain, only:type_domain
    use :: module_input, only:type_input
    use :: conditions_initial, only:abst_ic, type_ic_uniform, holder_ics
    implicit none
    private

    public :: type_ic_manager

    type :: type_ic_manager
        type(holder_ics) :: list(NUM_IC_TARGETS)
    contains
        procedure :: initialize => initialize_type_ic_manager
        procedure :: apply
    end type type_ic_manager

contains

    ! Sets up the manager by creating the correct IC objects based on input.
    subroutine initialize_type_ic_manager(self, input)
        implicit none
        class(type_ic_manager), intent(inout) :: self
        type(type_input), intent(in) :: input

        integer(int32) :: i, ic_method
        integer(int32) :: target_ic_id
        character(:), allocatable :: target_str

        do i = 1, NUM_IC_TARGETS
            ! Skip if analysis is not active (except Porosity which might always be needed)
            if (.not. input%basic%analysis_controls%is_active(i) .and. i /= IC_TARGET_POROSITY) cycle

            ic_method = input%conditions%initial_conditions%physics(i)%type

            select case (ic_method)
            case (IC_METHOD_UNIFORM)
                allocate (type_ic_uniform :: self%list(i)%ic)
                ! case (IC_METHOD_LAPLACE)
                !     allocate (type_ic_laplace :: self%list(i)%ic)
            case default
                ! Future: Handle IC_METHOD_FROM_FILE or others
            end select

            if (allocated(self%list(i)%ic)) then
                call self%list(i)%ic%initialize(input, i)
            end if
        end do

    end subroutine initialize_type_ic_manager

    subroutine apply(self, initial_target_id, domain, variable)
        implicit none
        class(type_ic_manager), intent(in) :: self
        integer(int32), intent(in) :: initial_target_id
        type(type_domain), intent(in) :: domain
        type(type_variable), intent(inout) :: variable

        integer(int32) :: id

        if (initial_target_id > 0 .and. initial_target_id <= NUM_IC_TARGETS) then
            if (allocated(self%list(initial_target_id)%ic)) then
                call self%list(initial_target_id)%ic%apply(domain, variable)
            end if
        end if
    end subroutine apply

end module conditions_initial_manager
