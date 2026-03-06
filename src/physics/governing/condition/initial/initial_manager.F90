! =============================================================================
! module conditions_initial_manager
! Purpose: Manages all initial condition objects.
! =============================================================================
module condition_initial_manager
    use, intrinsic :: iso_fortran_env
    use :: module_core
    use :: module_domain, only:type_domain
    use :: module_input, only:type_input
    use :: condition_initial, only:abst_ic, type_ic_uniform, holder_ics
    implicit none
    private

    public :: type_ic_manager

    type :: type_ic_manager
        type(holder_ics), private :: ic(IC_TARGETS%NUM_ID)
    contains
        procedure, public, pass(self) :: initialize => initialize_type_ic_manager
        procedure, public, pass(self) :: apply => apply_ic_manager
    end type type_ic_manager

contains

    ! Sets up the manager by creating the correct IC objects based on input.
    subroutine initialize_type_ic_manager(self, configs)
        implicit none
        class(type_ic_manager), intent(inout) :: self
        type(type_config_ic), intent(in) :: configs(:)
        ! type(type_input), intent(in) :: input

        integer(int32) :: i, ic_method
        integer(int32) :: target_ic_id
        character(:), allocatable :: target_str

        do i = 1, IC_TARGETS%NUM_ID
            ! Skip if analysis is not active (except Porosity which might always be needed)
            ! if (.not. input%basic%analysis_controls%is_active(i) .and. i /= IC_TARGETS%POROSITY%ID) cycle

            ! ic_method = input%conditions%initial_conditions%physics(i)%type

            select case (configs(i)%ic_kind%ID)
            case (IC_METHODS%UNIFORM%ID)
                allocate (type_ic_uniform :: self%ic(i)%p)
                ! case (IC_METHOD_LAPLACE)
                !     allocate (type_ic_laplace :: self%ic(i)%p)
            case default
                ! Future: Handle IC_METHOD_FROM_FILE or others
            end select

            if (allocated(self%ic(i)%p)) then
                call self%ic(i)%p%initialize(configs(i))
            end if
        end do

    end subroutine initialize_type_ic_manager

    subroutine apply_ic_manager(self, initial_target_id, variable)
        implicit none
        class(type_ic_manager), intent(in) :: self
        type(type_constant_id), intent(in) :: initial_target_id
        type(type_variable), intent(inout) :: variable

        integer(int32) :: id

        if (.not. IC_TARGETS%is_valid(initial_target_id)) then
            return
        end if

        call self%ic(initial_target_id%ID)%p%apply(variable)

    end subroutine apply_ic_manager

end module condition_initial_manager
