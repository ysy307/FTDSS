! =============================================================================
! submodule (conditions_initial) conditions_initial_laplace
! Purpose: Provides the concrete implementations for the procedures
!          defined in the conditions_initial module.
! =============================================================================
submodule(conditions_initial) conditions_initial_laplace
    implicit none

contains

    module subroutine initialize_type_ic_laplace(self, input, initial_target)
        implicit none
        class(type_ic_laplace), intent(inout) :: self
        type(type_input), intent(in) :: input
        character(*), intent(in) :: initial_target

        select case (trim(adjustl(initial_target)))
        case ('thermal')
            self%type = input%conditions%initial_conditions%thermal%type
        case ('hydraulic')
            self%type = input%conditions%initial_conditions%hydraulic%type
        end select
    end subroutine initialize_type_ic_laplace

    module subroutine apply_laplace(self, domain, variable)
        implicit none
        class(type_ic_laplace), intent(in) :: self
        type(type_domain), intent(in) :: domain
        type(type_variable), intent(inout) :: variable

        ! Your implementation for applying type_ic_uniform
        ! var%new(:) = self%value
        ! var%pre(:) = var%new(:)
    end subroutine apply_laplace

end submodule conditions_initial_laplace
