! =============================================================================
! submodule (conditions_initial) conditions_initial_Uniform
! Purpose: Provides the concrete implementations for the procedures
!          defined in the conditions_initial module.
! =============================================================================
submodule(conditions_initial) conditions_initial_uniform
    implicit none

contains

    module subroutine initialize_type_ic_uniform(self, input, initial_target)
        implicit none
        class(type_ic_uniform), intent(inout) :: self
        type(type_input), intent(in) :: input
        character(*), intent(in) :: initial_target

        select case (trim(adjustl(initial_target)))
        case ('thermal')
            self%type = input%conditions%initial_conditions%thermal%type
            self%value = input%conditions%initial_conditions%thermal%value
        case ('hydraulic')
            self%type = input%conditions%initial_conditions%hydraulic%type
            self%value = input%conditions%initial_conditions%hydraulic%value
        end select
    end subroutine initialize_type_ic_uniform

    module subroutine apply_uniform(self, domain, variable)
        implicit none
        class(type_ic_uniform), intent(in) :: self
        type(type_domain), intent(in) :: domain
        type(type_variable), intent(inout) :: variable

        variable%new(:) = self%value
        variable%pre(:) = self%value
    end subroutine

end submodule conditions_initial_Uniform
