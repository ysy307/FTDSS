! =============================================================================
! submodule (conditions_initial) conditions_initial_Uniform
! Purpose: Provides the concrete implementations for the procedures
!          defined in the conditions_initial module.
! =============================================================================
submodule(conditions_initial) conditions_initial_uniform
    ! use :: module_core
    implicit none

contains

    module subroutine initialize_type_ic_uniform(self, input, initial_target_id)
        implicit none
        class(type_ic_uniform), intent(inout) :: self
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: initial_target_id

        if (initial_target_id > 0) then
            self%target_id = initial_target_id
            self%type = IC_METHOD_UNIFORM
            self%value = input%conditions%initial_conditions%physics(initial_target_id)%value
        end if
    end subroutine initialize_type_ic_uniform

    module subroutine apply_uniform(self, variable)
        implicit none
        class(type_ic_uniform), intent(in) :: self
        type(type_variable), intent(inout) :: variable

        variable%new(:) = self%value
        variable%pre(:) = self%value
    end subroutine

end submodule conditions_initial_uniform
