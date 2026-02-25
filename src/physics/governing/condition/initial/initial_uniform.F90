! =============================================================================
! submodule (conditions_initial) conditions_initial_Uniform
! Purpose: Provides the concrete implementations for the procedures
!          defined in the conditions_initial module.
! =============================================================================
submodule(conditions_initial) conditions_initial_uniform
    implicit none

contains

    module subroutine apply_ic_uniform(self, variable)
        implicit none
        class(type_ic_uniform), intent(in) :: self
        type(type_variable), intent(inout) :: variable

        if (.not. self%initialized) then
            error stop "Error: IC not initialized."
        end if

        call variable%set_current(self%config%value)
        call variable%set_previous(self%config%value)
    end subroutine apply_ic_uniform

end submodule conditions_initial_uniform
