! =============================================================================
! submodule (condition_initial) condition_initial_Uniform
! Purpose: Provides the concrete implementations for the procedures
!          defined in the condition_initial module.
! =============================================================================
submodule(condition_initial) condition_initial_uniform
    implicit none

contains

    module subroutine initialize_ic(self, config)
        implicit none
        class(type_ic_uniform), intent(inout) :: self
        type(type_config_ic), intent(in) :: config

        self%physics_type = config%physics_type
        self%ic_kind = config%ic_kind
        self%value = config%value

        self%initialized = .true.
    end subroutine initialize_ic

    module subroutine apply_ic_uniform(self, variable)
        implicit none
        class(type_ic_uniform), intent(in) :: self
        type(type_variable), intent(inout) :: variable

        if (.not. self%initialized) then
            error stop "Error: IC not initialized."
        end if

        call variable%set_current(self%value)
        call variable%set_previous(self%value)
    end subroutine apply_ic_uniform

end submodule condition_initial_uniform
