submodule(condition_initial) condition_initial_from_file
    implicit none
contains

    module subroutine initialize_ic_from_file(self, config)
        implicit none
        class(type_ic_from_file), intent(inout) :: self
        type(type_config_ic), intent(in) :: config

        self%physics_type = config%physics_type
        self%ic_kind = config%ic_kind
        if (allocated(config%values)) allocate (self%values, source=config%values)
        self%initialized = .true.
    end subroutine initialize_ic_from_file

    module subroutine apply_ic_from_file(self, variable)
        implicit none
        class(type_ic_from_file), intent(in) :: self
        type(type_variable), intent(inout) :: variable

        if (.not. self%initialized) error stop "Error: IC not initialized."
        if (.not. allocated(self%values)) error stop "Error: IC file values not loaded."

        call variable%set_current(self%values)
        call variable%set_previous(self%values)
    end subroutine apply_ic_from_file

end submodule condition_initial_from_file
