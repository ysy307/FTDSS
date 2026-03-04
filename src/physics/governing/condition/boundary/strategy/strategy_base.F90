submodule(condition_boundary_strategy) strategy_base
    implicit none
contains

    module subroutine initialize_bc(self, config_bc)
        implicit none
        class(abst_bc), intent(inout) :: self
        type(type_config_bc), intent(in) :: config_bc

        self%physics_type = config_bc%physics_type
        self%bc_kind = config_bc%bc_kind

        ! Initialization logic for data_provider will be implemented by Manager
        self%is_initialized = .true.
    end subroutine initialize_bc

    module subroutine destroy_bc(self)
        implicit none
        class(abst_bc), intent(inout) :: self

        self%physics_type = type_constant_id("", "", -1)
        self%bc_kind = type_constant_id("", "", -1)

        if (allocated(self%data_provider)) then
            call self%data_provider%destroy()
            deallocate (self%data_provider)
        end if

        self%is_initialized = .false.
    end subroutine destroy_bc

    module subroutine set_bc_kind_abst_bc(self, bc_kind)
        implicit none
        class(abst_bc), intent(inout) :: self
        type(type_constant_id), intent(in) :: bc_kind

        if (.not. THERMAL_BC_TYPES%is_valid(bc_kind) .and. &
            .not. HYDRAULIC_BC_TYPES%is_valid(bc_kind)) then
            error stop "Invalid BC kind: "//trim(bc_kind%name) ! TODO: call raise_error with appropriate error codes
            ! call raise_error(ERROR_CODES%INVALILD_TYPES "Invalid BC kind: "//trim(bc_kind%name))
        end if

        self%bc_kind = bc_kind
    end subroutine set_bc_kind_abst_bc

    module subroutine get_bc_kind_abst_bc(self, bc_kind)
        implicit none
        class(abst_bc), intent(in), target :: self
        type(type_constant_id), intent(inout), pointer :: bc_kind

        bc_kind => self%bc_kind
    end subroutine get_bc_kind_abst_bc

end submodule strategy_base
