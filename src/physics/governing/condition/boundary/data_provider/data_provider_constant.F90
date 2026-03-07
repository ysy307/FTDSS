submodule(condition_boundary_data_provider) data_provider_constant
    implicit none
contains

    module subroutine initialize_type_bc_data_constant(self, config)
        implicit none
        class(type_bc_data_constant), intent(inout) :: self
        type(type_config_bc), intent(in) :: config
        ! User implements initialization

        call allocate_array(self%constant_values, config%num_variables)
        self%constant_values = config%values(:, 1) ! Assuming the first time

        self%data_kind = BC_DATA_PROVIDERS%CONSTANT
    end subroutine initialize_type_bc_data_constant

    module subroutine destroy_type_bc_data_constant(self)
        implicit none
        class(type_bc_data_constant), intent(inout) :: self

        call deallocate_array(self%constant_values)

        self%data_kind = type_constant_id("", "", -1)
    end subroutine destroy_type_bc_data_constant

    module subroutine get_data_bc_data_constant(self, current_time, values)
        implicit none
        class(type_bc_data_constant), intent(in) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(inout), allocatable :: values(:)

        if (allocated(values)) deallocate (values)
        if (allocated(self%constant_values)) then
            allocate (values(size(self%constant_values)))
            values = self%constant_values
        end if
    end subroutine get_data_bc_data_constant

end submodule data_provider_constant
