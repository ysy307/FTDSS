submodule(condition_boundary_data_provider) data_provider_constant
    implicit none
contains

    module subroutine initialize_type_bc_data_constant(self, config_bc)
        implicit none
        class(type_bc_data_constant), intent(inout) :: self
        type(type_config_bc), intent(in) :: config_bc

        if (allocated(self%constant_values)) deallocate (self%constant_values)
        ! allocate (self%constant_values(size(config_bc%values)))
        ! self%constant_values = config_bc%values
    end subroutine initialize_type_bc_data_constant

    module subroutine destroy_type_bc_data_constant(self)
        implicit none
        class(type_bc_data_constant), intent(inout) :: self

        if (allocated(self%constant_values)) deallocate (self%constant_values)
    end subroutine destroy_type_bc_data_constant

    module subroutine get_data_bc_data_constant(self, current_time, output_value)
        implicit none
        class(type_bc_data_constant), intent(inout) :: self
        real(real64), intent(in) :: current_time
        class(abst_bc_dto), intent(inout) :: output_value

        call output_value%reset()

        ! DTOの型に応じて、自身の配列から必要な値をマッピングする
        select type (dto => output_value)
        type is (type_bc_data_scalar)
            dto%prescribed_value = self%constant_values(1)
        type is (type_bc_data_robin)
            dto%transfer_coeff = self%constant_values(1)
            dto%environment_value = self%constant_values(2)
        type is (type_bc_data_hydraulic)
            dto%potential_flux = self%constant_values(1)
            dto%limit_min = self%constant_values(2)
            dto%limit_max = self%constant_values(3)
        type is (type_bc_data_cauchy)
            dto%prescribed_value = self%constant_values(1)
            dto%flux_value = self%constant_values(2)
            dto%flux_derivative = self%constant_values(3)
        end select
    end subroutine get_data_bc_data_constant

end submodule data_provider_constant
