submodule(condition_boundary_data_provider) data_provider_dynamic
    implicit none
contains

    module subroutine initialize_type_bc_data_dynamic(self, config_bc)
        implicit none
        class(type_bc_data_dynamic), intent(inout) :: self
        type(type_config_bc), intent(in) :: config_bc

        if (allocated(self%current_buffer)) deallocate (self%current_buffer)
        ! allocate(self%current_buffer(config_bc%num_variables))
        ! self%current_buffer = 0.0d0
    end subroutine initialize_type_bc_data_dynamic

    module subroutine destroy_type_bc_data_dynamic(self)
        implicit none
        class(type_bc_data_dynamic), intent(inout) :: self

        if (allocated(self%current_buffer)) deallocate (self%current_buffer)
    end subroutine destroy_type_bc_data_dynamic

    module subroutine update_buffer_bc_data_dynamic(self, new_values)
        implicit none
        class(type_bc_data_dynamic), intent(inout) :: self
        real(real64), intent(in) :: new_values(:)

        self%current_buffer = new_values
    end subroutine update_buffer_bc_data_dynamic

    module subroutine get_data_bc_data_dynamic(self, current_time, output_value)
        implicit none
        class(type_bc_data_dynamic), intent(inout) :: self
        real(real64), intent(in) :: current_time
        class(abst_bc_dto), intent(inout) :: output_value

        call output_value%reset()

        ! 大気側などからプッシュされた最新のバッファから値をマッピングする
        select type (dto => output_value)
        type is (type_bc_data_scalar)
            dto%prescribed_value = self%current_buffer(1)

        type is (type_bc_data_robin)
            dto%transfer_coeff = self%current_buffer(1)
            dto%environment_value = self%current_buffer(2)

        type is (type_bc_data_hydraulic)
            dto%potential_flux = self%current_buffer(1)
            dto%limit_min = self%current_buffer(2)
            dto%limit_max = self%current_buffer(3)
        type is (type_bc_data_cauchy)
            dto%prescribed_value = self%current_buffer(1)
            dto%flux_value = self%current_buffer(2)
            dto%flux_derivative = self%current_buffer(3)
        end select
    end subroutine get_data_bc_data_dynamic

end submodule data_provider_dynamic
