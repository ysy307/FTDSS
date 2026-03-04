submodule(condition_boundary_data_provider) data_provider_dynamic
    implicit none
contains
    module subroutine initialize_type_bc_data_dynamic(self, config_bc)
        implicit none
        class(type_bc_data_dynamic), intent(inout) :: self
        type(type_config_bc), intent(in) :: config_bc
    end subroutine initialize_type_bc_data_dynamic

    module subroutine get_data_bc_data_dynamic(self, current_time, output_value)
        implicit none
        class(type_bc_data_dynamic), intent(in) :: self
        real(real64), intent(in) :: current_time
        class(abst_bc_dto), intent(inout) :: output_value
    end subroutine get_data_bc_data_dynamic

    module subroutine update_buffer_bc_data_dynamic(self, new_values)
        implicit none
        class(type_bc_data_dynamic), intent(inout) :: self
        real(real64), intent(in) :: new_values(:)
    end subroutine update_buffer_bc_data_dynamic

    module subroutine destroy_type_bc_data_dynamic(self)
        implicit none
        class(type_bc_data_dynamic), intent(inout) :: self
    end subroutine destroy_type_bc_data_dynamic
end submodule data_provider_dynamic
