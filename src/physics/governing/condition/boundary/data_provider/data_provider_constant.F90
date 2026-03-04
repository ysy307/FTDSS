submodule(condition_boundary_data_provider) data_provider_constant
    implicit none
contains

    module subroutine initialize_type_bc_data_constant(self, config_bc)
        implicit none
        class(type_bc_data_constant), intent(inout) :: self
        type(type_config_bc), intent(in) :: config_bc

    end subroutine initialize_type_bc_data_constant

    module subroutine destroy_type_bc_data_constant(self)
        implicit none
        class(type_bc_data_constant), intent(inout) :: self
    end subroutine destroy_type_bc_data_constant

    module subroutine get_data_bc_data_constant(self, current_time, output_value)
        implicit none
        class(type_bc_data_constant), intent(in) :: self
        real(real64), intent(in) :: current_time
        class(abst_bc_dto), intent(inout) :: output_value

    end subroutine get_data_bc_data_constant

end submodule data_provider_constant
