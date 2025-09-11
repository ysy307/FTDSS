submodule(conditions_boundary) conditions_boundary_adiabatic
    implicit none
contains

    module function construct_type_bc_thermal_adiabatic(input, domain, controls, id) result(structure)
        implicit none
        type(type_input), intent(in) :: input
        type(type_domain), intent(in) :: domain
        type(type_controls), intent(in) :: controls
        integer(int32), intent(in) :: id
        class(abst_bc), allocatable :: structure

        allocate (type_bc_thermal_adiabatic :: structure)

        select type (this => structure)
        type is (type_bc_thermal_adiabatic)
            this%group_id = input%conditions%boundary_conditions(id)%id
            this%dimension = input%basic%simulation_settings%calculate_dimension - 1
            call find_target_by_group(domain, this%dimension, this%group_id, this%target_ids)
        end select
    end function construct_type_bc_thermal_adiabatic

    module subroutine apply_thermal_adiabatic(self, current_time, A, b, domain, mode)
        implicit none
        class(type_bc_thermal_adiabatic), intent(in) :: self
        real(real64), intent(in) :: current_time
        type(type_jacobian_matrix), intent(inout), optional :: A
        type(type_residual_vector), intent(inout) :: b
        type(type_domain), intent(inout), target :: domain
        integer(int32), intent(in), optional :: mode

        return

    end subroutine apply_thermal_adiabatic

end submodule conditions_boundary_adiabatic
