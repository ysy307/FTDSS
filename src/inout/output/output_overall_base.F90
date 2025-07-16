submodule(input_output) input_output_overall_base
    implicit none

contains
    module subroutine initialize_input_type_output_overall(self, input, coordinate, domain)
        implicit none
        class(type_output_overall), intent(inout) :: self
        type(Type_input), intent(in) :: input
        type(type_dp_3d), intent(in) :: coordinate
        type(type_domain), intent(inout) :: domain

        select case (self%fextend)
        case (".vtk")
            call self%initialize_vtk(input, coordinate, domain)
        case (".vtu")
            call self%initialize_vtu(input, coordinate, domain)
        end select

    end subroutine initialize_input_type_output_overall

end submodule input_output_overall_base
