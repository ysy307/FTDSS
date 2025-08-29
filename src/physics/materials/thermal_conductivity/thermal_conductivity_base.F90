submodule(physics_material_thermal_conductivity) thermal_conductivity_base
    implicit none
contains
    module subroutine initialize_holder_thcs(self, input, material_id)
        implicit none
        class(holder_thcs), intent(inout) :: self
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: material_id

        select case (input%basic%materials(material_id)%phase)
        case (3)
            self%p = type_thc_3phase(input, material_id)
        end select

    end subroutine initialize_holder_thcs

    module pure elemental function calc_thc_3(lambda_soil, phi_soil, &
                                              lambda_water, phi_water, &
                                              lambda_ice, phi_ice) result(lambda)
        implicit none
        real(real64), intent(in) :: lambda_soil
        real(real64), intent(in) :: phi_soil
        real(real64), intent(in) :: lambda_water
        real(real64), intent(in) :: phi_water
        real(real64), intent(in) :: lambda_ice
        real(real64), intent(in) :: phi_ice
        real(real64) :: lambda

        lambda = lambda_soil**phi_soil &
                 * lambda_water**phi_water &
                 * lambda_ice**phi_ice

    end function calc_thc_3

end submodule thermal_conductivity_base
