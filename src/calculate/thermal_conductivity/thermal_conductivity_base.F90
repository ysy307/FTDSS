submodule(calculate_thermal_conductivity) calc_thc_base
    implicit none
contains
    module subroutine initialize_holder_thcs(self, input, i_material)
        implicit none
        class(holder_thcs), intent(inout) :: self
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: i_material

        select case (input%basic%materials(i_material)%phase)
        case (3)
            self%p = type_thc_3phase(input, i_material)
        end select

    end subroutine initialize_holder_thcs

    module function calc_thc_3(lambda_soil, phi_soil, &
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

end submodule calc_thc_base
