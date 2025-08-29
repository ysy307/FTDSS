submodule(calculate_gcc) gcc_base
    implicit none
contains

    module subroutine initialize_holder_gccs(self, input, i_material)
        implicit none
        class(holder_gccs), intent(inout) :: self
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: i_material

        if (input%basic%materials(i_material)%thermal%phase_change%gcc%is_segregation) then
            select case (input%basic%materials(i_material)%thermal%phase_change%gcc%unit)
            case ('m')
                self%p = type_gcc_segregation_m(input%basic%materials(i_material)%thermal%phase_change%freezing_temperature, &
                                                input%basic%materials(i_material)%thermal%phase_change%latent_heat_fusion)
            case ("pa")
                self%p = type_gcc_segregation_pa(input%basic%materials(i_material)%thermal%phase_change%freezing_temperature, &
                                                 input%basic%materials(i_material)%thermal%phase_change%latent_heat_fusion)
            end select
        else
            select case (input%basic%materials(i_material)%thermal%phase_change%gcc%unit)
            case ('m')
                self%p = type_gcc_non_segregation_m(input%basic%materials(i_material)%thermal%phase_change%freezing_temperature, &
                                                    input%basic%materials(i_material)%thermal%phase_change%latent_heat_fusion)
            case ("pa")
                self%p = type_gcc_non_segregation_pa(input%basic%materials(i_material)%thermal%phase_change%freezing_temperature, &
                                                     input%basic%materials(i_material)%thermal%phase_change%latent_heat_fusion)
            end select
        end if

    end subroutine initialize_holder_gccs

end submodule gcc_base
