submodule(physics_models_wrf) calculate_wrf_base
    implicit none
contains

    module subroutine initialize_holder_wrfs(self, input, i_material)
        implicit none
        class(holder_wrfs), intent(inout) :: self
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: i_material

        select case (input%basic%materials(i_material)%thermal%phase_change%wrf%model_number)
        case (1)
            self%p = type_wrf_bc(input, i_material)
        case (2)
            self%p = type_wrf_vg(input, i_material)
        case (3)
            self%p = type_wrf_ko(input, i_material)
        case (4)
            self%p = type_wrf_mvg(input, i_material)
        case (5)
            self%p = type_wrf_durner(input, i_material)
        case (6)
            self%p = type_wrf_dvgch(input, i_material)
        end select

    end subroutine initialize_holder_wrfs

end submodule calculate_wrf_Base
