submodule(calculate_wrf) calculate_wrf_base
    implicit none
contains

    module subroutine initialize_holder_wrfs(self, input, i_material)
        implicit none
        class(holder_wrfs), intent(inout) :: self
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: i_material

        select case (input%basic%materials(i_material)%thermal%phase_change%wrf%model_number)
        case (1)
            self%p = type_wrf_bc(input%basic%materials(i_material)%thermal%phase_change%wrf)
        case (2)
            self%p = type_wrf_vg(input%basic%materials(i_material)%thermal%phase_change%wrf)
        case (3)
            self%p = type_wrf_ko(input%basic%materials(i_material)%thermal%phase_change%wrf)
        case (4)
            self%p = type_wrf_mvg(input%basic%materials(i_material)%thermal%phase_change%wrf)
        case (5)
            self%p = type_wrf_durner(input%basic%materials(i_material)%thermal%phase_change%wrf)
        case (6)
            self%p = type_wrf_dvgch(input%basic%materials(i_material)%thermal%phase_change%wrf)
        end select

    end subroutine initialize_holder_wrfs

end submodule calculate_wrf_Base
