submodule(calculate_wrf) calculate_wrf_base
    implicit none
contains

    module subroutine initialize_holder_wrfs(self, iRegion, Input)
        implicit none
        class(holder_wrfs), intent(inout) :: self
        integer(int32), intent(in) :: iRegion
        type(Type_Input), intent(in) :: Input

        select case (Input%Regions(iRegion)%Ice%ModelType)
        case (1)
            self%p = type_wrf_bc(Input%Regions(iRegion))
        case (2)
            self%p = type_wrf_vg(Input%Regions(iRegion))
        case (3)
            self%p = type_wrf_ko(Input%Regions(iRegion))
        case (4)
            self%p = type_wrf_mvg(Input%Regions(iRegion))
        case (5)
            self%p = type_wrf_durner(Input%Regions(iRegion))
        case (6)
            self%p = type_wrf_dvgch(Input%Regions(iRegion))
        case default
            stop 'Invalid ModelType'
        end select

    end subroutine initialize_holder_wrfs

end submodule calculate_wrf_Base
