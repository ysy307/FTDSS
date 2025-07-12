submodule(Calculate_WRF) Calculate_WRF_Base
    implicit none
contains

    module subroutine WRFHolder_initialize(self, iRegion, Input)
        implicit none
        class(WRFHolder), intent(inout) :: self
        integer(int32), intent(in) :: iRegion
        type(Type_Input), intent(in) :: Input

        select case (Input%Regions(iRegion)%Ice%ModelType)
        case (1)
            self%w = Type_WRF_BC(Input%Regions(iRegion))
        case (2)
            self%w = Type_WRF_VG(Input%Regions(iRegion))
        case (3)
            self%w = Type_WRF_KO(Input%Regions(iRegion))
        case (4)
            self%w = Type_WRF_MVG(Input%Regions(iRegion))
        case (5)
            self%w = Type_WRF_Durner(Input%Regions(iRegion))
        case (6)
            self%w = Type_WRF_DVGCH(Input%Regions(iRegion))
        case default
            stop 'Invalid ModelType'
        end select

    end subroutine WRFHolder_initialize

end submodule Calculate_WRF_Base
