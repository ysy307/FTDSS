submodule(Calculate_GCC) GCC_Base
    implicit none
contains

    module subroutine GCCHolder_initialize(self, iRegion, Input)
        implicit none
        class(GCCHolder), intent(inout) :: self
        integer(int32), intent(in) :: iRegion
        type(Type_Input), intent(in) :: Input

        if (Input%Regions(iRegion)%Ice%isSegregation) then
            select case (Input%Regions(iRegion)%Ice%c_unit)
            case ('m')
                self%g = Type_GCC_Segregation_m(Input%Regions(iRegion)%Ice%Tf, Input%Regions(iRegion)%Thermal%LatentHeat)
            case ("Pa")
                self%g = Type_GCC_Segregation_Pa(Input%Regions(iRegion)%Ice%Tf, Input%Regions(iRegion)%Thermal%LatentHeat)
            end select
        else
            select case (Input%Regions(iRegion)%Ice%c_unit)
            case ('m')
                self%g = Type_GCC_NonSegregation_m(Input%Regions(iRegion)%Ice%Tf, Input%Regions(iRegion)%Thermal%LatentHeat)
            case ("Pa")
                self%g = Type_GCC_NonSegregation_Pa(Input%Regions(iRegion)%Ice%Tf, Input%Regions(iRegion)%Thermal%LatentHeat)
            end select
        end if

    end subroutine GCCHolder_initialize

end submodule GCC_Base
