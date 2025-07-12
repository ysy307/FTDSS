submodule(calculate_gcc) gcc_base
    implicit none
contains

    module subroutine initialize_holder_gccs(self, iRegion, Input)
        implicit none
        class(holder_gccs), intent(inout) :: self
        integer(int32), intent(in) :: iRegion
        type(type_Input), intent(in) :: Input

        if (Input%Regions(iRegion)%Ice%isSegregation) then
            select case (Input%Regions(iRegion)%Ice%c_unit)
            case ('m')
                self%p = type_gcc_segregation_m(Input%Regions(iRegion)%Ice%Tf, Input%Regions(iRegion)%Thermal%LatentHeat)
            case ("Pa")
                self%p = type_gcc_segregation_pa(Input%Regions(iRegion)%Ice%Tf, Input%Regions(iRegion)%Thermal%LatentHeat)
            end select
        else
            select case (Input%Regions(iRegion)%Ice%c_unit)
            case ('m')
                self%p = type_gcc_non_segregation_m(Input%Regions(iRegion)%Ice%Tf, Input%Regions(iRegion)%Thermal%LatentHeat)
            case ("Pa")
                self%p = type_gcc_non_segregation_pa(Input%Regions(iRegion)%Ice%Tf, Input%Regions(iRegion)%Thermal%LatentHeat)
            end select
        end if

    end subroutine initialize_holder_gccs

end submodule gcc_base
