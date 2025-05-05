submodule(Calculate_Density) Calc_DEN_Base
    implicit none

contains
    module function Calc_DEN_3(NodeBelonging, density_soil, phi_soil, &
                               density_water, phi_water, density_ice, phi_ice) result(density)
        implicit none
        type(Belonging), intent(inout) :: NodeBelonging
        real(real64), intent(in) :: density_soil(:)
        real(real64), intent(in) :: phi_soil
        real(real64), intent(in) :: density_water(:)
        real(real64), intent(in) :: phi_water
        real(real64), intent(in) :: density_ice(:)
        real(real64), intent(in) :: phi_ice
        real(real64) :: density

        real(real64) :: val_density_soil, val_density_water, val_density_ice

        val_density_soil = NodeBelonging%value(density_soil)
        val_density_water = NodeBelonging%value(density_water)
        val_density_ice = NodeBelonging%value(density_ice)

        density = val_density_soil * phi_soil &
                  + val_density_water * phi_water &
                  + val_density_ice * phi_ice

    end function Calc_DEN_3
end submodule Calc_DEN_Base
