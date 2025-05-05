submodule(Calculate_SpecificHeat) Calc_SPH_Base
contains
    function Calc_SPH_3(NodeBelonging, SpecificHeat_soil, phi_soil, &
                        SpecificHeat_water, phi_water, SpecificHeat_ice, phi_ice) result(SpecificHeat)
        implicit none
        type(Belonging), intent(inout) :: NodeBelonging
        real(real64), intent(in) :: SpecificHeat_soil(:)
        real(real64), intent(in) :: phi_soil
        real(real64), intent(in) :: SpecificHeat_water(:)
        real(real64), intent(in) :: phi_water
        real(real64), intent(in) :: SpecificHeat_ice(:)
        real(real64), intent(in) :: phi_ice
        real(real64) :: SpecificHeat

        real(real64) :: val_SpecificHeat_soil, val_SpecificHeat_water, val_SpecificHeat_ice

        val_SpecificHeat_soil = NodeBelonging%value(SpecificHeat_soil)
        val_SpecificHeat_water = NodeBelonging%value(SpecificHeat_water)
        val_SpecificHeat_ice = NodeBelonging%value(SpecificHeat_ice)

        SpecificHeat = val_SpecificHeat_soil * phi_soil &
                       + val_SpecificHeat_water * phi_water &
                       + val_SpecificHeat_ice * phi_ice

    end function Calc_SPH_3
end submodule Calc_SPH_Base
