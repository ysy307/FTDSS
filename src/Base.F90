submodule(Calculate_ThermalConductivity) Calc_THC_Base

contains

    module function Calc_THC_3(NodeBelonging, lambda_soil, phi_soil, &
                               lambda_water, phi_water, lambda_ice, phi_ice) result(lambda)
        implicit none
        type(Belonging), intent(inout) :: NodeBelonging
        real(real64), intent(in) :: lambda_soil(:)
        real(real64), intent(in) :: phi_soil
        real(real64), intent(in) :: lambda_water(:)
        real(real64), intent(in) :: phi_water
        real(real64), intent(in) :: lambda_ice(:)
        real(real64), intent(in) :: phi_ice
        real(real64) :: lambda

        real(real64) :: val_lambda_soil, val_lambda_water, val_lambda_ice

        val_lambda_soil = NodeBelonging%value(lambda_soil)
        val_lambda_water = NodeBelonging%value(lambda_water)
        val_lambda_ice = NodeBelonging%value(lambda_ice)

        lambda = val_lambda_soil**phi_soil &
                 * val_lambda_water**phi_water &
                 * val_lambda_ice**phi_ice

    end function Calc_THC_3

end submodule Calc_THC_Base
