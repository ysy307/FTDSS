module Properties_Thermal_Model_3Phase
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Core_BaseTypes, only:GaussPointState_t
    use :: Properties_Thermal_Model_Base, only:ThermalModel_Base_t
    use :: Properties_Thermal_Material_Manager, only:ThermalMaterialManager_t
    use Calculate_ThermalConductivity, only: THCHolder
    implicit none

    type, extends(ThermalModel_Base_t) :: ThermalModel_3Phase_t
        private
        ! 3相の熱伝導率を計算するためのホルダー
        type(ThermalMaterialManager_t) :: Materials

    contains
        procedure :: get_lambda => calculate_THC
        procedure :: get_Ca => calculate_heat_capacity
        procedure :: get_dCa_dT => calculate_dcdt
    end type

contains
    function calculate_THC(self, state, region_id) result(lambda)
        class(ThermalModel_3Phase_t), intent(in) :: self
        type(GaussPointState_t), intent(in) :: state
        integer(int32), intent(in) :: region_id
        real(real64) :: lambda
        type(THCHolder) :: THC_holder

        THC_holder = self%Materials%get_thc_model(region_id)
        lambda = THC_holder%l%Calc_GaussPoint(state)

    end function

    function calculate_heat_capacity(self, state) result(Ca)
        class(ThermalModel_3Phase_t), intent(in) :: self
        type(GaussPointState_t), intent(in) :: state
        real(real64) :: Ca
        ! ... 見かけの熱容量を計算 ...
    end function

    function calculate_dcdt(self, state) result(dCa_dT)
        class(ThermalModel_3Phase_t), intent(in) :: self
        type(GaussPointState_t), intent(in) :: state
        real(real64) :: dCa_dT
        ! ... 熱容量の温度微分を計算 ...
    end function
end module Properties_Thermal_Model_3Phase
