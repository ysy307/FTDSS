module Properties_Model_Base
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Core_BaseTypes, only:GaussPointState_t

    use :: Properties_Material_Manager, only:MaterialManager_t

    use :: Calculate_ThermalConductivity, only:THCHolder
    use :: Calculate_Density, only:DENHolder
    use :: Calculate_VolumetricHeatCapacity, only:VHCHolder
    use :: Calculate_GCC, only:GCCHolder
    use :: Calculate_WRF, only:WRFHolder
    use :: Calculate_Density, only:DENHolder

    implicit none
    private
    public :: Proereties_Model_t

    type :: Proereties_Model_t
        type(MaterialManager_t) :: Materials
    contains
        procedure, pass(self) :: get_lambda => calculate_THC
        procedure, pass(self) :: get_Ca => calculate_heat_capacity
        ! procedure, pass(self) :: get_dCa_dT => calculate_dcdt
        procedure, pass(self) :: get_Qw => calculate_water_content
    end type

contains
    function calculate_THC(self, state, region_id) result(lambda)
        implicit none
        class(Proereties_Model_t), intent(in) :: self
        type(GaussPointState_t), intent(in) :: state
        integer(int32), intent(in) :: region_id
        real(real64) :: lambda
        type(THCHolder) :: THC_holder

        THC_holder = self%Materials%get_THC(region_id)
        lambda = THC_holder%l%Calc_GaussPoint(state)

    end function

    function calculate_heat_capacity(self, state, region_id) result(Ca)
        implicit none
        class(Proereties_Model_t), intent(in) :: self
        type(GaussPointState_t), intent(in) :: state
        integer(int32), intent(in) :: region_id
        real(real64) :: Ca

        real(real64) :: Lf, dQi_dT

        type(VHCHolder) :: VHC_holder
        type(GCCHolder) :: GCC_holder
        type(DENHolder) :: DEN_holder
        type(WRFHolder) :: WRF_holder

        VHC_holder = self%Materials%get_VHC(region_id)
        DEN_holder = self%Materials%get_DEN(region_id)
        GCC_holder = self%Materials%get_GCC(region_id)
        WRF_holder = self%Materials%get_WRF(region_id)

        Lf = GCC_holder%g%Lf
        dQi_dT = WRF_holder%w%DERIV(-GCC_holder%g%Calc(T=state%temperature, &
                                                       Pw=state%pressure, &
                                                       rhoW=DEN_holder%d%Material2, &
                                                       rhoI=DEN_holder%d%Material3)) &
                 * GCC_holder%g%DERIV(T=state%temperature, &
                                      Pw=state%pressure, &
                                      rhoW=DEN_holder%d%Material2, &
                                      rhoI=DEN_holder%d%Material3)

        ! , state, DEN, LatentHeat, dQi_dT
        Ca = VHC_holder%c%Calc_GaussPoint(state=state, &
                                          DEN=DEN_holder, &
                                          LatentHeat=Lf, &
                                          dQi_dT=dQi_dT)

    end function

    ! function calculate_dcdt(self, state, region_id) result(dCa_dT)
    !     implicit none
    !     class(Proereties_Model_t), intent(in) :: self
    !     type(GaussPointState_t), intent(in) :: state
    !     integer(int32), intent(in) :: region_id
    !     real(real64) :: dCa_dT
    !     ! ... 熱容量の温度微分を計算 ...
    ! end function

    function calculate_water_content(self, state, region_id) result(water_content)
        implicit none
        class(Proereties_Model_t), intent(in) :: self
        type(GaussPointState_t), intent(in) :: state
        integer(int32), intent(in) :: region_id
        real(real64) :: water_content

        type(GCCHolder) :: GCC_holder
        type(WRFHolder) :: WRF_holder
        type(DENHolder) :: DEN_holder

        GCC_holder = self%Materials%get_GCC(region_id)
        WRF_holder = self%Materials%get_WRF(region_id)
        DEN_holder = self%Materials%get_DEN(region_id)

        ! ... 水分量の計算ロジックをここに追加 ...
        ! print *, GCC_holder%g%Calc(T=-10.0d0, &
        !                            Pw=state%pressure, &
        !                            rhoW=DEN_holder%d%Material2, &
        !                            rhoI=DEN_holder%d%Material3)
        water_content = WRF_holder%w%Calc(-GCC_holder%g%Calc(T=state%temperature, &
                                                             Pw=state%pressure, &
                                                             rhoW=DEN_holder%d%Material2, &
                                                             rhoI=DEN_holder%d%Material3))
        ! print *, state%temperature, water_content, DEN_holder%d%Material2

    end function

end module Properties_Model_Base
