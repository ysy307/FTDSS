module properties_properties_manager
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core, only:type_gauss_point_state
    use :: module_calculate, only:holder_gccs, holder_wrfs, holder_dens, holder_vhcs, holder_thcs
    use :: Properties_material_Manager, only:type_material_manager

    implicit none
    private
    public :: type_proereties_manager

    type :: type_proereties_manager
        type(type_material_manager) :: materials
    contains
        procedure, pass(self) :: get_lambda => calculate_THC
        procedure, pass(self) :: get_Ca => calculate_heat_capacity
        ! procedure, pass(self) :: get_dCa_dT => calculate_dcdt
        procedure, pass(self) :: get_Qw => calculate_water_content
    end type

contains
    function calculate_THC(self, state, region_id) result(lambda)
        implicit none
        class(type_proereties_manager), intent(in) :: self
        type(type_gauss_point_state), intent(in) :: state
        integer(int32), intent(in) :: region_id
        real(real64) :: lambda
        type(holder_thcs) :: THC_holder

        THC_holder = self%materials%get_THC(region_id)
        lambda = THC_holder%p%calc_gauss_point(state)

    end function

    function calculate_heat_capacity(self, state, region_id) result(Ca)
        implicit none
        class(type_proereties_manager), intent(in) :: self
        type(type_gauss_point_state), intent(in) :: state
        integer(int32), intent(in) :: region_id
        real(real64) :: Ca

        real(real64) :: Lf, dQi_dT

        type(holder_vhcs) :: VHC_holder
        type(holder_gccs) :: GCC_holder
        type(holder_dens) :: DEN_holder
        type(holder_wrfs) :: WRF_holder

        VHC_holder = self%materials%get_VHC(region_id)
        DEN_holder = self%materials%get_DEN(region_id)
        GCC_holder = self%materials%get_GCC(region_id)
        WRF_holder = self%materials%get_WRF(region_id)

        Lf = GCC_holder%p%Lf
        dQi_dT = WRF_holder%p%deriv(-GCC_holder%p%calc(T=state%temperature, &
                                                       Pw=state%pressure, &
                                                       rhoW=DEN_holder%p%material2, &
                                                       rhoI=DEN_holder%p%material3)) &
                 * GCC_holder%p%deriv(T=state%temperature, &
                                      Pw=state%pressure, &
                                      rhoW=DEN_holder%p%material2, &
                                      rhoI=DEN_holder%p%material3)

        ! , state, DEN, LatentHeat, dQi_dT
        Ca = VHC_holder%p%calc_gauss_point(state=state, &
                                           DEN=DEN_holder, &
                                           LatentHeat=Lf, &
                                           dQi_dT=dQi_dT)

    end function

    ! function calculate_dcdt(self, state, region_id) result(dCa_dT)
    !     implicit none
    !     class(type_proereties_manager), intent(in) :: self
    !     type(type_gauss_point_state), intent(in) :: state
    !     integer(int32), intent(in) :: region_id
    !     real(real64) :: dCa_dT
    !     ! ... 熱容量の温度微分を計算 ...
    ! end function

    function calculate_water_content(self, state, region_id) result(water_content)
        implicit none
        class(type_proereties_manager), intent(in) :: self
        type(type_gauss_point_state), intent(in) :: state
        integer(int32), intent(in) :: region_id
        real(real64) :: water_content

        type(holder_gccs) :: GCC_holder
        type(holder_wrfs) :: WRF_holder
        type(holder_dens) :: DEN_holder

        GCC_holder = self%materials%get_GCC(region_id)
        WRF_holder = self%materials%get_WRF(region_id)
        DEN_holder = self%materials%get_DEN(region_id)

        ! ... 水分量の計算ロジックをここに追加 ...
        ! print *, GCC_holder%g%Calc(T=-10.0d0, &
        !                            Pw=state%pressure, &
        !                            rhoW=DEN_holder%p%material2, &
        !                            rhoI=DEN_holder%p%material3)
        water_content = WRF_holder%p%calc(-GCC_holder%p%calc(T=state%temperature, &
                                                             Pw=state%pressure, &
                                                             rhoW=DEN_holder%p%material2, &
                                                             rhoI=DEN_holder%p%material3))
        ! print *, state%temperature, water_content, DEN_holder%p%material2

    end function

end module properties_properties_manager
