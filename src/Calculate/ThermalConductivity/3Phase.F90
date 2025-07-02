submodule(Calculate_ThermalConductivity) Calc_THC_3
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Construct each type of heat conductivity
    !----------------------------------------------------------------------------------------------------
    module function THC_3_Construct(iRegion, Input) result(Structure)
        implicit none
        class(Abst_THC), allocatable :: Structure
        integer(int32), intent(in) :: iRegion
        type(Type_Input), intent(in) :: Input

        if (allocated(Structure)) deallocate (Structure)
        allocate (Type_THC_3Phase :: Structure)

        Structure%region_id = iRegion

        Structure%Material1 = Input%Regions(iRegion)%Thermal%rho(1)
        Structure%Material2 = Input%Regions(iRegion)%Thermal%rho(2)
        Structure%Material3 = Input%Regions(iRegion)%Thermal%rho(3)

    end function THC_3_Construct

    module function Calc_THC_GaussPoint_3Phase(self, state) result(lambda)
        implicit none
        class(Type_THC_3Phase), intent(in) :: self
        type(GaussPointState_t), intent(in) :: state
        real(real64) :: lambda

        real(real64) :: phi1, phi2, phi3

        phi1 = 1.0d0 - state%porosity
        phi2 = state%water_content
        phi3 = 1.0d0 - phi1 - phi2

        lambda = Calc_THC_3(self%Material1, phi1, self%Material2, phi2, self%Material3, phi3)

    end function Calc_THC_GaussPoint_3Phase

    ! module function Calc_THC_3_Wrap(self, NodeBelonging, phi1, phi2, phi3, phi4, waterFlux) result(lambda)
    !     implicit none
    !     class(Type_ThermalConductivity_3Phase), intent(in) :: self
    !     type(Belonging), intent(inout) :: NodeBelonging
    !     real(real64), intent(in), optional :: phi1
    !     real(real64), intent(in), optional :: phi2
    !     real(real64), intent(in), optional :: phi3
    !     real(real64), intent(in), optional :: phi4
    !     real(real64), intent(in), optional :: waterFlux(:)
    !     real(real64) :: lambda

    !     lambda = Calc_THC_3(NodeBelonging, self%soil, phi1, self%water, phi2, self%ice, phi3)
    ! end function Calc_THC_3_Wrap

    ! module subroutine Update_THC_3(self, NodeBelonging, arr_phi1, arr_phi2, arr_phi3, arr_phi4, waterFlux)
    !     implicit none
    !     class(Type_ThermalConductivity_3Phase), intent(inout) :: self
    !     type(Belonging), intent(inout) :: NodeBelonging(:)
    !     real(real64), intent(in), optional :: arr_phi1(:)
    !     real(real64), intent(in), optional :: arr_phi2(:)
    !     real(real64), intent(in), optional :: arr_phi3(:)
    !     real(real64), intent(in), optional :: arr_phi4(:)
    !     type(DP3d), intent(in), optional :: waterFlux

    !     integer(int32) :: iN

    !     !$omp parallel do private(iN)
    !     do iN = 1, self%nsize
    !         self%value(iN, 1) = Calc_THC_3(NodeBelonging(iN), &
    !                                        self%soil(:), &
    !                                        arr_phi1(iN), &
    !                                        self%water(:), &
    !                                        arr_phi2(iN), &
    !                                        self%ice(:), &
    !                                        arr_phi3(iN))
    !     end do
    !     !$omp end parallel do
    ! end subroutine Update_THC_3

end submodule Calc_THC_3
