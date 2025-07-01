submodule(Calculate_ThermalConductivity) Calc_THC_3
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Construct each type of heat conductivity
    !----------------------------------------------------------------------------------------------------
    module function THC_3_Construct(region_id, lambda1, lambda2, lambda3, lambda4) result(structure)
        implicit none
        integer(int32), intent(in) :: region_id
        real(real64), intent(in), optional :: lambda1
        real(real64), intent(in), optional :: lambda2
        real(real64), intent(in), optional :: lambda3
        real(real64), intent(in), optional :: lambda4
        class(Abst_ThermalConductivity), allocatable :: structure

        integer(int32) :: iRegion

        allocate (Type_THC_3Phase :: structure)
        select type (this => structure)
        type is (Type_THC_3Phase)
            this%region_id = region_id
            if (present(lambda1)) this%Material1 = lambda1
            if (present(lambda2)) this%Material2 = lambda2
            if (present(lambda3)) this%Material3 = lambda3
            if (present(lambda4)) this%Material4 = lambda4

        end select

    end function THC_3_Construct

    module function Calc_GaussPoint_3Phase(self, state) result(lambda)
        implicit none
        class(Type_THC_3Phase), intent(in) :: self
        type(GaussPointState_t), intent(in) :: state
        real(real64) :: lambda

        real(real64) :: phi1, phi2, phi3

        phi1 = state%porosity
        phi2 = state%water_content
        phi3 = 1.0d0 - phi1 - phi2

        lambda = Calc_THC_3(self%Material1, phi1, self%Material2, phi2, self%Material3, phi3)

    end function Calc_GaussPoint_3Phase

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
