submodule(Calculate_Density) Calc_DEN_3
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Construct each type of density
    !----------------------------------------------------------------------------------------------------
    module function DEN_3_Construct(iRegion, Input) result(Structure)
        implicit none
        class(Abstract_Density), allocatable :: Structure
        integer(int32), intent(in) :: iRegion
        type(Type_Input), intent(in) :: Input

        if (allocated(Structure)) deallocate (Structure)
        allocate (Type_Density_3Phase :: Structure)

        Structure%Material1 = Input%Regions(iRegion)%Thermal%rho(1)
        Structure%Material2 = Input%Regions(iRegion)%Thermal%rho(2)
        Structure%Material3 = Input%Regions(iRegion)%Thermal%rho(3)

    end function DEN_3_Construct

    module function Calc_DEN_3_Wrap(self, phi, Temperature, Pw, Ice) result(density)
        implicit none
        class(Type_Density_3Phase), intent(in) :: self
        real(real64), intent(in) :: phi
        real(real64), intent(in) :: Temperature
        real(real64), intent(in), optional :: Pw
        class(Abstract_Ice), intent(inout), optional :: Ice
        real(real64) :: density

        density = Calc_DEN_3(self%Material1, phi1, self%Material2, phi2, self%Material3, phi3)
    end function Calc_DEN_3_Wrap

    ! module subroutine Update_DEN_3(self, NodeBelonging, arr_phi1, arr_phi2, arr_phi3, arr_phi4)
    !     implicit none
    !     class(Type_Density_3Phase), intent(inout) :: self
    !     type(Belonging), intent(inout) :: NodeBelonging(:)
    !     real(real64), intent(in), optional :: arr_phi1(:)
    !     real(real64), intent(in), optional :: arr_phi2(:)
    !     real(real64), intent(in), optional :: arr_phi3(:)
    !     real(real64), intent(in), optional :: arr_phi4(:)
    !     integer(int32) :: iN
    !     real(real64) :: density_soil, density_water, density_ice

    !     !$omp parallel do private(iN)
    !     do iN = 1, self%nsize
    !         self%value(iN, 1) = Calc_DEN_3(NodeBelonging(iN), &
    !                                        self%soil, &
    !                                        arr_phi1(iN), &
    !                                        self%water, &
    !                                        arr_phi2(iN), &
    !                                        self%ice, &
    !                                        arr_phi3(iN))
    !     end do
    !     !$omp end parallel do
    ! end subroutine Update_DEN_3

end submodule Calc_DEN_3
