submodule(Calculate_ThermalConductivity) Calculate_ThermalConductivity_3Phase
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Construct each type of heat conductivity
    !----------------------------------------------------------------------------------------------------
    module function ThermalConductivity_3Phase_Construct(Input) result(structure)
        implicit none
        type(Type_Input), intent(in) :: Input
        class(Abstract_ThermalConductivity), allocatable :: structure

        integer(int32) :: iRegion

        allocate (Type_ThermalConductivity_3Phase :: structure)
        select type (this => structure)
        type is (Type_ThermalConductivity_3Phase)
            this%numRegion = Input%Basic%numRegion
            call Allocate_Array(this%soil, this%numRegion)
            call Allocate_Array(this%water, this%numRegion)
            call Allocate_Array(this%ice, this%numRegion)

            do iRegion = 1, this%numRegion
                this%soil(iRegion) = Input%Regions(iRegion)%Thermal%lambda(1)
                this%water(iRegion) = Input%Regions(iRegion)%Thermal%lambda(2)
                this%ice(iRegion) = Input%Regions(iRegion)%Thermal%lambda(3)
            end do

            this%nsize = Input%VTK%numPoints
            call Allocate_Array(this%value, this%nsize, 1_int32)
            this%value(:, :) = 0.0d0

        end select

    end function ThermalConductivity_3Phase_Construct

    function Calculate_ThermalConductivity_3Phase(NodeBelonging, lambda_soil, phi_soil, &
                                                  lambda_water, phi_water, lambda_ice, phi_ice) result(lambda)
        implicit none
        type(Belonging), intent(in) :: NodeBelonging
        real(real64), intent(in) :: lambda_soil(:)
        real(real64), intent(in) :: phi_soil
        real(real64), intent(in) :: lambda_water(:)
        real(real64), intent(in) :: phi_water
        real(real64), intent(in) :: lambda_ice(:)
        real(real64), intent(in) :: phi_ice
        real(real64) :: lambda

        real(real64) :: val_lambda_soil, val_lambda_water, val_lambda_ice

        val_lambda_soil = sum(lambda_soil(NodeBelonging%group(:))) / NodeBelonging%nsize
        val_lambda_water = sum(lambda_water(NodeBelonging%group(:))) / NodeBelonging%nsize
        val_lambda_ice = sum(lambda_ice(NodeBelonging%group(:))) / NodeBelonging%nsize

        lambda = val_lambda_soil**(1.0d0 - phi_soil) &
                 * val_lambda_water**phi_water &
                 * val_lambda_ice**phi_ice

    end function Calculate_ThermalConductivity_3Phase

    module function Calculate_ThermalConductivity_3Phase_Wrap(self, NodeBelonging, phi1, phi2, phi3, phi4, waterFlux) result(lambda)
        implicit none
        class(Type_ThermalConductivity_3Phase), intent(in) :: self
        type(Belonging), intent(in) :: NodeBelonging
        real(real64), intent(in), optional :: phi1
        real(real64), intent(in), optional :: phi2
        real(real64), intent(in), optional :: phi3
        real(real64), intent(in), optional :: phi4
        real(real64), intent(in), optional :: waterFlux(:)
        real(real64) :: lambda

        lambda = Calculate_ThermalConductivity_3Phase(NodeBelonging, self%soil, phi1, self%water, phi2, self%ice, phi3)
    end function Calculate_ThermalConductivity_3Phase_Wrap

    module subroutine Update_ThermalConductivity_3Phase(self, NodeBelonging, arr_phi1, arr_phi2, arr_phi3, arr_phi4, waterFlux)
        implicit none
        class(Type_ThermalConductivity_3Phase), intent(inout) :: self
        type(Belonging), intent(in) :: NodeBelonging(:)
        real(real64), intent(in), optional :: arr_phi1(:)
        real(real64), intent(in), optional :: arr_phi2(:)
        real(real64), intent(in), optional :: arr_phi3(:)
        real(real64), intent(in), optional :: arr_phi4(:)
        type(DP3d), intent(in), optional :: waterFlux
        integer(int32) :: iN
        real(real64) :: lambda_soil, lambda_water, lambda_ice

        !$omp parallel do private(iN)
        do iN = 1, self%nsize
            self%value(iN, 1) = Calculate_ThermalConductivity_3Phase(NodeBelonging(iN), &
                                                                     self%soil, &
                                                                     arr_phi1(iN), &
                                                                     self%water, &
                                                                     arr_phi2(iN), &
                                                                     self%ice, &
                                                                     arr_phi3(iN))
        end do
        !$omp end parallel do
    end subroutine Update_ThermalConductivity_3Phase

end submodule
