submodule(Calculate_ThermalConductivity) Calc_THC_3
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Construct each type of heat conductivity
    !----------------------------------------------------------------------------------------------------
    module function THC_3_Construct(Input) result(structure)
        implicit none
        type(Type_Input), intent(in) :: Input
        class(Abstract_ThermalConductivity), allocatable :: structure

        integer(int32) :: iRegion

        allocate (Type_ThermalConductivity_3Phase :: structure)
        select type (this => structure)
        type is (Type_ThermalConductivity_3Phase)
            this%nRegion = Input%Basic%numRegion
            call Allocate_Array(this%soil, this%nRegion)
            call Allocate_Array(this%water, this%nRegion)
            call Allocate_Array(this%ice, this%nRegion)

            do iRegion = 1, this%nRegion
                this%soil(iRegion) = Input%Regions(iRegion)%Thermal%lambda(1)
                this%water(iRegion) = Input%Regions(iRegion)%Thermal%lambda(2)
                this%ice(iRegion) = Input%Regions(iRegion)%Thermal%lambda(3)
            end do

            this%nsize = Input%VTK%numPoints
            call Allocate_Array(this%value, this%nsize, 1_int32)
            this%value(:, :) = 0.0d0

        end select

    end function THC_3_Construct

    module function Calc_THC_3_Wrap(self, NodeBelonging, phi1, phi2, phi3, phi4, waterFlux) result(lambda)
        implicit none
        class(Type_ThermalConductivity_3Phase), intent(in) :: self
        type(Belonging), intent(inout) :: NodeBelonging
        real(real64), intent(in), optional :: phi1
        real(real64), intent(in), optional :: phi2
        real(real64), intent(in), optional :: phi3
        real(real64), intent(in), optional :: phi4
        real(real64), intent(in), optional :: waterFlux(:)
        real(real64) :: lambda

        lambda = Calc_THC_3(NodeBelonging, self%soil, phi1, self%water, phi2, self%ice, phi3)
    end function Calc_THC_3_Wrap

    module subroutine Update_THC_3(self, NodeBelonging, arr_phi1, arr_phi2, arr_phi3, arr_phi4, waterFlux)
        implicit none
        class(Type_ThermalConductivity_3Phase), intent(inout) :: self
        type(Belonging), intent(inout) :: NodeBelonging(:)
        real(real64), intent(in), optional :: arr_phi1(:)
        real(real64), intent(in), optional :: arr_phi2(:)
        real(real64), intent(in), optional :: arr_phi3(:)
        real(real64), intent(in), optional :: arr_phi4(:)
        type(DP3d), intent(in), optional :: waterFlux

        integer(int32) :: iN

        !$omp parallel do private(iN)
        do iN = 1, self%nsize
            self%value(iN, 1) = Calc_THC_3(NodeBelonging(iN), &
                                           self%soil(:), &
                                           arr_phi1(iN), &
                                           self%water(:), &
                                           arr_phi2(iN), &
                                           self%ice(:), &
                                           arr_phi3(iN))
        end do
        !$omp end parallel do
    end subroutine Update_THC_3

end submodule Calc_THC_3
