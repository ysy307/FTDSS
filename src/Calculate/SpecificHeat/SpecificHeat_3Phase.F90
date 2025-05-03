submodule(Calculate_SpecificHeat) Calculate_SpecificHeat_3Phase
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Construct each type of SpecificHeat
    !----------------------------------------------------------------------------------------------------
    module function SPH_3Phase_Construct(Input) result(structure)
        implicit none
        type(Type_Input), intent(in) :: Input
        class(Abstract_SpecificHeat), allocatable :: structure

        integer(int32) :: iRegion

        allocate (Type_SpecificHeat_3Phase :: structure)
        select type (this => structure)
        type is (Type_SpecificHeat_3Phase)
            this%numRegion = Input%Basic%numRegion
            call Allocate_Array(this%soil, this%numRegion)
            call Allocate_Array(this%water, this%numRegion)
            call Allocate_Array(this%ice, this%numRegion)

            do iRegion = 1, this%numRegion
                this%soil(iRegion) = Input%Regions(iRegion)%Thermal%c(1)
                this%water(iRegion) = Input%Regions(iRegion)%Thermal%c(2)
                this%ice(iRegion) = Input%Regions(iRegion)%Thermal%c(3)
            end do

            this%nsize = Input%VTK%numPoints
            call Allocate_Array(this%value, this%nsize, 1_int32)
            this%value(:, :) = 0.0d0

        end select

    end function SPH_3Phase_Construct

    function Calc_SPH_3Phase(NodeBelonging, SpecificHeat_soil, phi_soil, &
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

    end function Calc_SPH_3Phase

    module function Calc_SPH_3Phase_Wrap(self, NodeBelonging, phi1, phi2, phi3, phi4) result(SpecificHeat)
        implicit none
        class(Type_SpecificHeat_3Phase), intent(in) :: self
        type(Belonging), intent(inout) :: NodeBelonging
        real(real64), intent(in), optional :: phi1
        real(real64), intent(in), optional :: phi2
        real(real64), intent(in), optional :: phi3
        real(real64), intent(in), optional :: phi4
        real(real64) :: SpecificHeat

        SpecificHeat = Calc_SPH_3Phase(NodeBelonging, self%soil, phi1, self%water, phi2, self%ice, phi3)
    end function Calc_SPH_3Phase_Wrap

    module subroutine Update_SPH_3Phase(self, NodeBelonging, arr_phi1, arr_phi2, arr_phi3, arr_phi4)
        implicit none
        class(Type_SpecificHeat_3Phase), intent(inout) :: self
        type(Belonging), intent(inout) :: NodeBelonging(:)
        real(real64), intent(in), optional :: arr_phi1(:)
        real(real64), intent(in), optional :: arr_phi2(:)
        real(real64), intent(in), optional :: arr_phi3(:)
        real(real64), intent(in), optional :: arr_phi4(:)
        integer(int32) :: iN
        real(real64) :: SpecificHeat_soil, SpecificHeat_water, SpecificHeat_ice

        !$omp parallel do private(iN)
        do iN = 1, self%nsize
            self%value(iN, 1) = Calc_SPH_3Phase(NodeBelonging(iN), &
                                                self%soil, &
                                                arr_phi1(iN), &
                                                self%water, &
                                                arr_phi2(iN), &
                                                self%ice, &
                                                arr_phi3(iN))
        end do
        !$omp end parallel do
    end subroutine Update_SPH_3Phase

end submodule Calculate_SpecificHeat_3Phase
