submodule(Calculate_Density) Calculate_Density_3Phase
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Construct each type of density
    !----------------------------------------------------------------------------------------------------
    module function DEN_3Phase_Construct(Input) result(structure)
        implicit none
        type(Type_Input), intent(in) :: Input
        class(Abstract_Density), allocatable :: structure

        integer(int32) :: iRegion

        allocate (Type_Density_3Phase :: structure)
        select type (this => structure)
        type is (Type_Density_3Phase)
            this%numRegion = Input%Basic%numRegion
            call Allocate_Array(this%soil, this%numRegion)
            call Allocate_Array(this%water, this%numRegion)
            call Allocate_Array(this%ice, this%numRegion)

            do iRegion = 1, this%numRegion
                this%soil(iRegion) = Input%Regions(iRegion)%Thermal%rho(1)
                this%water(iRegion) = Input%Regions(iRegion)%Thermal%rho(2)
                this%ice(iRegion) = Input%Regions(iRegion)%Thermal%rho(3)
            end do

            this%nsize = Input%VTK%numPoints
            call Allocate_Array(this%value, this%nsize, 1_int32)
            this%value(:, :) = 0.0d0

        end select

    end function DEN_3Phase_Construct

    function Calc_DEN_3Phase(NodeBelonging, density_soil, phi_soil, &
                             density_water, phi_water, density_ice, phi_ice) result(density)
        implicit none
        type(Belonging), intent(inout) :: NodeBelonging
        real(real64), intent(in) :: density_soil(:)
        real(real64), intent(in) :: phi_soil
        real(real64), intent(in) :: density_water(:)
        real(real64), intent(in) :: phi_water
        real(real64), intent(in) :: density_ice(:)
        real(real64), intent(in) :: phi_ice
        real(real64) :: density

        real(real64) :: val_density_soil, val_density_water, val_density_ice

        val_density_soil = NodeBelonging%value(density_soil)
        val_density_water = NodeBelonging%value(density_water)
        val_density_ice = NodeBelonging%value(density_ice)

        density = val_density_soil * phi_soil &
                  + val_density_water * phi_water &
                  + val_density_ice * phi_ice

    end function Calc_DEN_3Phase

    module function Calc_DEN_3Phase_Wrap(self, NodeBelonging, phi1, phi2, phi3, phi4) result(density)
        implicit none
        class(Type_Density_3Phase), intent(in) :: self
        type(Belonging), intent(inout) :: NodeBelonging
        real(real64), intent(in), optional :: phi1
        real(real64), intent(in), optional :: phi2
        real(real64), intent(in), optional :: phi3
        real(real64), intent(in), optional :: phi4
        real(real64) :: density

        density = Calc_DEN_3Phase(NodeBelonging, self%soil, phi1, self%water, phi2, self%ice, phi3)
    end function Calc_DEN_3Phase_Wrap

    module subroutine Update_DEN_3Phase(self, NodeBelonging, arr_phi1, arr_phi2, arr_phi3, arr_phi4)
        implicit none
        class(Type_Density_3Phase), intent(inout) :: self
        type(Belonging), intent(inout) :: NodeBelonging(:)
        real(real64), intent(in), optional :: arr_phi1(:)
        real(real64), intent(in), optional :: arr_phi2(:)
        real(real64), intent(in), optional :: arr_phi3(:)
        real(real64), intent(in), optional :: arr_phi4(:)
        integer(int32) :: iN
        real(real64) :: density_soil, density_water, density_ice

        !$omp parallel do private(iN)
        do iN = 1, self%nsize
            self%value(iN, 1) = Calc_DEN_3Phase(NodeBelonging(iN), &
                                                self%soil, &
                                                arr_phi1(iN), &
                                                self%water, &
                                                arr_phi2(iN), &
                                                self%ice, &
                                                arr_phi3(iN))
        end do
        !$omp end parallel do
    end subroutine Update_DEN_3Phase

end submodule Calculate_Density_3Phase
