submodule(Calculate_HeatCapacity) Calculate_HeatCapacity_3Phase_Apparent
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Construct
    !----------------------------------------------------------------------------------------------------
    module function HTC_3Phase_Apparent_Construct(Input) result(structure)
        implicit none
        type(Type_Input), intent(in) :: Input
        class(Abstract_HeatCapacity), allocatable :: structure

        integer(int32) :: iRegion

        allocate (Type_HeatCapacity_3Phase_Apparent :: structure)
        select type (this => structure)
        type is (Type_HeatCapacity_3Phase_Apparent)
            this%numRegion = Input%Basic%numRegion
            call Allocate_Array(this%soil, this%numRegion)
            call Allocate_Array(this%water, this%numRegion)
            call Allocate_Array(this%ice, this%numRegion)

            do iRegion = 1, this%numRegion
                this%soil(iRegion) = Input%Regions(iRegion)%Thermal%Cp(1)
                this%water(iRegion) = Input%Regions(iRegion)%Thermal%Cp(2)
                this%ice(iRegion) = Input%Regions(iRegion)%Thermal%Cp(3)
            end do

            this%nsize = Input%VTK%numPoints
            call Allocate_Array(this%value, this%nsize, 2_int32)
            this%value(:, :) = 0.0d0

        end select

    end function HTC_3Phase_Apparent_Construct

    function Calc_HTC_3Phase(NodeBelonging, HeatCapacity_soil, phi_soil, &
                             HeatCapacity_water, phi_water, HeatCapacity_ice, phi_ice) result(HeatCapacity)
        implicit none
        type(Belonging), intent(inout) :: NodeBelonging
        real(real64), intent(in) :: HeatCapacity_soil(:)
        real(real64), intent(in) :: phi_soil
        real(real64), intent(in) :: HeatCapacity_water(:)
        real(real64), intent(in) :: phi_water
        real(real64), intent(in) :: HeatCapacity_ice(:)
        real(real64), intent(in) :: phi_ice
        real(real64) :: HeatCapacity

        real(real64) :: val_HeatCapacity_soil, val_HeatCapacity_water, val_HeatCapacity_ice

        val_HeatCapacity_ice = NodeBelonging%value(HeatCapacity_ice)
        val_HeatCapacity_soil = NodeBelonging%value(HeatCapacity_soil)
        val_HeatCapacity_water = NodeBelonging%value(HeatCapacity_water)

        HeatCapacity = val_HeatCapacity_soil * phi_soil &
                       + val_HeatCapacity_water * phi_water &
                       + val_HeatCapacity_ice * phi_ice

    end function Calc_HTC_3Phase

    function Calc_HTC_3Phase_Apparent(NodeBelonging, Cp, Ice, Temperature, Density, Pw) result(HeatCapacity)
        implicit none
        type(Belonging), intent(inout) :: NodeBelonging
        real(real64), intent(in) :: Cp
        class(Abstract_Ice), intent(inout), optional :: Ice
        real(real64), intent(in), optional :: Temperature
        class(Abstract_Density), intent(inout), optional :: Density
        real(real64), intent(in), optional :: Pw
        real(real64) :: HeatCapacity

        real(real64) :: Lf
        real(real64) :: rho_ice, rho_water

        select type (Den => Density)
        type is (Type_Density_3Phase)
            rho_ice = NodeBelonging%value(Den%ice)
            rho_water = NodeBelonging%value(Den%water)
        end select

        select type (this => Ice)
        type is (Type_Ice_GCC)
            Lf = this%GCC%Lf

            select type (structure_GCC => this%GCC)
            type is (Type_GCC_NonSegregation_m)
                HeatCapacity = Cp - Lf * rho_ice * this%Calculate_Ice_Derivative(Temperature)
            type is (Type_GCC_NonSegregation_Pa)
                HeatCapacity = Cp - Lf * rho_ice * this%Calculate_Ice_Derivative(Temperature, rho_water)
            type is (Type_GCC_Segregation_m)
                HeatCapacity = Cp - Lf * rho_ice * this%Calculate_Ice_Derivative(Temperature, Pw, rho_water)
            type is (Type_GCC_Segregation_Pa)
                HeatCapacity = Cp - Lf * rho_ice * this%Calculate_Ice_Derivative(Temperature, Pw, rho_water)
            end select
        type is (Type_Ice_EXP)
            Lf = this%Lf
            HeatCapacity = Cp - Lf * rho_ice * this%Calculate_Ice_Derivative(Temperature)
        end select

    end function Calc_HTC_3Phase_Apparent

    module function Calc_HTC_3Phase_Wrap(self, NodeBelonging, phi1, phi2, phi3, phi4, &
                                         Ice, Temperature, Density, Pw) result(HeatCapacity)
        implicit none
        class(Type_HeatCapacity_3Phase_Apparent), intent(in) :: self
        type(Belonging), intent(inout) :: NodeBelonging
        real(real64), intent(in), optional :: phi1
        real(real64), intent(in), optional :: phi2
        real(real64), intent(in), optional :: phi3
        real(real64), intent(in), optional :: phi4
        class(Abstract_Ice), intent(inout), optional :: Ice
        real(real64), intent(in), optional :: Temperature
        class(Abstract_Density), intent(inout), optional :: Density
        real(real64), intent(in), optional :: Pw
        real(real64) :: HeatCapacity

        real(real64) :: Cp, Lf
        real(real64) :: rho_ice, rho_water

        Cp = Calc_HTC_3Phase(NodeBelonging, self%soil, phi1, self%water, phi2, self%ice, phi3)
        HeatCapacity = Calc_HTC_3Phase_Apparent(NodeBelonging, Cp, Ice, Temperature, Density, Pw)

    end function Calc_HTC_3Phase_Wrap

    module subroutine Update_HTC_3Phase(self, NodeBelonging, arr_phi1, arr_phi2, arr_phi3, arr_phi4, &
                                        Ice, Temperature, Density, arr_Pw)
        implicit none
        class(Type_HeatCapacity_3Phase_Apparent), intent(inout) :: self
        type(Belonging), intent(inout) :: NodeBelonging(:)
        real(real64), intent(in), optional :: arr_phi1(:)
        real(real64), intent(in), optional :: arr_phi2(:)
        real(real64), intent(in), optional :: arr_phi3(:)
        real(real64), intent(in), optional :: arr_phi4(:)
        class(Abstract_Ice), intent(inout), optional :: Ice
        real(real64), intent(in), optional :: Temperature(:)
        class(Abstract_Density), intent(inout), optional :: Density
        real(real64), intent(in), optional :: arr_Pw(:)

        integer(int32) :: iN
        real(real64) :: HeatCapacity_soil, HeatCapacity_water, HeatCapacity_ice

        if (.not. present(arr_Pw)) then
            do iN = 1, self%nsize
                self%value(iN, 1) = Calc_HTC_3Phase(NodeBelonging(iN), &
                                                    self%soil, &
                                                    arr_phi1(iN), &
                                                    self%water, &
                                                    arr_phi2(iN), &
                                                    self%ice, &
                                                    arr_phi3(iN))
                self%value(iN, 2) = Calc_HTC_3Phase_Apparent(NodeBelonging(iN), &
                                                             self%value(iN, 1), &
                                                             Ice, &
                                                             Temperature(iN), &
                                                             Density)
            end do
            ! $omp end parallel do
        else
            !$omp parallel do private(iN)
            do iN = 1, self%nsize
                self%value(iN, 1) = Calc_HTC_3Phase(NodeBelonging(iN), &
                                                    self%soil, &
                                                    arr_phi1(iN), &
                                                    self%water, &
                                                    arr_phi2(iN), &
                                                    self%ice, &
                                                    arr_phi3(iN))
                self%value(iN, 2) = Calc_HTC_3Phase_Apparent(NodeBelonging(iN), &
                                                             self%value(iN, 1), &
                                                             Ice, &
                                                             Temperature(iN), &
                                                             Density, &
                                                             arr_Pw(iN))
            end do
            !$omp end parallel do
        end if

    end subroutine Update_HTC_3Phase

end submodule Calculate_HeatCapacity_3Phase_Apparent
