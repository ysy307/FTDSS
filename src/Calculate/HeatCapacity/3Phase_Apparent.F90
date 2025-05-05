submodule(Calculate_HeatCapacity) Calc_HTC_3A
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Construct
    !----------------------------------------------------------------------------------------------------
    module function HTC_3A_Construct(Input) result(Structure)
        implicit none
        class(Abstract_HeatCapacity), allocatable :: Structure
        type(Type_Input), intent(in) :: Input

        integer(int32) :: iRegion

        allocate (Type_HeatCapacity_3Phase_Apparent :: structure)
        select type (this => structure)
        type is (Type_HeatCapacity_3Phase_Apparent)
            this%nRegion = Input%Basic%numRegion
            call Allocate_Array(this%soil, this%nRegion)
            call Allocate_Array(this%water, this%nRegion)
            call Allocate_Array(this%ice, this%nRegion)

            do iRegion = 1, this%nRegion
                this%soil(iRegion) = Input%Regions(iRegion)%Thermal%Cp(1)
                this%water(iRegion) = Input%Regions(iRegion)%Thermal%Cp(2)
                this%ice(iRegion) = Input%Regions(iRegion)%Thermal%Cp(3)
            end do

            this%nsize = Input%VTK%numPoints
            call Allocate_Array(this%value, this%nsize, 2_int32)
            this%value(:, :) = 0.0d0

        end select

    end function HTC_3A_Construct

    module function Calc_HTC_3A_Wrap(self, NodeBelonging, phi1, phi2, phi3, phi4, &
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

        Cp = Calc_HTC_3(NodeBelonging, &
                        self%soil, phi1, &
                        self%water, phi2, &
                        self%ice, phi3)
        HeatCapacity = Calc_HTC_3A(NodeBelonging, Cp, Ice, Temperature, Density, Pw)

    end function Calc_HTC_3A_Wrap

    module subroutine Update_HTC_3A(self, NodeBelonging, arr_phi1, arr_phi2, arr_phi3, arr_phi4, &
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
                self%value(iN, 1) = Calc_HTC_3(NodeBelonging(iN), &
                                               self%soil, &
                                               arr_phi1(iN), &
                                               self%water, &
                                               arr_phi2(iN), &
                                               self%ice, &
                                               arr_phi3(iN))
                self%value(iN, 2) = Calc_HTC_3A(NodeBelonging(iN), &
                                                self%value(iN, 1), &
                                                Ice, &
                                                Temperature(iN), &
                                                Density)
            end do
            ! $omp end parallel do
        else
            !$omp parallel do private(iN)
            do iN = 1, self%nsize
                self%value(iN, 1) = Calc_HTC_3(NodeBelonging(iN), &
                                               self%soil, &
                                               arr_phi1(iN), &
                                               self%water, &
                                               arr_phi2(iN), &
                                               self%ice, &
                                               arr_phi3(iN))
                self%value(iN, 2) = Calc_HTC_3A(NodeBelonging(iN), &
                                                self%value(iN, 1), &
                                                Ice, &
                                                Temperature(iN), &
                                                Density, &
                                                arr_Pw(iN))
            end do
            !$omp end parallel do
        end if

    end subroutine Update_HTC_3A

end submodule Calc_HTC_3A
