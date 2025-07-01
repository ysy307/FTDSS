submodule(Calculate_HeatCapacity) Calc_HTC_3A
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Construct
    !----------------------------------------------------------------------------------------------------
    module function HTC_3A_Construct(iRegion, Input) result(Structure)
        implicit none
        class(Abst_HeatCapacity), allocatable :: Structure
        integer(int32), intent(in) :: iRegion
        type(Type_Input), intent(in) :: Input

        if (allocated(Structure)) deallocate (Structure)
        allocate (Type_HeatCapacity_3Phase_Apparent :: Structure)
        Structure%Material1 = Input%Regions(iRegion)%Thermal%Cp(1)
        Structure%Material2 = Input%Regions(iRegion)%Thermal%Cp(2)
        Structure%Material3 = Input%Regions(iRegion)%Thermal%Cp(3)

    end function HTC_3A_Construct

    module function Calc_HTC_3A_Wrap(self, phi, Temperature, Pw, Ice, Density) result(HeatCapacity)
        implicit none
        class(Type_HeatCapacity_3Phase_Apparent), intent(in) :: self
        real(real64), intent(in) :: phi
        real(real64), intent(in) :: Temperature
        real(real64), intent(in), optional :: Pw
        class(Abstract_Ice), intent(inout), optional :: Ice
        class(Abstract_Density), intent(inout), optional :: Density
        real(real64) :: HeatCapacity

        real(real64) :: Cp, Lf
        real(real64) :: rho_i, rho_w
        real(real64) :: theta_w, theta_i, theta_s
        real(real64) :: Cp_w, Cp_i, Cp_s

        theta_s = 1.0d0 - phi
        rho_w = Density%Material2
        rho_i = Density%Material3

        select type (this => Ice)
        type is (Type_Ice_GCC)
            select type (structure_GCC => this%GCC)
            type is (Type_GCC_NonSegregation_m)
                theta_i = this%Calculate_Ice(T=Temperature, phi=phi)
                theta_w = phi - theta_i
                Cp = Calc_HTC_3(self%Material1, theta_s, &
                                self%Material2, theta_w, &
                                self%Material3, theta_i)
                HeatCapacity = Calc_HTC_3A(Cp=Cp, &
                                           Ice=Ice, &
                                           Temperature=Temperature, &
                                           rho_ice=rho_i, &
                                           rho_water=rho_w)
            type is (Type_GCC_NonSegregation_Pa)
                theta_i = this%Calculate_Ice(T=Temperature, phi=phi, rhoW=rho_w)
                theta_w = phi - theta_i
                Cp = Calc_HTC_3(self%Material1, theta_s, &
                                self%Material2, theta_w, &
                                self%Material3, theta_i)
                HeatCapacity = Calc_HTC_3A(Cp=Cp, &
                                           Ice=Ice, &
                                           Temperature=Temperature, &
                                           rho_ice=rho_i, &
                                           rho_water=rho_w)
            type is (Type_GCC_Segregation_m)
                theta_i = this%Calculate_Ice(Temperature, phi, Pw, rho_w, rho_i)
                theta_w = phi - theta_i
                Cp = Calc_HTC_3(self%Material1, theta_s, &
                                self%Material2, theta_w, &
                                self%Material3, theta_i)
                HeatCapacity = Calc_HTC_3A(Cp=Cp, &
                                           Ice=Ice, &
                                           Temperature=Temperature, &
                                           rho_ice=rho_i, &
                                           rho_water=rho_w, &
                                           Pw=Pw)
            type is (Type_GCC_Segregation_Pa)
                theta_i = this%Calculate_Ice(Temperature, phi, Pw, rho_w, rho_i)
                theta_w = phi - theta_i
                Cp = Calc_HTC_3(self%Material1, theta_s, &
                                self%Material2, theta_w, &
                                self%Material3, theta_i)
                HeatCapacity = Calc_HTC_3A(Cp=Cp, &
                                           Ice=Ice, &
                                           Temperature=Temperature, &
                                           rho_ice=rho_i, &
                                           rho_water=rho_w, &
                                           Pw=Pw)
            end select
        end select

    end function Calc_HTC_3A_Wrap

    ! module subroutine Update_HTC_3A(self, NodeBelonging, arr_phi1, arr_phi2, arr_phi3, arr_phi4, &
    !                                 Ice, Temperature, Density, arr_Pw)
    !     implicit none
    !     class(Type_HeatCapacity_3Phase_Apparent), intent(inout) :: self
    !     type(Belonging), intent(inout) :: NodeBelonging(:)
    !     real(real64), intent(in), optional :: arr_phi1(:)
    !     real(real64), intent(in), optional :: arr_phi2(:)
    !     real(real64), intent(in), optional :: arr_phi3(:)
    !     real(real64), intent(in), optional :: arr_phi4(:)
    !     class(Abstract_Ice), intent(inout), optional :: Ice
    !     real(real64), intent(in), optional :: Temperature(:)
    !     class(Abstract_Density), intent(inout), optional :: Density
    !     real(real64), intent(in), optional :: arr_Pw(:)

    !     integer(int32) :: iN
    !     real(real64) :: HeatCapacity_soil, HeatCapacity_water, HeatCapacity_ice

    !     ! if (.not. present(arr_Pw)) then
    !     !     do iN = 1, self%nsize
    !     !         self%value(iN, 1) = Calc_HTC_3(NodeBelonging(iN), &
    !     !                                        self%soil, &
    !     !                                        arr_phi1(iN), &
    !     !                                        self%water, &
    !     !                                        arr_phi2(iN), &
    !     !                                        self%ice, &
    !     !                                        arr_phi3(iN))
    !     !         self%value(iN, 2) = Calc_HTC_3A(NodeBelonging(iN), &
    !     !                                         self%value(iN, 1), &
    !     !                                         Ice, &
    !     !                                         Temperature(iN), &
    !     !                                         Density)
    !     !     end do
    !     ! else
    !     !     do iN = 1, self%nsize
    !     !         self%value(iN, 1) = Calc_HTC_3(NodeBelonging(iN), &
    !     !                                        self%soil, &
    !     !                                        arr_phi1(iN), &
    !     !                                        self%water, &
    !     !                                        arr_phi2(iN), &
    !     !                                        self%ice, &
    !     !                                        arr_phi3(iN))
    !     !         self%value(iN, 2) = Calc_HTC_3A(NodeBelonging(iN), &
    !     !                                         self%value(iN, 1), &
    !     !                                         Ice, &
    !     !                                         Temperature(iN), &
    !     !                                         Density, &
    !     !                                         arr_Pw(iN))
    !     !     end do
    !     ! end if

    ! end subroutine Update_HTC_3A

end submodule Calc_HTC_3A
