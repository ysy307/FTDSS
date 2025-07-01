submodule(Calculate_HeatCapacity) Calc_HTC_Base
    implicit none
contains
    module subroutine HTCHolder_allocate(self, iRegion, Input)
        implicit none
        class(HTCHolder), intent(inout) :: self
        integer(int32), intent(in) :: iRegion
        type(Type_Input), intent(in) :: Input

        if (Input%Regions(iRegion)%Flag%is3Phase .and. &
            Input%Regions(iRegion)%isFrozen) then
            self%c = Type_HeatCapacity_3Phase_Apparent(iRegion, Input)
        end if

    end subroutine HTCHolder_allocate

    module function Calc_HTC_3(HeatCapacity_soil, phi_soil, HeatCapacity_water, phi_water, HeatCapacity_ice, phi_ice) result(HeatCapacity)
        implicit none
        real(real64), intent(in) :: HeatCapacity_soil
        real(real64), intent(in) :: phi_soil
        real(real64), intent(in) :: HeatCapacity_water
        real(real64), intent(in) :: phi_water
        real(real64), intent(in) :: HeatCapacity_ice
        real(real64), intent(in) :: phi_ice
        real(real64) :: HeatCapacity

        HeatCapacity = HeatCapacity_soil * phi_soil &
                       + HeatCapacity_water * phi_water &
                       + HeatCapacity_ice * phi_ice

    end function Calc_HTC_3

    module function Calc_HTC_3A(Cp, Ice, Temperature, rho_ice, rho_water, Pw, phi) result(HeatCapacity)
        implicit none
        real(real64), intent(in) :: Cp
        class(Abstract_Ice), intent(inout), optional :: Ice
        real(real64), intent(in), optional :: Temperature
        real(real64), intent(in) :: rho_ice, rho_water
        real(real64), intent(in), optional :: Pw
        real(real64), intent(in), optional :: phi
        real(real64) :: HeatCapacity

        real(real64) :: Lf

        select type (this => Ice)
        type is (Type_Ice_GCC)
            Lf = this%GCC%Lf

            select type (structure_GCC => this%GCC)
            type is (Type_GCC_NonSegregation_m)
                HeatCapacity = Cp - Lf * rho_ice * this%Calculate_Ice_Derivative(T=Temperature)
            type is (Type_GCC_NonSegregation_Pa)
                HeatCapacity = Cp - Lf * rho_ice * this%Calculate_Ice_Derivative(T=Temperature, rhoW=rho_water)
            type is (Type_GCC_Segregation_m)
                HeatCapacity = Cp - Lf * rho_ice * this%Calculate_Ice_Derivative(T=Temperature, Pw=Pw, rhoW=rho_water, rhoI=rho_ice)
            type is (Type_GCC_Segregation_Pa)
                HeatCapacity = Cp - Lf * rho_ice * this%Calculate_Ice_Derivative(T=Temperature, Pw=Pw, rhoW=rho_water, rhoI=rho_ice)
            end select
        type is (Type_Ice_EXP)
            Lf = this%Lf
            HeatCapacity = Cp - Lf * rho_ice * this%Calculate_Ice_Derivative(T=Temperature, phi=phi)
        end select

    end function Calc_HTC_3A

    ! function Calc_HTC_3A2(NodeBelonging, HeatCapacity_soil, HeatCapacity_water, HeatCapacity_ice, phi, Ice, Temperature, Density, Pw) result(HeatCapacity)
    !     implicit none
    !     type(Belonging), intent(inout) :: NodeBelonging
    !     real(real64), intent(in) :: phi
    !     class(Abstract_Ice), intent(inout), optional :: Ice
    !     real(real64), intent(in), optional :: Temperature
    !     class(Abstract_Density), intent(inout), optional :: Density
    !     real(real64), intent(in), optional :: Pw
    !     real(real64) :: HeatCapacity

    !     real(real64) :: Lf
    !     real(real64) :: rho_ice, rho_water

    !     select type (Den => Density)
    !     type is (Type_Density_3Phase)
    !         rho_ice = NodeBelonging%value(Den%ice)
    !         rho_water = NodeBelonging%value(Den%water)
    !     end select

    !     ! select type (this => Ice)
    !     ! type is (Type_Ice_GCC)
    !     !     Lf = this%GCC%Lf

    !     !     select type (structure_GCC => this%GCC)
    !     !     type is (Type_GCC_NonSegregation_m)
    !     !         HeatCapacity = Cp - Lf * rho_ice * this%Calculate_Ice_Derivative(Temperature)
    !     !     type is (Type_GCC_NonSegregation_Pa)
    !     !         HeatCapacity = Cp - Lf * rho_ice * this%Calculate_Ice_Derivative(Temperature, rho_water)
    !     !     type is (Type_GCC_Segregation_m)
    !     !         HeatCapacity = Cp - Lf * rho_ice * this%Calculate_Ice_Derivative(Temperature, Pw, rho_water)
    !     !     type is (Type_GCC_Segregation_Pa)
    !     !         HeatCapacity = Cp - Lf * rho_ice * this%Calculate_Ice_Derivative(Temperature, Pw, rho_water)
    !     !     end select
    !     ! type is (Type_Ice_EXP)
    !     !     Lf = this%Lf
    !     !     HeatCapacity = Cp - Lf * rho_ice * this%Calculate_Ice_Derivative(Temperature)
    !     ! end select

    ! end function Calc_HTC_3A2

end submodule Calc_HTC_Base
