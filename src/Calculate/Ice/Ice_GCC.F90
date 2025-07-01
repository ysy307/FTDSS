submodule(Calculate_Ice) Calculate_Ice_GCC
    implicit none
contains
    module function Type_Ice_GCC_Construct(Input, nsize) result(Structure)
        implicit none
        type(Input_Region), intent(inout) :: Input
        integer(int32), intent(in) :: nsize
        class(Abstract_Ice), allocatable :: Structure

        if (allocated(Structure)) deallocate (Structure)
        allocate (Type_Ice_GCC :: Structure)

        select type (this => Structure)
        type is (Type_Ice_GCC)
            select case (Input%Ice%ModelType)
            case (1)
                this%WRF = Type_WRF_BC(Input)
            case (2)
                this%WRF = Type_WRF_VG(Input)
            case (3)
                this%WRF = Type_WRF_KO(Input)
            case (4)
                this%WRF = Type_WRF_MVG(Input)
            case (5)
                this%WRF = Type_WRF_Durner(Input)
            case (6)
                this%WRF = Type_WRF_DVGCH(Input)
            case default
                stop 'Invalid ModelType'
            end select

            if (Input%Ice%isSegregation) then
                select case (Input%Ice%c_unit)
                case ('m')
                    this%GCC = Type_GCC_Segregation_m(Input%Ice%Tf, Input%Thermal%LatentHeat)
                case ("Pa")
                    this%GCC = Type_GCC_Segregation_Pa(Input%Ice%Tf, Input%Thermal%LatentHeat)
                end select
            else
                select case (Input%Ice%c_unit)
                case ('m')
                    this%GCC = Type_GCC_NonSegregation_m(Input%Ice%Tf, Input%Thermal%LatentHeat)
                case ("Pa")
                    this%GCC = Type_GCC_NonSegregation_Pa(Input%Ice%Tf, Input%Thermal%LatentHeat)
                end select
            end if

            this%nsize = nsize

        end select

    end function Type_Ice_GCC_Construct

    module function Calculate_Ice_GCC(self, T, phi, Pw, rhoW, rhoI) result(Qice)
        implicit none
        class(Type_Ice_GCC), intent(inout) :: self
        real(real64), intent(in), optional :: T
        real(real64), intent(in), optional :: phi
        real(real64), intent(in), optional :: Pw
        real(real64), intent(in), optional :: rhoW
        real(real64), intent(in), optional :: rhoI
        real(real64) :: Qice

        select type (GCC => self%GCC)
        type is (Type_GCC_NonSegregation_m)
            Qice = phi - self%WRF%Calc(-GCC%Calc(T=T))
        type is (Type_GCC_NonSegregation_Pa)
            Qice = phi - self%WRF%Calc(-GCC%Calc(T=T, rhoW=rhoW))
        type is (Type_GCC_Segregation_m)
            Qice = phi - self%WRF%Calc(-GCC%Calc(T=T, Pw=Pw, rhoW=rhoW, rhoI=rhoI))
        type is (Type_GCC_Segregation_Pa)
            Qice = phi - self%WRF%Calc(-GCC%Calc(T=T, Pw=Pw, rhoW=rhoW, rhoI=rhoI))
        end select

    end function Calculate_Ice_GCC

    module function Calculate_Ice_GCC_Derivative(self, T, phi, Pw, rhoW, rhoI) result(D_Qice)
        implicit none
        class(Type_Ice_GCC), intent(inout) :: self
        real(real64), intent(in), optional :: T
        real(real64), intent(in), optional :: phi
        real(real64), intent(in), optional :: Pw
        real(real64), intent(in), optional :: rhoW
        real(real64), intent(in), optional :: rhoI
        real(real64) :: D_Qice

        select type (GCC => self%GCC)
        type is (Type_GCC_NonSegregation_m)
            D_Qice = -self%WRF%DERIV(-self%GCC%Calc(T=T)) &
                     * self%GCC%DERIV(T=T)
        type is (Type_GCC_NonSegregation_Pa)
            D_Qice = -self%WRF%DERIV(-self%GCC%Calc(T=T, rhoW=rhoW)) &
                     * self%GCC%DERIV(T=T, rhoW=rhoW)
        type is (Type_GCC_Segregation_m)
            D_Qice = -self%WRF%DERIV(-self%GCC%Calc(T=T, Pw=Pw, rhoW=rhoW, rhoI=rhoI)) &
                     * self%GCC%DERIV(T=T, Pw=Pw, rhoW=rhoW, rhoI=rhoI)
        type is (Type_GCC_Segregation_Pa)
            D_Qice = self%WRF%DERIV(-self%GCC%Calc(T=T, Pw=Pw, rhoW=rhoW, rhoI=rhoI)) &
                     * self%GCC%DERIV(T=T, Pw=Pw, rhoW=rhoW, rhoI=rhoI)
        end select

    end function Calculate_Ice_GCC_Derivative

    module subroutine Update_Ice_GCC(self, NodeBelonging, arr_T, arr_phi, arr_Pw, Density, arr_Cp, arr_Qw, arr_Qice, arr_Si)
        implicit none
        class(Type_Ice_GCC), intent(inout) :: self
        type(Belonging), intent(inout), optional :: NodeBelonging(:)
        real(real64), intent(inout), optional :: arr_T(:)
        real(real64), intent(in), optional :: arr_phi(:)
        real(real64), intent(in), optional :: arr_Pw(:)
        class(Abstract_Density), intent(in), optional :: Density
        real(real64), intent(in) :: arr_Cp(:)
        real(real64), intent(inout), optional :: arr_Qw(:)
        real(real64), intent(inout), optional :: arr_Qice(:)
        type(Variables), intent(inout), optional :: arr_Si

        real(real64) :: rhoW, rhoI
        integer(int32) :: iN

        if (.not. present(arr_Pw)) then
            do iN = 1, self%nsize
                select type (DEN => Density)
                type is (Type_Density_3Phase)
                    rhoW = NodeBelonging(iN)%value(DEN%water)
                    rhoI = NodeBelonging(iN)%value(DEN%ice)
                end select
                arr_Qw(iN) = self%Calculate_Ice(T=arr_T(iN), phi=arr_phi(iN), rhoW=rhoW, rhoI=rhoI)
                arr_Qice(iN) = arr_phi(iN) - arr_Qw(iN)
            end do
        else
            do iN = 1, self%nsize
                select type (DEN => Density)
                type is (Type_Density_3Phase)
                    rhoW = NodeBelonging(iN)%value(DEN%water)
                    rhoI = NodeBelonging(iN)%value(DEN%ice)
                end select
                arr_Qice(iN) = self%Calculate_Ice(arr_T(iN), arr_phi(iN), arr_Pw(iN), rhoW, rhoI)
                arr_Qw(iN) = arr_phi(iN) - arr_Qice(iN)
            end do
        end if

    end subroutine Update_Ice_GCC

    module subroutine Update_Ice_GCC_Derivative(self, NodeBelonging, arr_T, arr_phi, arr_Pw, Density, arr_Dice)
        implicit none
        class(Type_Ice_GCC), intent(inout) :: self
        type(Belonging), intent(inout), optional :: NodeBelonging(:)
        real(real64), intent(in), optional :: arr_T(:)
        real(real64), intent(in), optional :: arr_phi(:)
        real(real64), intent(in), optional :: arr_Pw(:)
        class(Abstract_Density), intent(in), optional :: Density
        real(real64), intent(inout), optional :: arr_Dice(:)

        real(real64) :: rhoW, rhoI
        integer(int32) :: iN

        do iN = 1, self%nsize
            select type (DEN => Density)
            type is (Type_Density_3Phase)
                rhoW = NodeBelonging(iN)%value(DEN%water)
                rhoI = NodeBelonging(iN)%value(DEN%ice)
            end select
            arr_Dice(iN) = self%Calculate_Ice_Derivative(arr_T(iN), arr_phi(iN), arr_Pw(iN), rhoW, rhoI)
        end do

    end subroutine Update_Ice_GCC_Derivative

end submodule Calculate_Ice_GCC
