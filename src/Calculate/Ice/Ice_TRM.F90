submodule(Calculate_Ice) Calculate_Ice_TRM_Implementation
    use, intrinsic :: iso_fortran_env, only: int32, real64
    implicit none

contains
    module function Construct_Type_Ice_TRM(Input, nsize) result(structure)
        implicit none
        type(Input_Region), intent(inout) :: Input
        integer(int32), intent(in) :: nsize
        class(Abstract_Ice), allocatable :: structure

        if (allocated(structure)) deallocate (structure)
        allocate (Type_Ice_TRM :: structure)

        select type (this => structure)
        type is (Type_Ice_TRM)
            this%Lf = Input%Thermal%LatentHeat
            this%Tf = Input%Ice%Tf
            this%nsize = nsize
        end select

    end function Construct_Type_Ice_TRM

    module function Calculate_Ice_TRM(self, T, phi, Pw, rhoW, rhoI) result(Qice)
        implicit none
        class(Type_Ice_TRM), intent(inout) :: self
        real(real64), intent(in), optional :: T
        real(real64), intent(in), optional :: phi
        real(real64), intent(in), optional :: Pw
        real(real64), intent(in), optional :: rhoW
        real(real64), intent(in), optional :: rhoI
        real(real64) :: Qice

        !!POSITIVE INF
        Qice = transfer(Z'7FF8000000000000', 0.0_real64)

    end function Calculate_Ice_TRM

    module function Calculate_Ice_TRM_Derivative(self, T, phi, Pw, rhoW, rhoI) result(D_Qice)
        implicit none
        class(Type_Ice_TRM), intent(inout) :: self
        real(real64), intent(in), optional :: T
        real(real64), intent(in), optional :: phi
        real(real64), intent(in), optional :: Pw
        real(real64), intent(in), optional :: rhoW
        real(real64), intent(in), optional :: rhoI
        real(real64) :: D_Qice

        !!POSITIVE INF
        D_Qice = transfer(Z'7FF8000000000000', 0.0_real64)

    end function Calculate_Ice_TRM_Derivative

    module subroutine Update_Ice_TRM(self, NodeBelonging, arr_T, arr_phi, arr_Pw, Density, arr_Cp, arr_Qw, arr_Qice, arr_Si)
        implicit none
        class(Type_Ice_TRM), intent(inout) :: self
        type(Belonging), intent(inout), optional :: NodeBelonging(:)
        real(real64), intent(inout), optional :: arr_T(:)
        real(real64), intent(in), optional :: arr_phi(:)
        real(real64), intent(in), optional :: arr_Pw(:)
        class(Abstract_Density), intent(in), optional :: Density
        real(real64), intent(in) :: arr_Cp(:)
        real(real64), intent(inout), optional :: arr_Qw(:)
        real(real64), intent(inout), optional :: arr_Qice(:)
        type(Variables), intent(inout), optional :: arr_Si

        integer(int32) :: iN
        real(real64) :: C, tmpSi, rhoW

        !$omp parallel do schedule(guided) private(iN, C, tmpSi, rhoW)
        do iN = 1, self%nsize
            select type (DEN => Density)
            type is (Type_Density_3Phase)
                rhoW = NodeBelonging(iN)%value(DEN%water)
            end select
            C = arr_Cp(iN) / (arr_Qw(iN) * rhoW * self%Lf)
            tmpSi = arr_Si%old(iN, 1) + C * (self%Tf - arr_T(iN))

            if (tmpSi <= 0.0d0 .and. arr_Si%old(iN, 1) == 0.0d0) then
                arr_Si%new(iN) = 0.0d0
            else if (tmpSi >= 1.0d0 .and. arr_Si%old(iN, 1) == 1.0d0) then
                arr_Si%new(iN) = 1.0d0
            else if (0.0d0 < tmpSi .and. tmpSi < 1.0d0 .and. arr_Si%old(iN, 1) <= 1.0d0) then
                arr_T(iN) = self%Tf
                arr_Si%new(iN) = tmpSi
            else if (0.0d0 < arr_Si%old(iN, 1) .and. arr_Si%old(iN, 1) < 1.0d0 .and. tmpSi >= 1.0d0) then
                arr_T(iN) = self%Tf + (1.0d0 - tmpSi) / C
                arr_Si%new(iN) = 1.0d0
            end if
        end do

    end subroutine Update_Ice_TRM

    module subroutine Update_Ice_TRM_Derivative(self, NodeBelonging, arr_T, arr_phi, arr_Pw, Density, arr_Dice)
        implicit none
        class(Type_Ice_TRM), intent(inout) :: self
        type(Belonging), intent(inout), optional :: NodeBelonging(:)
        real(real64), intent(in), optional :: arr_T(:)
        real(real64), intent(in), optional :: arr_phi(:)
        real(real64), intent(in), optional :: arr_Pw(:)
        class(Abstract_Density), intent(in), optional :: Density
        real(real64), intent(inout), optional :: arr_Dice(:)

        arr_Dice(:) = transfer(Z'7FF8000000000000', 0.0_real64)

    end subroutine Update_Ice_TRM_Derivative

end submodule Calculate_Ice_TRM_Implementation
