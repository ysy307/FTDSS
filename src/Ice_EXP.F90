submodule(Calculate_Ice) Calculate_Ice_EXP_Implementation
    use, intrinsic :: iso_fortran_env, only: int32, real64
    implicit none
contains
    module function Type_Ice_EXP_Construct(Input, nsize) result(Structure)
        implicit none
        type(Input_Region), intent(inout) :: Input
        integer(int32), intent(in) :: nsize
        class(Abstract_Ice), allocatable :: Structure

        if (allocated(Structure)) deallocate (Structure)
        allocate (Type_Ice_EXP :: structure)

        select type (this => structure)
        type is (Type_Ice_EXP)
            this%Lf = Input%Thermal%LatentHeat
            this%Tf = Input%Ice%Tf
            this%a = Input%Ice%a
            this%nsize = nsize
        end select

    end function Type_Ice_EXP_Construct

    module function Calculate_Ice_EXP(self, T, phi, Pw, rhoW, rhoI) result(Qice)
        implicit none
        class(Type_Ice_EXP), intent(inout) :: self
        real(real64), intent(in), optional :: T
        real(real64), intent(in), optional :: phi
        real(real64), intent(in), optional :: Pw
        real(real64), intent(in), optional :: rhoW
        real(real64), intent(in), optional :: rhoI
        real(real64) :: Qice

        if (T <= self%Tf) then
            Qice = phi * (1.0d0 - (1.0d0 - T + self%Tf)**self%a)
        else
            Qice = 0.0d0
        end if

    end function Calculate_Ice_EXP

    module function Calculate_Ice_EXP_Derivative(self, T, phi, Pw, rhoW, rhoI) result(D_Qice)
        implicit none
        class(Type_Ice_EXP), intent(inout) :: self
        real(real64), intent(in), optional :: T
        real(real64), intent(in), optional :: phi
        real(real64), intent(in), optional :: Pw
        real(real64), intent(in), optional :: rhoW
        real(real64), intent(in), optional :: rhoI
        real(real64) :: D_Qice

        if (T <= self%Tf) then
            D_Qice = phi * self%a * (1.0d0 - T + self%Tf)**(self%a - 1.0d0)
        else
            D_Qice = 0.0d0
        end if

    end function Calculate_Ice_EXP_Derivative

    module subroutine Update_Ice_EXP(self, NodeBelonging, arr_T, arr_phi, arr_Pw, Density, arr_Cp, arr_Qw, arr_Qice, arr_Si)
        implicit none
        class(Type_Ice_EXP), intent(inout) :: self
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

        !$omp parallel do schedule(guided) private(iN)
        do iN = 1, self%nsize
            if (arr_T(iN) < self%Tf) then
                arr_Qice(iN) = self%Calculate_Ice(arr_T(iN), arr_phi(iN))
            else
                arr_Qice(iN) = 0.0d0
            end if
            arr_Qw(iN) = arr_phi(iN) - arr_Qice(iN)
        end do

    end subroutine Update_Ice_EXP

    module subroutine Update_Ice_EXP_Derivative(self, NodeBelonging, arr_T, arr_phi, arr_Pw, Density, arr_Dice)
        implicit none
        class(Type_Ice_EXP), intent(inout) :: self
        type(Belonging), intent(inout), optional :: NodeBelonging(:)
        real(real64), intent(in), optional :: arr_T(:)
        real(real64), intent(in), optional :: arr_phi(:)
        real(real64), intent(in), optional :: arr_Pw(:)
        class(Abstract_Density), intent(in), optional :: Density
        real(real64), intent(inout), optional :: arr_Dice(:)

        integer(int32) :: iN

        !$omp parallel do schedule(guided) private(iN)
        do iN = 1, self%nsize
            if (arr_T(iN) < self%Tf) then
                arr_Dice(iN) = self%Calculate_Ice_Derivative(arr_T(iN), arr_phi(iN))
            else
                arr_Dice(iN) = 0.0d0
            end if
        end do

    end subroutine Update_Ice_EXP_Derivative

end submodule Calculate_Ice_EXP_Implementation
