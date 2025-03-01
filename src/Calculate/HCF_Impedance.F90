submodule(Calculate_HCF) Calculate_HCF_Impedance_Implementation
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Constructe the type
    !----------------------------------------------------------------------------------------------------
    module function Construct_Type_HCF_Impedance(Ks, Omega, nsize) result(structure_HCF)
        implicit none
        real(real64), intent(in) :: Ks
        real(real64), intent(in) :: Omega
        integer(int32), intent(in) :: nsize
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Impedance :: structure_HCF)

        select type (this => structure_HCF)
        type is (Type_HCF_Impedance)
            this%Ks = Ks
            this%Omega = Omega

            call Allocate_Array(this%Kflh, nsize)
            this%Kflh(:) = 0.0d0
        end select

    end function Construct_Type_HCF_Impedance

    module function Construct_Type_HCF_Impedance_minimal() result(structure_HCF)
        implicit none
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Impedance :: structure_HCF)

    end function Construct_Type_HCF_Impedance_minimal

    module function Construct_Type_HCF_Impedance_Viscosity(Ks, Omega, useViscosity, nsize) result(structure_HCF)
        implicit none
        real(real64), intent(in) :: Ks
        real(real64), intent(in) :: Omega
        integer(int32), intent(in) :: useViscosity
        integer(int32), intent(in) :: nsize
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Impedance_Viscosity :: structure_HCF)

        select type (this => structure_HCF)
        type is (Type_HCF_Impedance_Viscosity)
            this%Ks = Ks
            this%Omega = Omega

            call this%Set_Calculate_Viscosity(useViscosity, this%Calculate_Viscosity)
            this%Kzero = this%Ks * this%Calculate_Viscosity(15.d0)

            call Allocate_Array(this%Kflh, nsize)
            this%Kflh(:) = 0.0d0
        end select

    end function Construct_Type_HCF_Impedance_Viscosity

    module function Construct_Type_HCF_Impedance_Viscosity_minimal() result(structure_HCF)
        implicit none
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Impedance_Viscosity :: structure_HCF)

    end function Construct_Type_HCF_Impedance_Viscosity_minimal

    !----------------------------------------------------------------------------------------------------
    ! Wrapper of the function to calculate the impedance factor
    !----------------------------------------------------------------------------------------------------
    module function Calculate_Impedance_Base(Omega, thetaI) result(Impedance)
        !$omp declare simd uniform(Omega, thetaI)
        implicit none
        real(real64), intent(in) :: Omega
        real(real64), intent(in) :: thetaI
        real(real64) :: Impedance

        Impedance = 10.0d0**(-Omega * thetaI)

    end function Calculate_Impedance_Base

    !----------------------------------------------------------------------------------------------------
    ! Calculate Kflh using the impedance factor
    !----------------------------------------------------------------------------------------------------
    module function Calculate_Kflh_Impedance(self, thetaI) result(Kflh)
        implicit none
        class(Type_HCF_Impedance), intent(in) :: self
        real(real64), intent(in) :: thetaI
        real(real64) :: Kflh

        Kflh = self%Ks * self%Calculate_Impedance(self%Omega, thetaI)

    end function Calculate_Kflh_Impedance

    module function Calculate_Kflh_Impedance_Viscosity(self, thetaI, Temperature) result(Kflh)
        implicit none
        class(Type_HCF_Impedance_Viscosity), intent(in) :: self
        real(real64), intent(in) :: thetaI
        real(real64), intent(in) :: Temperature
        real(real64) :: Kflh

        Kflh = self%Ks * self%Calculate_Impedance(self%Omega, thetaI) / self%Calculate_Viscosity(Temperature)

    end function Calculate_Kflh_Impedance_Viscosity

    !----------------------------------------------------------------------------------------------------
    ! Update Kflh using the impedance factor
    !----------------------------------------------------------------------------------------------------
    module subroutine Update_Kflh_Impedance(self, arr_thetaI)
        implicit none
        class(Type_HCF_Impedance), intent(inout) :: self
        real(real64), intent(in) :: arr_thetaI(:)

        integer(int32) :: iN, n

        n = size(arr_thetaI(:))

        !$omp parallel do schedule(guided) private(iN)
        do iN = 1, n
            self%Kflh(iN) = self%Calculate_Kflh(arr_thetaI(iN))
        end do

    end subroutine Update_Kflh_Impedance

    module subroutine Update_Kflh_Impedance_Viscosity(self, arr_thetaI, arr_Temperature)
        implicit none
        class(Type_HCF_Impedance_Viscosity), intent(inout) :: self
        real(real64), intent(in) :: arr_thetaI(:)
        real(real64), intent(in) :: arr_Temperature(:)

        integer(int32) :: iN, n

        n = size(arr_Temperature(:))

        !$omp parallel do schedule(guided) private(iN)
        do iN = 1, n
            self%Kflh(iN) = self%Calculate_Kflh(arr_thetaI(iN), arr_Temperature(iN))
        end do

    end subroutine Update_Kflh_Impedance_Viscosity

end submodule Calculate_HCF_Impedance_Implementation
