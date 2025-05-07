submodule(Calculate_HCF) Calculate_HCF_MVG_Implementation
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Constructe each types by using Modified van-Genuchten model
    !----------------------------------------------------------------------------------------------------
    module function Construct_Type_HCF_Base_MVG(Ks, thetaS, thetaR, alpha1, n1, l, hcrit, nsize) result(structure_HCF)
        implicit none
        real(real64), intent(in) :: Ks
        real(real64), intent(in) :: thetaS
        real(real64), intent(in) :: thetaR
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: hcrit
        real(real64), intent(in) :: l
        integer(int32), intent(in) :: nsize
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_MVG :: structure_HCF)

        select type (this => structure_HCF)
        type is (Type_HCF_Base_MVG)
            this%Ks = Ks
            this%thetaS = thetaS
            this%thetaR = thetaR
            this%alpha1 = alpha1
            this%n1 = n1
            this%m1 = 1.0d0 - 1.0d0 / n1
            this%l = l
            this%hcrit = hcrit
            this%nsize = nsize

            call Allocate_Array(this%Kflh, nsize)
            this%Kflh(:) = 0.0d0
        end select

    end function Construct_Type_HCF_Base_MVG

    module function Construct_Type_HCF_Base_MVG_minimal() result(structure_HCF)
        implicit none
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_MVG :: structure_HCF)

    end function Construct_Type_HCF_Base_MVG_minimal

    module function Construct_Type_HCF_Base_Impedance_MVG(Ks, thetaS, thetaR, alpha1, n1, l, hcrit, Omega, nsize) result(structure_HCF)
        implicit none
        real(real64), intent(in) :: Ks
        real(real64), intent(in) :: thetaS
        real(real64), intent(in) :: thetaR
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: l
        real(real64), intent(in) :: hcrit
        real(real64), intent(in) :: Omega
        integer(int32), intent(in) :: nsize
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_Impedance_MVG :: structure_HCF)

        select type (this => structure_HCF)
        type is (Type_HCF_Base_Impedance_MVG)
            this%Ks = Ks
            this%thetaS = thetaS
            this%thetaR = thetaR
            this%alpha1 = alpha1
            this%n1 = n1
            this%m1 = 1.0d0 - 1.0d0 / n1
            this%l = l
            this%hcrit = hcrit
            this%Omega = Omega
            this%nsize = nsize

            call Allocate_Array(this%Kflh, nsize)
            this%Kflh(:) = 0.0d0
        end select

    end function Construct_Type_HCF_Base_Impedance_MVG

    module function Construct_Type_HCF_Base_Impedance_MVG_minimal() result(structure_HCF)
        implicit none
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_Impedance_MVG :: structure_HCF)

    end function Construct_Type_HCF_Base_Impedance_MVG_minimal

    module function Construct_Type_HCF_Base_Viscosity_MVG(Ks, thetaS, thetaR, alpha1, n1, l, hcrit, useViscosity, nsize) result(structure_HCF)
        implicit none
        real(real64), intent(in) :: Ks
        real(real64), intent(in) :: thetaS
        real(real64), intent(in) :: thetaR
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: l
        real(real64), intent(in) :: hcrit
        integer(int32), intent(in) :: useViscosity
        integer(int32), intent(in) :: nsize
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_Viscosity_MVG :: structure_HCF)

        select type (this => structure_HCF)
        type is (Type_HCF_Base_Viscosity_MVG)
            this%Ks = Ks
            this%thetaS = thetaS
            this%thetaR = thetaR
            this%alpha1 = alpha1
            this%n1 = n1
            this%m1 = 1.0d0 - 1.0d0 / n1
            this%hcrit = hcrit
            this%l = l
            this%nsize = nsize

            call this%Set_Calculate_Viscosity(useViscosity, this%Calculate_Viscosity)
            this%Kzero = this%Ks * this%Calculate_Viscosity(15.d0)

            call Allocate_Array(this%Kflh, nsize)
            this%Kflh(:) = 0.0d0
        end select

    end function Construct_Type_HCF_Base_Viscosity_MVG

    module function Construct_Type_HCF_Base_Viscosity_MVG_minimal() result(structure_HCF)
        implicit none
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_Viscosity_MVG :: structure_HCF)

    end function Construct_Type_HCF_Base_Viscosity_MVG_minimal

    module function Construct_Type_HCF_Base_Impedance_Viscosity_MVG(Ks, thetaS, thetaR, alpha1, n1, l, hcrit, Omega, useViscosity, nsize) result(structure_HCF)
        implicit none
        real(real64), intent(in) :: Ks
        real(real64), intent(in) :: thetaS
        real(real64), intent(in) :: thetaR
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: hcrit
        real(real64), intent(in) :: l
        real(real64), intent(in) :: Omega
        integer(int32), intent(in) :: useViscosity
        integer(int32), intent(in) :: nsize
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_Impedance_Viscosity_MVG :: structure_HCF)

        select type (this => structure_HCF)
        type is (Type_HCF_Base_Impedance_Viscosity_MVG)
            this%Ks = Ks
            this%thetaS = thetaS
            this%thetaR = thetaR
            this%alpha1 = alpha1
            this%n1 = n1
            this%m1 = 1.0d0 - 1.0d0 / n1
            this%hcrit = hcrit
            this%l = l
            this%Omega = Omega
            this%nsize = nsize

            call this%Set_Calculate_Viscosity(useViscosity, this%Calculate_Viscosity)
            this%Kzero = this%Ks * this%Calculate_Viscosity(15.d0)

            call Allocate_Array(this%Kflh, nsize)
            this%Kflh(:) = 0.0d0
        end select

    end function Construct_Type_HCF_Base_Impedance_Viscosity_MVG

    module function Construct_Type_HCF_Base_Impedance_Viscosity_MVG_minimal() result(structure_HCF)
        implicit none
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_Impedance_Viscosity_MVG :: structure_HCF)

    end function Construct_Type_HCF_Base_Impedance_Viscosity_MVG_minimal

    !----------------------------------------------------------------------------------------------------
    ! Calculate kr for Modified van-Genuchten model
    !----------------------------------------------------------------------------------------------------
    module function Calculate_kr_MVG_Base(thetaS, thetaR, alpha1, n1, m1, l, hcrit, h) result(kr)
        !$omp declare simd uniform(thetaS, thetaR, alpha1, n1, m1, l, hcrit, h)
        implicit none
        real(real64), intent(in) :: thetaS
        real(real64), intent(in) :: thetaR
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: m1
        real(real64), intent(in) :: l
        real(real64), intent(in) :: hcrit
        real(real64), intent(in) :: h
        real(real64) :: kr
        real(real64) :: Sw, thetaM

        thetaM = thetaR + (thetaS - thetaR) * (1.0d0 + (-alpha1 * hcrit)**n1)**(-m1)

        if (h < hcrit) then
            Sw = (thetaS - thetaR) / (thetaM - thetaR) * (1.0d0 + abs(alpha1 * h)**n1)**(-m1)
            kr = Sw**l * ((1.0d0 - (1.0d0 - Sw**(1.0d0 / m1))**m1) / (1.0d0 - (1.0d0 - 1.0d0**(1.0d0 / m1))**m1))**2.0d0
        else
            kr = 1.0d0
        end if

    end function Calculate_kr_MVG_Base

    !----------------------------------------------------------------------------------------------------
    ! Wrapper of calculating kr for Modified van-Genuchten model bounding different derived types
    !----------------------------------------------------------------------------------------------------
    module function Calculate_kr_Base_MVG(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_MVG), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = Calculate_kr_MVG_Base(self%thetaS, self%thetaR, self%alpha1, self%n1, self%m1, self%l, self%hcrit, h)

    end function Calculate_kr_Base_MVG

    module function Calculate_kr_Base_Impedance_MVG(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_Impedance_MVG), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = Calculate_kr_MVG_Base(self%thetaS, self%thetaR, self%alpha1, self%n1, self%m1, self%l, self%hcrit, h)

    end function Calculate_kr_Base_Impedance_MVG

    module function Calculate_kr_Base_Viscosity_MVG(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_Viscosity_MVG), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = Calculate_kr_MVG_Base(self%thetaS, self%thetaR, self%alpha1, self%n1, self%m1, self%l, self%hcrit, h)

    end function Calculate_kr_Base_Viscosity_MVG

    module function Calculate_kr_Base_Impedance_Viscosity_MVG(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_Impedance_Viscosity_MVG), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = Calculate_kr_MVG_Base(self%thetaS, self%thetaR, self%alpha1, self%n1, self%m1, self%l, self%hcrit, h)

    end function Calculate_kr_Base_Impedance_Viscosity_MVG

    !----------------------------------------------------------------------------------------------------
    ! Update Kflh for Modified van-Genuchten model bounding different derived types
    !----------------------------------------------------------------------------------------------------
    module function Calculate_Kflh_Base_MVG(self, h) result(Kflh)
        implicit none
        class(Type_HCF_Base_MVG), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: Kflh

        Kflh = self%Ks * self%Calculate_kr(h)

    end function Calculate_Kflh_Base_MVG

    module function Calculate_Kflh_Base_Impedance_MVG(self, h, thetaI) result(Kflh)
        implicit none
        class(Type_HCF_Base_Impedance_MVG), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(in) :: thetaI
        real(real64) :: Kflh

        Kflh = self%Ks * self%Calculate_kr(h) * self%Calculate_Impedance(self%Omega, thetaI)

    end function Calculate_Kflh_Base_Impedance_MVG

    module function Calculate_Kflh_Base_Viscosity_MVG(self, h, Temperature) result(Kflh)
        implicit none
        class(Type_HCF_Base_Viscosity_MVG), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(in) :: Temperature
        real(real64) :: Kflh

        Kflh = self%Kzero * self%Calculate_kr(h) / self%Calculate_Viscosity(Temperature)

    end function Calculate_Kflh_Base_Viscosity_MVG

    module function Calculate_Kflh_Base_Impedance_Viscosity_MVG(self, h, thetaI, Temperature) result(Kflh)
        implicit none
        class(Type_HCF_Base_Impedance_Viscosity_MVG), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(in) :: thetaI
        real(real64), intent(in) :: Temperature
        real(real64) :: Kflh

        Kflh = self%Kzero * self%Calculate_kr(h) * self%Calculate_Impedance(self%Omega, thetaI) / self%Calculate_Viscosity(Temperature)

    end function Calculate_Kflh_Base_Impedance_Viscosity_MVG

    !----------------------------------------------------------------------------------------------------
    ! Update Kflh for Modified van-Genuchten model bounding different derived types
    !----------------------------------------------------------------------------------------------------
    module subroutine Update_Kflh_Base_MVG(self, arr_h)
        implicit none
        class(Type_HCF_Base_MVG), intent(inout) :: self
        real(real64), intent(in) :: arr_h(:)

        integer(int32) :: iN

        !$omp parallel do schedule(guided) private(iN)
        do iN = 1, self%nsize
            self%Kflh(iN) = self%Calculate_Kflh(arr_h(iN))
        end do

    end subroutine Update_Kflh_Base_MVG

    module subroutine Update_Kflh_Base_Impedance_MVG(self, arr_h, arr_thetaI)
        implicit none
        class(Type_HCF_Base_Impedance_MVG), intent(inout) :: self
        real(real64), intent(in) :: arr_h(:)
        real(real64), intent(in) :: arr_thetaI(:)

        integer(int32) :: iN

        !$omp parallel do schedule(guided) private(iN)
        do iN = 1, self%nsize
            self%Kflh(iN) = self%Calculate_Kflh(arr_h(iN), arr_thetaI(iN))
        end do

    end subroutine Update_Kflh_Base_Impedance_MVG

    module subroutine Update_Kflh_Base_Viscosity_MVG(self, arr_h, arr_Temperature)
        implicit none
        class(Type_HCF_Base_Viscosity_MVG), intent(inout) :: self
        real(real64), intent(in) :: arr_h(:)
        real(real64), intent(in) :: arr_Temperature(:)

        integer(int32) :: iN

        !$omp parallel do schedule(guided) private(iN)
        do iN = 1, self%nsize
            self%Kflh(iN) = self%Calculate_Kflh(arr_h(iN), arr_Temperature(iN))
        end do

    end subroutine Update_Kflh_Base_Viscosity_MVG

    module subroutine Update_Kflh_Base_Impedance_Viscosity_MVG(self, arr_h, arr_thetaI, arr_Temperature)
        implicit none
        class(Type_HCF_Base_Impedance_Viscosity_MVG), intent(inout) :: self
        real(real64), intent(in) :: arr_h(:)
        real(real64), intent(in) :: arr_thetaI(:)
        real(real64), intent(in) :: arr_Temperature(:)

        integer(int32) :: iN

        !$omp parallel do schedule(guided) private(iN)
        do iN = 1, self%nsize
            self%Kflh(iN) = self%Calculate_Kflh(arr_h(iN), arr_thetaI(iN), arr_Temperature(iN))
        end do

    end subroutine Update_Kflh_Base_Impedance_Viscosity_MVG

end submodule Calculate_HCF_MVG_Implementation
