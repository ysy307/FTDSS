submodule(Calculate_HCF) Calculate_HCF_DVGCH_Implementation
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Constructe each types by using Modified van-Genuchten model
    !----------------------------------------------------------------------------------------------------
    module function Construct_Type_HCF_Base_DVGCH(Ks, alpha1, n1, w1, n2, l, nsize) result(structure_HCF)
        implicit none
        real(real64), intent(in) :: Ks
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: w1
        real(real64), intent(in) :: n2
        real(real64), intent(in) :: l
        integer(int32), intent(in) :: nsize
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_DVGCH :: structure_HCF)

        select type (this => structure_HCF)
        type is (Type_HCF_Base_DVGCH)
            this%Ks = Ks
            this%alpha1 = alpha1
            this%n1 = n1
            this%m1 = 1.0d0 - 1.0d0 / n1
            this%w1 = w1
            this%n2 = n2
            this%m2 = 1.0d0 - 1.0d0 / n2
            this%w2 = 1.0d0 - w1
            this%l = l
            this%nsize = nsize

            call Allocate_Array(this%Kflh, nsize)
            this%Kflh(:) = 0.0d0
        end select

    end function Construct_Type_HCF_Base_DVGCH

    module function Construct_Type_HCF_Base_DVGCH_minimal() result(structure_HCF)
        implicit none
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_DVGCH :: structure_HCF)

    end function Construct_Type_HCF_Base_DVGCH_minimal

    module function Construct_Type_HCF_Base_Impedance_DVGCH(Ks, alpha1, n1, w1, n2, l, Omega, nsize) result(structure_HCF)
        implicit none
        real(real64), intent(in) :: Ks
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: w1
        real(real64), intent(in) :: n2
        real(real64), intent(in) :: l
        real(real64), intent(in) :: Omega
        integer(int32), intent(in) :: nsize
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_Impedance_DVGCH :: structure_HCF)

        select type (this => structure_HCF)
        type is (Type_HCF_Base_Impedance_DVGCH)
            this%Ks = Ks
            this%alpha1 = alpha1
            this%n1 = n1
            this%m1 = 1.0d0 - 1.0d0 / n1
            this%w1 = w1
            this%n2 = n2
            this%m2 = 1.0d0 - 1.0d0 / n2
            this%w2 = 1.0d0 - w1
            this%l = l
            this%Omega = Omega
            this%nsize = nsize

            call Allocate_Array(this%Kflh, nsize)
            this%Kflh(:) = 0.0d0
        end select

    end function Construct_Type_HCF_Base_Impedance_DVGCH

    module function Construct_Type_HCF_Base_Impedance_DVGCH_minimal() result(structure_HCF)
        implicit none
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_Impedance_DVGCH :: structure_HCF)

    end function Construct_Type_HCF_Base_Impedance_DVGCH_minimal

    module function Construct_Type_HCF_Base_Viscosity_DVGCH(Ks, alpha1, n1, w1, n2, l, useViscosity, nsize) result(structure_HCF)
        implicit none
        real(real64), intent(in) :: Ks
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: w1
        real(real64), intent(in) :: n2
        real(real64), intent(in) :: l
        integer(int32), intent(in) :: useViscosity
        integer(int32), intent(in) :: nsize
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_Viscosity_DVGCH :: structure_HCF)

        select type (this => structure_HCF)
        type is (Type_HCF_Base_Viscosity_DVGCH)
            this%Ks = Ks
            this%alpha1 = alpha1
            this%n1 = n1
            this%m1 = 1.0d0 - 1.0d0 / n1
            this%w1 = w1
            this%n2 = n2
            this%m2 = 1.0d0 - 1.0d0 / n2
            this%w2 = 1.0d0 - w1
            this%l = l
            this%nsize = nsize

            call this%Set_Calculate_Viscosity(useViscosity, this%Calculate_Viscosity)
            this%Kzero = this%Ks * this%Calculate_Viscosity(15.d0)

            call Allocate_Array(this%Kflh, nsize)
            this%Kflh(:) = 0.0d0
        end select

    end function Construct_Type_HCF_Base_Viscosity_DVGCH

    module function Construct_Type_HCF_Base_Viscosity_DVGCH_minimal() result(structure_HCF)
        implicit none
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_Viscosity_DVGCH :: structure_HCF)

    end function Construct_Type_HCF_Base_Viscosity_DVGCH_minimal

    module function Construct_Type_HCF_Base_Impedance_Viscosity_DVGCH(Ks, alpha1, n1, w1, n2, l, Omega, useViscosity, nsize) result(structure_HCF)
        implicit none
        real(real64), intent(in) :: Ks
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: w1
        real(real64), intent(in) :: n2
        real(real64), intent(in) :: l
        real(real64), intent(in) :: Omega
        integer(int32), intent(in) :: useViscosity
        integer(int32), intent(in) :: nsize
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_Impedance_Viscosity_DVGCH :: structure_HCF)

        select type (this => structure_HCF)
        type is (Type_HCF_Base_Impedance_Viscosity_DVGCH)
            this%Ks = Ks
            this%alpha1 = alpha1
            this%n1 = n1
            this%m1 = 1.0d0 - 1.0d0 / n1
            this%w1 = w1
            this%n2 = n2
            this%m2 = 1.0d0 - 1.0d0 / n2
            this%w2 = 1.0d0 - w1
            this%l = l
            this%Omega = Omega
            this%nsize = nsize

            call this%Set_Calculate_Viscosity(useViscosity, this%Calculate_Viscosity)
            this%Kzero = this%Ks * this%Calculate_Viscosity(15.d0)

            call Allocate_Array(this%Kflh, nsize)
            this%Kflh(:) = 0.0d0
        end select

    end function Construct_Type_HCF_Base_Impedance_Viscosity_DVGCH

    module function Construct_Type_HCF_Base_Impedance_Viscosity_DVGCH_minimal() result(structure_HCF)
        implicit none
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_Impedance_Viscosity_DVGCH :: structure_HCF)

    end function Construct_Type_HCF_Base_Impedance_Viscosity_DVGCH_minimal

    !----------------------------------------------------------------------------------------------------
    ! Calculate kr for Modified van-Genuchten model
    !----------------------------------------------------------------------------------------------------
    module function Calculate_kr_DVGCH_Base(alpha1, n1, m1, w1, n2, m2, w2, l, h) result(kr)
        !$omp declare simd uniform(alpha1, n1, m1, w1, n2, m2, w2, l, h)
        implicit none
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1, n2
        real(real64), intent(in) :: m1, m2
        real(real64), intent(in) :: w1, w2
        real(real64), intent(in) :: l
        real(real64), intent(in) :: h
        real(real64) :: kr
        real(real64) :: Sw1, Sw2

        if (h < 0) then
            Sw1 = (1.0d0 + (-alpha1 * h)**n1)**(-m1)
            Sw2 = (1.0d0 + (-alpha1 * h)**n2)**(-m2)
            kr = (w1 * Sw1 + w2 * Sw2)**l &
                 * (w1 * alpha1 * (1.0d0 - (1.0d0 - Sw1**(1.0d0 / m1))**m1) &
                    + w2 * alpha1 * (1.0d0 - (1.0d0 - Sw2**(1.0d0 / m2))**m2))**2.0d0 &
                 / (w1 * alpha1 + w2 * alpha1)**2.0d0
        else
            kr = 1.0d0
        end if

    end function Calculate_kr_DVGCH_Base

    !----------------------------------------------------------------------------------------------------
    ! Wrapper of calculating kr for Modified van-Genuchten model bounding different derived types
    !----------------------------------------------------------------------------------------------------
    module function Calculate_kr_Base_DVGCH(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_DVGCH), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = Calculate_kr_DVGCH_Base(self%alpha1, self%n1, self%m1, self%w1, self%n2, self%m2, self%w2, self%l, h)

    end function Calculate_kr_Base_DVGCH

    module function Calculate_kr_Base_Impedance_DVGCH(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_Impedance_DVGCH), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = Calculate_kr_DVGCH_Base(self%alpha1, self%n1, self%m1, self%w1, self%n2, self%m2, self%w2, self%l, h)

    end function Calculate_kr_Base_Impedance_DVGCH

    module function Calculate_kr_Base_Viscosity_DVGCH(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_Viscosity_DVGCH), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = Calculate_kr_DVGCH_Base(self%alpha1, self%n1, self%m1, self%w1, self%n2, self%m2, self%w2, self%l, h)

    end function Calculate_kr_Base_Viscosity_DVGCH

    module function Calculate_kr_Base_Impedance_Viscosity_DVGCH(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_Impedance_Viscosity_DVGCH), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = Calculate_kr_DVGCH_Base(self%alpha1, self%n1, self%m1, self%w1, self%n2, self%m2, self%w2, self%l, h)

    end function Calculate_kr_Base_Impedance_Viscosity_DVGCH

    !----------------------------------------------------------------------------------------------------
    ! Update Kflh for Modified van-Genuchten model bounding different derived types
    !----------------------------------------------------------------------------------------------------
    module function Calculate_Kflh_Base_DVGCH(self, h) result(Kflh)
        implicit none
        class(Type_HCF_Base_DVGCH), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: Kflh

        Kflh = self%Ks * self%Calculate_kr(h)

    end function Calculate_Kflh_Base_DVGCH

    module function Calculate_Kflh_Base_Impedance_DVGCH(self, h, thetaI) result(Kflh)
        implicit none
        class(Type_HCF_Base_Impedance_DVGCH), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(in) :: thetaI
        real(real64) :: Kflh

        Kflh = self%Ks * self%Calculate_kr(h) * self%Calculate_Impedance(self%Omega, thetaI)

    end function Calculate_Kflh_Base_Impedance_DVGCH

    module function Calculate_Kflh_Base_Viscosity_DVGCH(self, h, Temperature) result(Kflh)
        implicit none
        class(Type_HCF_Base_Viscosity_DVGCH), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(in) :: Temperature
        real(real64) :: Kflh

        Kflh = self%Kzero * self%Calculate_kr(h) / self%Calculate_Viscosity(Temperature)

    end function Calculate_Kflh_Base_Viscosity_DVGCH

    module function Calculate_Kflh_Base_Impedance_Viscosity_DVGCH(self, h, thetaI, Temperature) result(Kflh)
        implicit none
        class(Type_HCF_Base_Impedance_Viscosity_DVGCH), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(in) :: thetaI
        real(real64), intent(in) :: Temperature
        real(real64) :: Kflh

        Kflh = self%Kzero * self%Calculate_kr(h) * self%Calculate_Impedance(self%Omega, thetaI) / self%Calculate_Viscosity(Temperature)

    end function Calculate_Kflh_Base_Impedance_Viscosity_DVGCH

    !----------------------------------------------------------------------------------------------------
    ! Update Kflh for Modified van-Genuchten model bounding different derived types
    !----------------------------------------------------------------------------------------------------
    module subroutine Update_Kflh_Base_DVGCH(self, arr_h)
        implicit none
        class(Type_HCF_Base_DVGCH), intent(inout) :: self
        real(real64), intent(in) :: arr_h(:)

        integer(int32) :: iN

        !$omp parallel do schedule(guided) private(iN)
        do iN = 1, self%nsize
            self%Kflh(iN) = self%Calculate_Kflh(arr_h(iN))
        end do

    end subroutine Update_Kflh_Base_DVGCH

    module subroutine Update_Kflh_Base_Impedance_DVGCH(self, arr_h, arr_thetaI)
        implicit none
        class(Type_HCF_Base_Impedance_DVGCH), intent(inout) :: self
        real(real64), intent(in) :: arr_h(:)
        real(real64), intent(in) :: arr_thetaI(:)

        integer(int32) :: iN

        !$omp parallel do schedule(guided) private(iN)
        do iN = 1, self%nsize
            self%Kflh(iN) = self%Calculate_Kflh(arr_h(iN), arr_thetaI(iN))
        end do

    end subroutine Update_Kflh_Base_Impedance_DVGCH

    module subroutine Update_Kflh_Base_Viscosity_DVGCH(self, arr_h, arr_Temperature)
        implicit none
        class(Type_HCF_Base_Viscosity_DVGCH), intent(inout) :: self
        real(real64), intent(in) :: arr_h(:)
        real(real64), intent(in) :: arr_Temperature(:)

        integer(int32) :: iN

        !$omp parallel do schedule(guided) private(iN)
        do iN = 1, self%nsize
            self%Kflh(iN) = self%Calculate_Kflh(arr_h(iN), arr_Temperature(iN))
        end do

    end subroutine Update_Kflh_Base_Viscosity_DVGCH

    module subroutine Update_Kflh_Base_Impedance_Viscosity_DVGCH(self, arr_h, arr_thetaI, arr_Temperature)
        implicit none
        class(Type_HCF_Base_Impedance_Viscosity_DVGCH), intent(inout) :: self
        real(real64), intent(in) :: arr_h(:)
        real(real64), intent(in) :: arr_thetaI(:)
        real(real64), intent(in) :: arr_Temperature(:)

        integer(int32) :: iN

        !$omp parallel do schedule(guided) private(iN)
        do iN = 1, self%nsize
            self%Kflh(iN) = self%Calculate_Kflh(arr_h(iN), arr_thetaI(iN), arr_Temperature(iN))
        end do

    end subroutine Update_Kflh_Base_Impedance_Viscosity_DVGCH
end submodule Calculate_HCF_DVGCH_Implementation
