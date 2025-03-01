submodule(Calculate_HCF) Calculate_HCF_Durner_Implementation
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Constructe each types by using Modified Durner model
    !----------------------------------------------------------------------------------------------------
    module function Construct_Type_HCF_Base_Durner(Ks, alpha1, n1, w1, alpha2, n2, l, nsize) result(structure_HCF)
        implicit none
        real(real64), intent(in) :: Ks
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: w1
        real(real64), intent(in) :: alpha2
        real(real64), intent(in) :: n2
        real(real64), intent(in) :: l
        integer(int32), intent(in) :: nsize
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_Durner :: structure_HCF)

        select type (this => structure_HCF)
        type is (Type_HCF_Base_Durner)
            this%Ks = Ks
            this%alpha1 = alpha1
            this%n1 = n1
            this%m1 = 1.0d0 - 1.0d0 / n1
            this%w1 = w1
            this%alpha2 = alpha2
            this%n2 = n2
            this%m2 = 1.0d0 - 1.0d0 / n2
            this%w2 = 1.0d0 - w1
            this%l = l

            call Allocate_Array(this%Kflh, nsize)
            this%Kflh(:) = 0.0d0
        end select

    end function Construct_Type_HCF_Base_Durner

    module function Construct_Type_HCF_Base_Durner_minimal() result(structure_HCF)
        implicit none
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_Durner :: structure_HCF)

    end function Construct_Type_HCF_Base_Durner_minimal

    module function Construct_Type_HCF_Base_Impedance_Durner(Ks, alpha1, n1, w1, alpha2, n2, l, Omega, nsize) result(structure_HCF)
        implicit none
        real(real64), intent(in) :: Ks
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: w1
        real(real64), intent(in) :: alpha2
        real(real64), intent(in) :: n2
        real(real64), intent(in) :: l
        real(real64), intent(in) :: Omega
        integer(int32), intent(in) :: nsize
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_Impedance_Durner :: structure_HCF)

        select type (this => structure_HCF)
        type is (Type_HCF_Base_Impedance_Durner)
            this%Ks = Ks
            this%alpha1 = alpha1
            this%n1 = n1
            this%m1 = 1.0d0 - 1.0d0 / n1
            this%w1 = w1
            this%alpha2 = alpha2
            this%n2 = n2
            this%m2 = 1.0d0 - 1.0d0 / n2
            this%w2 = 1.0d0 - w1
            this%l = l
            this%Omega = Omega

            call Allocate_Array(this%Kflh, nsize)
            this%Kflh(:) = 0.0d0
        end select

    end function Construct_Type_HCF_Base_Impedance_Durner

    module function Construct_Type_HCF_Base_Impedance_Durner_minimal() result(structure_HCF)
        implicit none
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_Impedance_Durner :: structure_HCF)

    end function Construct_Type_HCF_Base_Impedance_Durner_minimal

    module function Construct_Type_HCF_Base_Viscosity_Durner(Ks, alpha1, n1, w1, alpha2, n2, l, useViscosity, nsize) result(structure_HCF)
        implicit none
        real(real64), intent(in) :: Ks
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: w1
        real(real64), intent(in) :: alpha2
        real(real64), intent(in) :: n2
        real(real64), intent(in) :: l
        integer(int32), intent(in) :: useViscosity
        integer(int32), intent(in) :: nsize
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_Viscosity_Durner :: structure_HCF)

        select type (this => structure_HCF)
        type is (Type_HCF_Base_Viscosity_Durner)
            this%Ks = Ks
            this%alpha1 = alpha1
            this%n1 = n1
            this%m1 = 1.0d0 - 1.0d0 / n1
            this%w1 = w1
            this%alpha2 = alpha2
            this%n2 = n2
            this%m2 = 1.0d0 - 1.0d0 / n2
            this%w2 = 1.0d0 - w1
            this%l = l

            call this%Set_Calculate_Viscosity(useViscosity, this%Calculate_Viscosity)
            this%Kzero = this%Ks * this%Calculate_Viscosity(15.d0)

            call Allocate_Array(this%Kflh, nsize)
            this%Kflh(:) = 0.0d0
        end select

    end function Construct_Type_HCF_Base_Viscosity_Durner

    module function Construct_Type_HCF_Base_Viscosity_Durner_minimal() result(structure_HCF)
        implicit none
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_Viscosity_Durner :: structure_HCF)

    end function Construct_Type_HCF_Base_Viscosity_Durner_minimal

    module function Construct_Type_HCF_Base_Impedance_Viscosity_Durner(Ks, alpha1, n1, w1, alpha2, n2, l, Omega, useViscosity, nsize) result(structure_HCF)
        implicit none
        real(real64), intent(in) :: Ks
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: w1
        real(real64), intent(in) :: alpha2
        real(real64), intent(in) :: n2
        real(real64), intent(in) :: l
        real(real64), intent(in) :: Omega
        integer(int32), intent(in) :: useViscosity
        integer(int32), intent(in) :: nsize
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_Impedance_Viscosity_Durner :: structure_HCF)

        select type (this => structure_HCF)
        type is (Type_HCF_Base_Impedance_Viscosity_Durner)
            this%Ks = Ks
            this%alpha1 = alpha1
            this%n1 = n1
            this%m1 = 1.0d0 - 1.0d0 / n1
            this%w1 = w1
            this%alpha2 = alpha2
            this%n2 = n2
            this%m2 = 1.0d0 - 1.0d0 / n2
            this%w2 = 1.0d0 - w1
            this%l = l
            this%Omega = Omega

            call this%Set_Calculate_Viscosity(useViscosity, this%Calculate_Viscosity)
            this%Kzero = this%Ks * this%Calculate_Viscosity(15.d0)

            call Allocate_Array(this%Kflh, nsize)
            this%Kflh(:) = 0.0d0
        end select

    end function Construct_Type_HCF_Base_Impedance_Viscosity_Durner

    module function Construct_Type_HCF_Base_Impedance_Viscosity_Durner_minimal() result(structure_HCF)
        implicit none
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_Impedance_Viscosity_Durner :: structure_HCF)

    end function Construct_Type_HCF_Base_Impedance_Viscosity_Durner_minimal

    !----------------------------------------------------------------------------------------------------
    ! Calculate kr for Modified Durner model
    !----------------------------------------------------------------------------------------------------
    module function Calculate_kr_Durner_Base(alpha1, n1, m1, w1, alpha2, n2, m2, w2, l, h) result(kr)
        !$omp declare simd uniform(alpha1, n1, m1, w1, alpha2, n2, m2, w2, l, h)
        implicit none
        real(real64), intent(in) :: alpha1, alpha2
        real(real64), intent(in) :: n1, n2
        real(real64), intent(in) :: m1, m2
        real(real64), intent(in) :: w1, w2
        real(real64), intent(in) :: l
        real(real64), intent(in) :: h
        real(real64) :: kr
        real(real64) :: Sw1, Sw2

        if (h < 0.0d0) then
            Sw1 = (1.0d0 + (-alpha1 * h)**n1)**(-m1)
            Sw2 = (1.0d0 + (-alpha2 * h)**n2)**(-m2)
            kr = (w1 * Sw1 + w2 * Sw2)**l * &
                 (w1 * alpha1 * (1.0d0 - (1.0d0 - Sw1**(1.0d0 / m1))**m1) &
                  + w2 * alpha2 * (1.0d0 - (1.0d0 - Sw2**(1.0d0 / m2))**m2))**2.0d0 / &
                 (w1 * alpha1 + w2 * alpha2)**2.0d0
        else
            kr = 1.0d0
        end if

    end function Calculate_kr_Durner_Base

    !----------------------------------------------------------------------------------------------------
    ! Wrapper of calculating kr for Modified Durner model bounding different derived types
    !----------------------------------------------------------------------------------------------------
    module function Calculate_kr_Base_Durner(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_Durner), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = Calculate_kr_Durner_Base(self%alpha1, self%n1, self%m1, self%w1, self%alpha2, self%n2, self%m2, self%w2, self%l, h)

    end function Calculate_kr_Base_Durner

    module function Calculate_kr_Base_Impedance_Durner(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_Impedance_Durner), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = Calculate_kr_Durner_Base(self%alpha1, self%n1, self%m1, self%w1, self%alpha2, self%n2, self%m2, self%w2, self%l, h)

    end function Calculate_kr_Base_Impedance_Durner

    module function Calculate_kr_Base_Viscosity_Durner(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_Viscosity_Durner), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = Calculate_kr_Durner_Base(self%alpha1, self%n1, self%m1, self%w1, self%alpha2, self%n2, self%m2, self%w2, self%l, h)

    end function Calculate_kr_Base_Viscosity_Durner

    module function Calculate_kr_Base_Impedance_Viscosity_Durner(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_Impedance_Viscosity_Durner), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = Calculate_kr_Durner_Base(self%alpha1, self%n1, self%m1, self%w1, self%alpha2, self%n2, self%m2, self%w2, self%l, h)

    end function Calculate_kr_Base_Impedance_Viscosity_Durner

    !----------------------------------------------------------------------------------------------------
    ! Update Kflh for Modified Durner model bounding different derived types
    !----------------------------------------------------------------------------------------------------
    module function Calculate_Kflh_Base_Durner(self, h) result(Kflh)
        implicit none
        class(Type_HCF_Base_Durner), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: Kflh

        Kflh = self%Ks * self%Calculate_kr(h)

    end function Calculate_Kflh_Base_Durner

    module function Calculate_Kflh_Base_Impedance_Durner(self, h, thetaI) result(Kflh)
        implicit none
        class(Type_HCF_Base_Impedance_Durner), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(in) :: thetaI
        real(real64) :: Kflh

        Kflh = self%Ks * self%Calculate_kr(h) * self%Calculate_Impedance(self%Omega, thetaI)

    end function Calculate_Kflh_Base_Impedance_Durner

    module function Calculate_Kflh_Base_Viscosity_Durner(self, h, Temperature) result(Kflh)
        implicit none
        class(Type_HCF_Base_Viscosity_Durner), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(in) :: Temperature
        real(real64) :: Kflh

        Kflh = self%Kzero * self%Calculate_kr(h) / self%Calculate_Viscosity(Temperature)

    end function Calculate_Kflh_Base_Viscosity_Durner

    module function Calculate_Kflh_Base_Impedance_Viscosity_Durner(self, h, thetaI, Temperature) result(Kflh)
        implicit none
        class(Type_HCF_Base_Impedance_Viscosity_Durner), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(in) :: thetaI
        real(real64), intent(in) :: Temperature
        real(real64) :: Kflh

        Kflh = self%Kzero * self%Calculate_kr(h) * self%Calculate_Impedance(self%Omega, thetaI) / self%Calculate_Viscosity(Temperature)

    end function Calculate_Kflh_Base_Impedance_Viscosity_Durner

    !----------------------------------------------------------------------------------------------------
    ! Update Kflh for Modified Durner model bounding different derived types
    !----------------------------------------------------------------------------------------------------
    module subroutine Update_Kflh_Base_Durner(self, arr_h)
        implicit none
        class(Type_HCF_Base_Durner), intent(inout) :: self
        real(real64), intent(in) :: arr_h(:)

        integer(int32) :: iN, n

        n = size(arr_h(:))

        !$omp parallel do schedule(guided) private(iN)
        do iN = 1, n
            self%Kflh(iN) = self%Calculate_Kflh(arr_h(iN))
        end do

    end subroutine Update_Kflh_Base_Durner

    module subroutine Update_Kflh_Base_Impedance_Durner(self, arr_h, arr_thetaI)
        implicit none
        class(Type_HCF_Base_Impedance_Durner), intent(inout) :: self
        real(real64), intent(in) :: arr_h(:)
        real(real64), intent(in) :: arr_thetaI(:)

        integer(int32) :: iN, n

        n = size(arr_h(:))

        !$omp parallel do schedule(guided) private(iN)
        do iN = 1, n
            self%Kflh(iN) = self%Calculate_Kflh(arr_h(iN), arr_thetaI(iN))
        end do

    end subroutine Update_Kflh_Base_Impedance_Durner

    module subroutine Update_Kflh_Base_Viscosity_Durner(self, arr_h, arr_Temperature)
        implicit none
        class(Type_HCF_Base_Viscosity_Durner), intent(inout) :: self
        real(real64), intent(in) :: arr_h(:)
        real(real64), intent(in) :: arr_Temperature(:)

        integer(int32) :: iN, n

        n = size(arr_h(:))

        !$omp parallel do schedule(guided) private(iN)
        do iN = 1, n
            self%Kflh(iN) = self%Calculate_Kflh(arr_h(iN), arr_Temperature(iN))
        end do

    end subroutine Update_Kflh_Base_Viscosity_Durner

    module subroutine Update_Kflh_Base_Impedance_Viscosity_Durner(self, arr_h, arr_thetaI, arr_Temperature)
        implicit none
        class(Type_HCF_Base_Impedance_Viscosity_Durner), intent(inout) :: self
        real(real64), intent(in) :: arr_h(:)
        real(real64), intent(in) :: arr_thetaI(:)
        real(real64), intent(in) :: arr_Temperature(:)

        integer(int32) :: iN, n

        n = size(arr_h(:))

        !$omp parallel do schedule(guided) private(iN)
        do iN = 1, n
            self%Kflh(iN) = self%Calculate_Kflh(arr_h(iN), arr_thetaI(iN), arr_Temperature(iN))
        end do

    end subroutine Update_Kflh_Base_Impedance_Viscosity_Durner
end submodule Calculate_HCF_Durner_Implementation
