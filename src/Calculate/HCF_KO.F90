submodule(Calculate_HCF) Calculate_HCF_KO_Implementation
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Constructe each types by using Kosugi model
    !----------------------------------------------------------------------------------------------------
    module function Construct_Type_HCF_Base_KO(Ks, alpha1, n1, l, nsize) result(structure_HCF)
        implicit none
        real(real64), intent(in) :: Ks
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: l
        integer(int32), intent(in) :: nsize
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_KO :: structure_HCF)

        select type (this => structure_HCF)
        type is (Type_HCF_Base_KO)
            this%Ks = Ks
            this%alpha1 = alpha1
            this%n1 = n1
            this%l = l

            call Allocate_Array(this%Kflh, nsize)
            this%Kflh(:) = 0.0d0
        end select

    end function Construct_Type_HCF_Base_KO

    module function Construct_Type_HCF_Base_KO_minimal() result(structure_HCF)
        implicit none
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_KO :: structure_HCF)

    end function Construct_Type_HCF_Base_KO_minimal

    module function Construct_Type_HCF_Base_Impedance_KO(Ks, alpha1, n1, l, Omega, nsize) result(structure_HCF)
        implicit none
        real(real64), intent(in) :: Ks
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: l
        real(real64), intent(in) :: Omega
        integer(int32), intent(in) :: nsize
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_Impedance_KO :: structure_HCF)

        select type (this => structure_HCF)
        type is (Type_HCF_Base_Impedance_KO)
            this%Ks = Ks
            this%alpha1 = alpha1
            this%n1 = n1
            this%l = l
            this%Omega = Omega

            call Allocate_Array(this%Kflh, nsize)
            this%Kflh(:) = 0.0d0
        end select

    end function Construct_Type_HCF_Base_Impedance_KO

    module function Construct_Type_HCF_Base_Impedance_KO_minimal() result(structure_HCF)
        implicit none
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_Impedance_KO :: structure_HCF)

    end function Construct_Type_HCF_Base_Impedance_KO_minimal

    module function Construct_Type_HCF_Base_Viscosity_KO(Ks, alpha1, n1, l, useViscosity, nsize) result(structure_HCF)
        implicit none
        real(real64), intent(in) :: Ks
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: l
        integer(int32), intent(in) :: useViscosity
        integer(int32), intent(in) :: nsize
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_Viscosity_KO :: structure_HCF)

        select type (this => structure_HCF)
        type is (Type_HCF_Base_Viscosity_KO)
            this%Ks = Ks
            this%alpha1 = alpha1
            this%n1 = n1
            this%l = l

            call this%Set_Calculate_Viscosity(useViscosity, this%Calculate_Viscosity)
            this%Kzero = this%Ks * this%Calculate_Viscosity(15.d0)

            call Allocate_Array(this%Kflh, nsize)
            this%Kflh(:) = 0.0d0
        end select

    end function Construct_Type_HCF_Base_Viscosity_KO

    module function Construct_Type_HCF_Base_Viscosity_KO_minimal() result(structure_HCF)
        implicit none
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_Viscosity_KO :: structure_HCF)

    end function Construct_Type_HCF_Base_Viscosity_KO_minimal

    module function Construct_Type_HCF_Base_Impedance_Viscosity_KO(Ks, alpha1, n1, l, Omega, useViscosity, nsize) result(structure_HCF)
        implicit none
        real(real64), intent(in) :: Ks
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: l
        real(real64), intent(in) :: Omega
        integer(int32), intent(in) :: useViscosity
        integer(int32), intent(in) :: nsize
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_Impedance_Viscosity_KO :: structure_HCF)

        select type (this => structure_HCF)
        type is (Type_HCF_Base_Impedance_Viscosity_KO)
            this%Ks = Ks
            this%alpha1 = alpha1
            this%n1 = n1
            this%l = l
            this%Omega = Omega

            call this%Set_Calculate_Viscosity(useViscosity, this%Calculate_Viscosity)
            this%Kzero = this%Ks * this%Calculate_Viscosity(15.d0)

            call Allocate_Array(this%Kflh, nsize)
            this%Kflh(:) = 0.0d0
        end select

    end function Construct_Type_HCF_Base_Impedance_Viscosity_KO

    module function Construct_Type_HCF_Base_Impedance_Viscosity_KO_minimal() result(structure_HCF)
        implicit none
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_Impedance_Viscosity_KO :: structure_HCF)

    end function Construct_Type_HCF_Base_Impedance_Viscosity_KO_minimal
    !----------------------------------------------------------------------------------------------------
    ! Calculate kr for Kosugi model
    !----------------------------------------------------------------------------------------------------
    module function Calculate_kr_KO_Base(alpha1, n1, l, h) result(kr)
        !$omp declare simd uniform(alpha1, n1, l, h)
        implicit none
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: l
        real(real64), intent(in) :: h
        real(real64) :: kr
        real(real64) :: Sw

        if (h < 0.0d0) then
            Sw = 0.5d0 * erfc(log(h / alpha1) / (n1 * sqrt(2.0d0)))
            kr = Sw**0.5d0 * (0.5d0 * erfc(log(h / alpha1) / (n1 * sqrt(2.0d0)) + n1 / sqrt(2.0d0)))**2.0d0
        else
            kr = 1.0d0
        end if

    end function Calculate_kr_KO_Base

    !----------------------------------------------------------------------------------------------------
    ! Wrapper of calculating kr for Kosugi model bounding different derived types
    !----------------------------------------------------------------------------------------------------
    module function Calculate_kr_Base_KO(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_KO), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = Calculate_kr_KO_Base(self%alpha1, self%n1, self%l, h)

    end function Calculate_kr_Base_KO

    module function Calculate_kr_Base_Impedance_KO(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_Impedance_KO), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = Calculate_kr_KO_Base(self%alpha1, self%n1, self%l, h)

    end function Calculate_kr_Base_Impedance_KO

    module function Calculate_kr_Base_Viscosity_KO(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_Viscosity_KO), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = Calculate_kr_KO_Base(self%alpha1, self%n1, self%l, h)

    end function Calculate_kr_Base_Viscosity_KO

    module function Calculate_kr_Base_Impedance_Viscosity_KO(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_Impedance_Viscosity_KO), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = Calculate_kr_KO_Base(self%alpha1, self%n1, self%l, h)

    end function Calculate_kr_Base_Impedance_Viscosity_KO

    !----------------------------------------------------------------------------------------------------
    ! Update Kflh for Kosugi model bounding different derived types
    !----------------------------------------------------------------------------------------------------
    module function Calculate_Kflh_Base_KO(self, h) result(Kflh)
        implicit none
        class(Type_HCF_Base_KO), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: Kflh

        Kflh = self%Ks * self%Calculate_kr(h)

    end function Calculate_Kflh_Base_KO

    module function Calculate_Kflh_Base_Impedance_KO(self, h, thetaI) result(Kflh)
        implicit none
        class(Type_HCF_Base_Impedance_KO), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(in) :: thetaI
        real(real64) :: Kflh

        Kflh = self%Ks * self%Calculate_kr(h) * self%Calculate_Impedance(self%Omega, thetaI)

    end function Calculate_Kflh_Base_Impedance_KO

    module function Calculate_Kflh_Base_Viscosity_KO(self, h, Temperature) result(Kflh)
        implicit none
        class(Type_HCF_Base_Viscosity_KO), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(in) :: Temperature
        real(real64) :: Kflh

        Kflh = self%Kzero * self%Calculate_kr(h) / self%Calculate_Viscosity(Temperature)

    end function Calculate_Kflh_Base_Viscosity_KO

    module function Calculate_Kflh_Base_Impedance_Viscosity_KO(self, h, thetaI, Temperature) result(Kflh)
        implicit none
        class(Type_HCF_Base_Impedance_Viscosity_KO), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(in) :: thetaI
        real(real64), intent(in) :: Temperature
        real(real64) :: Kflh

        Kflh = self%Kzero * self%Calculate_kr(h) * self%Calculate_Impedance(self%Omega, thetaI) / self%Calculate_Viscosity(Temperature)

    end function Calculate_Kflh_Base_Impedance_Viscosity_KO

    !----------------------------------------------------------------------------------------------------
    ! Update Kflh for Kosugi model bounding different derived types
    !----------------------------------------------------------------------------------------------------
    module subroutine Update_Kflh_Base_KO(self, arr_h)
        implicit none
        class(Type_HCF_Base_KO), intent(inout) :: self
        real(real64), intent(in) :: arr_h(:)

        integer(int32) :: iN, n

        n = size(arr_h(:))

        !$omp parallel do schedule(guided) private(iN)
        do iN = 1, n
            self%Kflh(iN) = self%Calculate_Kflh(arr_h(iN))
        end do

    end subroutine Update_Kflh_Base_KO

    module subroutine Update_Kflh_Base_Impedance_KO(self, arr_h, arr_thetaI)
        implicit none
        class(Type_HCF_Base_Impedance_KO), intent(inout) :: self
        real(real64), intent(in) :: arr_h(:)
        real(real64), intent(in) :: arr_thetaI(:)

        integer(int32) :: iN, n

        n = size(arr_h(:))

        !$omp parallel do schedule(guided) private(iN)
        do iN = 1, n
            self%Kflh(iN) = self%Calculate_Kflh(arr_h(iN), arr_thetaI(iN))
        end do

    end subroutine Update_Kflh_Base_Impedance_KO

    module subroutine Update_Kflh_Base_Viscosity_KO(self, arr_h, arr_Temperature)
        implicit none
        class(Type_HCF_Base_Viscosity_KO), intent(inout) :: self
        real(real64), intent(in) :: arr_h(:)
        real(real64), intent(in) :: arr_Temperature(:)

        integer(int32) :: iN, n

        n = size(arr_h(:))

        !$omp parallel do schedule(guided) private(iN)
        do iN = 1, n
            self%Kflh(iN) = self%Calculate_Kflh(arr_h(iN), arr_Temperature(iN))
        end do

    end subroutine Update_Kflh_Base_Viscosity_KO

    module subroutine Update_Kflh_Base_Impedance_Viscosity_KO(self, arr_h, arr_thetaI, arr_Temperature)
        implicit none
        class(Type_HCF_Base_Impedance_Viscosity_KO), intent(inout) :: self
        real(real64), intent(in) :: arr_h(:)
        real(real64), intent(in) :: arr_thetaI(:)
        real(real64), intent(in) :: arr_Temperature(:)

        integer(int32) :: iN, n

        n = size(arr_h(:))

        !$omp parallel do schedule(guided) private(iN)
        do iN = 1, n
            self%Kflh(iN) = self%Calculate_Kflh(arr_h(iN), arr_thetaI(iN), arr_Temperature(iN))
        end do

    end subroutine Update_Kflh_Base_Impedance_Viscosity_KO

end submodule Calculate_HCF_KO_Implementation
