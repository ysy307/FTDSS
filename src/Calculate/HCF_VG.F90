submodule(Calculate_HCF) Calculate_HCF_VG_Implementation
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Constructe each types by using van-Genuchten model
    !----------------------------------------------------------------------------------------------------
    module function Construct_Type_HCF_Base_VG(Ks, alpha1, n1, l, nsize) result(structure_HCF)
        implicit none
        real(real64), intent(in) :: Ks
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: l
        integer(int32), intent(in) :: nsize
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_VG :: structure_HCF)

        select type (this => structure_HCF)
        type is (Type_HCF_Base_VG)
            this%Ks = Ks
            this%alpha1 = alpha1
            this%n1 = n1
            this%m1 = 1.0d0 - 1.0d0 / n1
            this%l = l

            call Allocate_Array(this%Kflh, nsize)
            this%Kflh(:) = 0.0d0
        end select

    end function Construct_Type_HCF_Base_VG

    module function Construct_Type_HCF_Base_VG_minimal() result(structure_HCF)
        implicit none
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_VG :: structure_HCF)

    end function Construct_Type_HCF_Base_VG_minimal

    module function Construct_Type_HCF_Base_Impedance_VG(Ks, alpha1, n1, l, Omega, nsize) result(structure_HCF)
        implicit none
        real(real64), intent(in) :: Ks
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: l
        real(real64), intent(in) :: Omega
        integer(int32), intent(in) :: nsize
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_Impedance_VG :: structure_HCF)

        select type (this => structure_HCF)
        type is (Type_HCF_Base_Impedance_VG)
            this%Ks = Ks
            this%alpha1 = alpha1
            this%n1 = n1
            this%m1 = 1.0d0 - 1.0d0 / n1
            this%l = l
            this%Omega = Omega

            call Allocate_Array(this%Kflh, nsize)
            this%Kflh(:) = 0.0d0
        end select

    end function Construct_Type_HCF_Base_Impedance_VG

    module function Construct_Type_HCF_Base_Impedance_VG_minimal() result(structure_HCF)
        implicit none
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_Impedance_VG :: structure_HCF)

    end function Construct_Type_HCF_Base_Impedance_VG_minimal

    module function Construct_Type_HCF_Base_Viscosity_VG(Ks, alpha1, n1, l, useViscosity, nsize) result(structure_HCF)
        implicit none
        real(real64), intent(in) :: Ks
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: l
        integer(int32), intent(in) :: useViscosity
        integer(int32), intent(in) :: nsize
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_Viscosity_VG :: structure_HCF)

        select type (this => structure_HCF)
        type is (Type_HCF_Base_Viscosity_VG)
            this%Ks = Ks
            this%alpha1 = alpha1
            this%n1 = n1
            this%m1 = 1.0d0 - 1.0d0 / n1
            this%l = l

            call this%Set_Calculate_Viscosity(useViscosity, this%Calculate_Viscosity)
            this%Kzero = this%Ks * this%Calculate_Viscosity(15.d0)

            call Allocate_Array(this%Kflh, nsize)
            this%Kflh(:) = 0.0d0
        end select

    end function Construct_Type_HCF_Base_Viscosity_VG

    module function Construct_Type_HCF_Base_Viscosity_VG_minimal() result(structure_HCF)
        implicit none
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_Viscosity_VG :: structure_HCF)

    end function Construct_Type_HCF_Base_Viscosity_VG_minimal

    module function Construct_Type_HCF_Base_Impedance_Viscosity_VG(Ks, alpha1, n1, l, Omega, useViscosity, nsize) result(structure_HCF)
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
        allocate (Type_HCF_Base_Impedance_Viscosity_VG :: structure_HCF)

        select type (this => structure_HCF)
        type is (Type_HCF_Base_Impedance_Viscosity_VG)
            this%Ks = Ks
            this%alpha1 = alpha1
            this%n1 = n1
            this%m1 = 1.0d0 - 1.0d0 / n1
            this%l = l
            this%Omega = Omega

            call this%Set_Calculate_Viscosity(useViscosity, this%Calculate_Viscosity)
            this%Kzero = this%Ks * this%Calculate_Viscosity(15.d0)

            call Allocate_Array(this%Kflh, nsize)
            this%Kflh(:) = 0.0d0
        end select

    end function Construct_Type_HCF_Base_Impedance_Viscosity_VG

    module function Construct_Type_HCF_Base_Impedance_Viscosity_VG_minimal() result(structure_HCF)
        implicit none
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Base_Impedance_Viscosity_VG :: structure_HCF)

    end function Construct_Type_HCF_Base_Impedance_Viscosity_VG_minimal

    !----------------------------------------------------------------------------------------------------
    ! Calculate kr for van-Genuchten model
    !----------------------------------------------------------------------------------------------------
    module function Calculate_kr_VG_Base(alpha1, n1, m1, l, h) result(kr)
        !$omp declare simd uniform(alpha1, n1, m1, l, h)
        implicit none
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: m1
        real(real64), intent(in) :: l
        real(real64), intent(in) :: h
        real(real64) :: kr
        real(real64) :: Sw

        if (h < 0.0d0) then
            Sw = (1.0d0 + (-alpha1 * h)**n1)**(-m1)
        else
            Sw = 1.0d0
        end if

        kr = Sw**l * (1.0d0 - (1.0d0 - Sw**(1.0d0 / m1))**m1)**2.0d0

    end function Calculate_kr_VG_Base

    !----------------------------------------------------------------------------------------------------
    ! Wrapper of calculating kr for van-Genuchten model bounding different derived types
    !----------------------------------------------------------------------------------------------------
    module function Calculate_kr_Base_VG(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_VG), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = Calculate_kr_VG_Base(self%alpha1, self%n1, self%m1, self%l, h)

    end function Calculate_kr_Base_VG

    module function Calculate_kr_Base_Impedance_VG(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_Impedance_VG), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = Calculate_kr_VG_Base(self%alpha1, self%n1, self%m1, self%l, h)

    end function Calculate_kr_Base_Impedance_VG

    module function Calculate_kr_Base_Viscosity_VG(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_Viscosity_VG), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = Calculate_kr_VG_Base(self%alpha1, self%n1, self%m1, self%l, h)

    end function Calculate_kr_Base_Viscosity_VG

    module function Calculate_kr_Base_Impedance_Viscosity_VG(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_Impedance_Viscosity_VG), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = Calculate_kr_VG_Base(self%alpha1, self%n1, self%m1, self%l, h)

    end function Calculate_kr_Base_Impedance_Viscosity_VG

    !----------------------------------------------------------------------------------------------------
    ! Update Kflh for van-Genuchten model bounding different derived types
    !----------------------------------------------------------------------------------------------------
    module function Calculate_Kflh_Base_VG(self, h) result(Kflh)
        implicit none
        class(Type_HCF_Base_VG), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: Kflh

        Kflh = self%Ks * self%Calculate_kr(h)

    end function Calculate_Kflh_Base_VG

    module function Calculate_Kflh_Base_Impedance_VG(self, h, thetaI) result(Kflh)
        implicit none
        class(Type_HCF_Base_Impedance_VG), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(in) :: thetaI
        real(real64) :: Kflh

        Kflh = self%Ks * self%Calculate_kr(h) * self%Calculate_Impedance(self%Omega, thetaI)

    end function Calculate_Kflh_Base_Impedance_VG

    module function Calculate_Kflh_Base_Viscosity_VG(self, h, Temperature) result(Kflh)
        implicit none
        class(Type_HCF_Base_Viscosity_VG), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(in) :: Temperature
        real(real64) :: Kflh

        Kflh = self%Kzero * self%Calculate_kr(h) / self%Calculate_Viscosity(Temperature)

    end function Calculate_Kflh_Base_Viscosity_VG

    module function Calculate_Kflh_Base_Impedance_Viscosity_VG(self, h, thetaI, Temperature) result(Kflh)
        implicit none
        class(Type_HCF_Base_Impedance_Viscosity_VG), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(in) :: thetaI
        real(real64), intent(in) :: Temperature
        real(real64) :: Kflh

        Kflh = self%Kzero * self%Calculate_kr(h) * self%Calculate_Impedance(self%Omega, thetaI) / self%Calculate_Viscosity(Temperature)

    end function Calculate_Kflh_Base_Impedance_Viscosity_VG

    !----------------------------------------------------------------------------------------------------
    ! Update Kflh for van-Genuchten model bounding different derived types
    !----------------------------------------------------------------------------------------------------
    module subroutine Update_Kflh_Base_VG(self, arr_h)
        implicit none
        class(Type_HCF_Base_VG), intent(inout) :: self
        real(real64), intent(in) :: arr_h(:)

        integer(int32) :: iN, n

        n = size(arr_h(:))

        !$omp parallel do schedule(guided) private(iN)
        do iN = 1, n
            self%Kflh(iN) = self%Calculate_Kflh(arr_h(iN))
        end do

    end subroutine Update_Kflh_Base_VG

    module subroutine Update_Kflh_Base_Impedance_VG(self, arr_h, arr_thetaI)
        implicit none
        class(Type_HCF_Base_Impedance_VG), intent(inout) :: self
        real(real64), intent(in) :: arr_h(:)
        real(real64), intent(in) :: arr_thetaI(:)

        integer(int32) :: iN, n

        n = size(arr_h(:))

        !$omp parallel do schedule(guided) private(iN)
        do iN = 1, n
            self%Kflh(iN) = self%Calculate_Kflh(arr_h(iN), arr_thetaI(iN))
        end do

    end subroutine Update_Kflh_Base_Impedance_VG

    module subroutine Update_Kflh_Base_Viscosity_VG(self, arr_h, arr_Temperature)
        implicit none
        class(Type_HCF_Base_Viscosity_VG), intent(inout) :: self
        real(real64), intent(in) :: arr_h(:)
        real(real64), intent(in) :: arr_Temperature(:)

        integer(int32) :: iN, n

        n = size(arr_h(:))

        !$omp parallel do schedule(guided) private(iN)
        do iN = 1, n
            self%Kflh(iN) = self%Calculate_Kflh(arr_h(iN), arr_Temperature(iN))
        end do

    end subroutine Update_Kflh_Base_Viscosity_VG

    module subroutine Update_Kflh_Base_Impedance_Viscosity_VG(self, arr_h, arr_thetaI, arr_Temperature)
        implicit none
        class(Type_HCF_Base_Impedance_Viscosity_VG), intent(inout) :: self
        real(real64), intent(in) :: arr_h(:)
        real(real64), intent(in) :: arr_thetaI(:)
        real(real64), intent(in) :: arr_Temperature(:)

        integer(int32) :: iN, n

        n = size(arr_h(:))

        !$omp parallel do schedule(guided) private(iN)
        do iN = 1, n
            self%Kflh(iN) = self%Calculate_Kflh(arr_h(iN), arr_thetaI(iN), arr_Temperature(iN))
        end do

    end subroutine Update_Kflh_Base_Impedance_Viscosity_VG

end submodule Calculate_HCF_VG_Implementation
