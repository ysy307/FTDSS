submodule(Calculate_Ice) Calculate_Ice_GCC_Implementation
    use, intrinsic :: iso_fortran_env, only: int32, real64
    implicit none
contains
    module function Construct_Type_Ice_GCC(ModelType, isSegregation, c_unit, nsize, thetaS, thetaR, alpha1, n1, w1, hcrit, alpha2, n2, Tf, Lf, rhoI) result(construct)
        use :: Allocate_Allocate, only:Allocate_Array
        implicit none
        integer(int32), intent(in) :: ModelType
        logical(4), intent(in) :: isSegregation
        character(*), intent(in) :: c_unit
        integer(int32), intent(in) :: nsize
        real(real64), intent(in) :: thetaS
        real(real64), intent(in) :: thetaR
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in), optional :: w1
        real(real64), intent(in), optional :: hcrit
        real(real64), intent(in), optional :: alpha2
        real(real64), intent(in), optional :: n2
        real(real64), intent(in) :: Tf
        real(real64), intent(in) :: Lf
        real(real64), intent(in), optional :: rhoI

        class(Abstract_Ice), allocatable :: construct

        if (allocated(construct)) deallocate (construct)
        allocate (Type_Ice_GCC :: construct)

        select type (this => construct)
        type is (Type_Ice_GCC)
            select case (ModelType)
            case (1:3)
                this%WRF = this%Set_WRF(ModelType=ModelType, & !&
                                        thetaS=thetaS, & !&
                                        thetaR=thetaR, & !&
                                        alpha1=alpha1, & !&
                                        n1=n1) !&
            case (4)
                if (.not. present(hcrit)) stop 'hcrit is required' !&
                this%WRF = this%Set_WRF(ModelType=ModelType, & !&
                                        thetaS=thetaS, & !&
                                        thetaR=thetaR, & !&
                                        alpha1=alpha1, & !&
                                        n1=n1, & !&
                                        hcrit=hcrit) !&
            case (5)
                if (.not. present(w1) .or. & !&
                    .not. present(alpha2) .or. & !&
                    .not. present(n2) & !&
                    ) stop 'w1, alpha2, n2 are required'
                this%WRF = this%Set_WRF(ModelType=ModelType, & !&
                                        thetaS=thetaS, & !&
                                        thetaR=thetaR, & !&
                                        alpha1=alpha1, & !&
                                        n1=n1, & !&
                                        w1=w1, & !&
                                        alpha2=alpha2, & !&
                                        n2=n2) !&
            case (6)
                if (.not. present(w1) .or. & !&
                    .not. present(n2) & !&
                    ) stop 'w1, alpha2, n2 are required' !&
                this%WRF = this%Set_WRF(ModelType=ModelType, & !&
                                        thetaS=thetaS, & !&
                                        thetaR=thetaR, & !&
                                        alpha1=alpha1, & !&
                                        n1=n1, & !&
                                        w1=w1, & !&
                                        n2=n2) !&
            case default
                stop 'Invalid ModelType'
            end select
            if (isSegregation) then
                if (.not. present(rhoI)) stop 'rhoI is required'
                this%GCC = this%Set_GCC(isSegregation=isSegregation, & !&
                                        c_unit=c_unit, & !&
                                        Tf=Tf, & !&
                                        Lf=Lf, & !&
                                        rhoI=rhoI) !&
            else
                this%GCC = this%Set_GCC(isSegregation=isSegregation, & !&
                                        c_unit=c_unit, & !&
                                        Tf=Tf, & !&
                                        Lf=Lf) !&
            end if

            call Allocate_Array(this%Qw%old, nsize)
            call Allocate_Array(this%Qw%pre, nsize)
            call Allocate_Array(this%Qw%new, nsize)

            this%Qw%old(:) = 0.0d0
            this%Qw%pre(:) = 0.0d0
            this%Qw%new(:) = 0.0d0

            call Allocate_Array(this%Qice%old, nsize)
            call Allocate_Array(this%Qice%pre, nsize)
            call Allocate_Array(this%Qice%new, nsize)

            this%Qice%old(:) = 0.0d0
            this%Qice%pre(:) = 0.0d0
            this%Qice%new(:) = 0.0d0

            call Allocate_Array(this%Si%old, nsize)
            call Allocate_Array(this%Si%pre, nsize)
            call Allocate_Array(this%Si%new, nsize)

            this%Si%old(:) = 0.0d0
            this%Si%pre(:) = 0.0d0
            this%Si%new(:) = 0.0d0

            call Allocate_Array(this%D_Qice%old, nsize)
            call Allocate_Array(this%D_Qice%pre, nsize)
            call Allocate_Array(this%D_Qice%new, nsize)

            this%D_Qice%old(:) = 0.0d0
            this%D_Qice%pre(:) = 0.0d0
            this%D_Qice%new(:) = 0.0d0

        end select

    end function Construct_Type_Ice_GCC

    module function Construct_Type_Ice_GCC_minimum(ModelType, isSegregation, c_unit) result(construct)
        implicit none
        integer(int32), intent(in) :: ModelType
        logical(4), intent(in) :: isSegregation
        character(*), intent(in) :: c_unit

        class(Abstract_Ice), allocatable :: construct

        if (allocated(construct)) deallocate (construct)
        allocate (Type_Ice_GCC :: construct)

        select type (this => construct)
        type is (Type_Ice_GCC)
            this%WRF = this%Set_WRF(ModelType=ModelType)

            this%GCC = this%Set_GCC(isSegregation=isSegregation, & !&
                                        c_unit=c_unit) !&
        end select

    end function Construct_Type_Ice_GCC_minimum

    module function Set_Type_Ice_GCC_WRF(ModelType, thetaS, thetaR, alpha1, n1, w1, hcrit, alpha2, n2) result(structure_WRF)
        implicit none
        integer(int32), intent(in) :: ModelType
        real(real64), intent(in) :: thetaS
        real(real64), intent(in) :: thetaR
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in), optional :: w1
        real(real64), intent(in), optional :: hcrit
        real(real64), intent(in), optional :: alpha2
        real(real64), intent(in), optional :: n2
        class(Abstract_WRF), allocatable :: structure_WRF

        if (allocated(structure_WRF)) deallocate (structure_WRF)
        select case (ModelType)
        case (1)
            structure_WRF = Type_WRF_BC(thetaS=thetaS, & !&
                                        thetaR=thetaR, & !&
                                        alpha1=alpha1, & !&
                                        n1=n1) !&
        case (2)
            structure_WRF = Type_WRF_VG(thetaS=thetaS, & !&
                                        thetaR=thetaR, & !&
                                        alpha1=alpha1, & !&
                                        n1=n1) !&
        case (3)
            structure_WRF = Type_WRF_KO(thetaS=thetaS, & !&
                                        thetaR=thetaR, & !&
                                        alpha1=alpha1, & !&
                                        n1=n1) !&
        case (4)
            structure_WRF = Type_WRF_MVG(thetaS=thetaS, & !&
                                         thetaR=thetaR, & !&
                                         alpha1=alpha1, & !&
                                         n1=n1, & !&
                                         hcrit=hcrit) !&
        case (5)
            structure_WRF = Type_WRF_Durner(thetaS=thetaS, & !&
                                            thetaR=thetaR, & !&
                                            alpha1=alpha1, & !&
                                            n1=n1, & !&
                                            w1=w1, & !&
                                            alpha2=alpha2, & !&
                                            n2=n2) !&
        case (6)
            structure_WRF = Type_WRF_DVGCH(thetaS=thetaS, & !&
                                           thetaR=thetaR, & !&
                                           alpha1=alpha1, & !&
                                           n1=n1, & !&
                                           w1=w1, & !&
                                           n2=n2) !&
        end select

    end function Set_Type_Ice_GCC_WRF

    module function Set_Type_Ice_GCC_WRF_minimum(ModelType) result(structure_WRF)
        implicit none
        integer(int32), intent(in) :: ModelType
        class(Abstract_WRF), allocatable :: structure_WRF

        if (allocated(structure_WRF)) deallocate (structure_WRF)
        select case (ModelType)
        case (1)
            structure_WRF = Type_WRF_BC()
        case (2)
            structure_WRF = Type_WRF_VG()
        case (3)
            structure_WRF = Type_WRF_KO()
        case (4)
            structure_WRF = Type_WRF_MVG()
        case (5)
            structure_WRF = Type_WRF_Durner()
        case (6)
            structure_WRF = Type_WRF_DVGCH()
        end select

    end function Set_Type_Ice_GCC_WRF_minimum

    module function Set_Type_Ice_GCC_GCC(isSegregation, c_unit, Tf, Lf, rhoI) result(structure_GCC)
        implicit none
        logical(4), intent(in) :: isSegregation
        character(*), intent(in) :: c_unit
        real(real64), intent(in) :: Tf
        real(real64), intent(in) :: Lf
        real(real64), intent(in), optional :: rhoI

        class(Abstract_GCC), allocatable :: structure_GCC

        if (allocated(structure_GCC)) deallocate (structure_GCC)
        if (isSegregation) then
            select case (c_unit)
            case ('m')
                structure_GCC = Type_GCC_Segregation_m(Tf, Lf, rhoI)
            case ("Pa")
                structure_GCC = Type_GCC_Segregation_Pa(Tf, Lf, rhoI)
            case default
                stop 'Invalid unit'
            end select
        else
            select case (c_unit)
            case ('m')
                structure_GCC = Type_GCC_NonSegregation_m(Tf, Lf)
            case ("Pa")
                structure_GCC = Type_GCC_NonSegregation_Pa(Tf, Lf)
            case default
                stop 'Invalid unit'
            end select
        end if

    end function Set_Type_Ice_GCC_GCC

    module function Set_Type_Ice_GCC_GCC_minimum(isSegregation, c_unit) result(structure_GCC)
        implicit none
        logical(4), intent(in) :: isSegregation
        character(*), intent(in) :: c_unit

        class(Abstract_GCC), allocatable :: structure_GCC

        if (allocated(structure_GCC)) deallocate (structure_GCC)
        if (isSegregation) then
            select case (c_unit)
            case ('m')
                structure_GCC = Type_GCC_Segregation_m()
            case ("Pa")
                structure_GCC = Type_GCC_Segregation_Pa()
            case default
                stop 'Invalid unit'
            end select
        else
            select case (c_unit)
            case ('m')
                structure_GCC = Type_GCC_NonSegregation_m()
            case ("Pa")
                structure_GCC = Type_GCC_NonSegregation_Pa()
            case default
                stop 'Invalid unit'
            end select
        end if

    end function Set_Type_Ice_GCC_GCC_minimum

    module function Calculate_Ice_GCC_NonSegregation_m(self, Temperature) result(Qice)
        implicit none
        class(Type_Ice_GCC), intent(inout) :: self
        real(real64), intent(in) :: Temperature
        real(real64) :: Qice

        select type (GCC => self%GCC)
        type is (Type_GCC_NonSegregation_m)
            Qice = self%WRF%thetaS - self%WRF%Calculate_WRF(-GCC%Calculate_GCC(Temperature))
        end select
    end function Calculate_Ice_GCC_NonSegregation_m

    module function Calculate_Ice_GCC_rhoW_scalar(self, Temperature, rhoW, Pw) result(Qice)
        implicit none
        class(Type_Ice_GCC), intent(inout) :: self
        real(real64), intent(in) :: Temperature
        real(real64), intent(in) :: rhoW
        real(real64), intent(in), optional :: Pw
        real(real64) :: Qice

        select type (GCC => self%GCC)
        type is (Type_GCC_NonSegregation_Pa)
            Qice = self%WRF%thetaS - self%WRF%Calculate_WRF(-GCC%Calculate_GCC(Temperature, rhoW))
        type is (Type_GCC_Segregation_m)
            if (.not. present(Pw)) stop 'Pw is required'
            Qice = self%WRF%thetaS - self%WRF%Calculate_WRF(-GCC%Calculate_GCC(Temperature, Pw, rhoW))
        type is (Type_GCC_Segregation_Pa)
            if (.not. present(Pw)) stop 'Pw is required'
            Qice = self%WRF%thetaS - self%WRF%Calculate_WRF(-GCC%Calculate_GCC(Temperature, Pw, rhoW))
        end select

    end function Calculate_Ice_GCC_rhoW_scalar

    module function Calculate_Ice_GCC_Derivative_Temperature_NonSegregation_m(self, Temperature) result(D_Qice)
        implicit none
        class(Type_Ice_GCC), intent(inout) :: self
        real(real64), intent(in) :: Temperature
        real(real64) :: D_Qice

        select type (GCC => self%GCC)
        type is (Type_GCC_NonSegregation_m)
            D_Qice = self%WRF%Calculate_WRF_Derivative(-GCC%Calculate_GCC(Temperature)) * GCC%Calculate_GCC_Derivative(Temperature)
        end select
    end function Calculate_Ice_GCC_Derivative_Temperature_NonSegregation_m

    module function Calculate_Ice_GCC_Derivative_Temperature_rhoW_scalar(self, Temperature, rhoW, Pw) result(D_Qice)
        implicit none
        class(Type_Ice_GCC), intent(inout) :: self
        real(real64), intent(in) :: Temperature
        real(real64), intent(in) :: rhoW
        real(real64), intent(in), optional :: Pw
        real(real64) :: D_Qice

        select type (GCC => self%GCC)
        type is (Type_GCC_NonSegregation_Pa)
            D_Qice = self%WRF%Calculate_WRF_Derivative(-GCC%Calculate_GCC(Temperature, rhoW)) * GCC%Calculate_GCC_Derivative(Temperature, rhoW)

        type is (Type_GCC_Segregation_m)
            if (.not. present(Pw)) stop 'Pw is required'
            D_Qice = self%WRF%Calculate_WRF_Derivative(-GCC%Calculate_GCC(Temperature, Pw, rhoW)) * GCC%Calculate_GCC_Derivative(Temperature, Pw, rhoW)
        type is (Type_GCC_Segregation_Pa)
            if (.not. present(Pw)) stop 'Pw is required'
            D_Qice = self%WRF%Calculate_WRF_Derivative(-GCC%Calculate_GCC(Temperature, Pw, rhoW)) * GCC%Calculate_GCC_Derivative(Temperature, Pw, rhoW)
        end select

    end function Calculate_Ice_GCC_Derivative_Temperature_rhoW_scalar

    module subroutine Update_Ice_GCC_NonSegregation_m(self, arr_Temperature)
        implicit none
        class(Type_Ice_GCC), intent(inout) :: self
        real(real64), intent(in) :: arr_Temperature(:)

        real(real64) :: Qs
        integer(int32) :: iN, n

        Qs = self%WRF%thetaS

        n = size(arr_Temperature)

        select type (GCC => self%GCC)
        type is (Type_GCC_NonSegregation_m)
            !$omp parallel do schedule(guided) private(iN)
            do iN = 1, n
                self%Qice%pre(iN) = Qs - self%WRF%Calculate_WRF(-GCC%Calculate_GCC(arr_Temperature(iN)))
            end do
        end select
    end subroutine Update_Ice_GCC_NonSegregation_m

    module subroutine Update_Ice_GCC_rhoW_scalar(self, arr_Temperature, rhoW, arr_Pw)
        implicit none
        class(Type_Ice_GCC), intent(inout) :: self
        real(real64), intent(in) :: arr_Temperature(:)
        real(real64), intent(in) :: rhoW
        real(real64), intent(in), optional :: arr_Pw(:)

        real(real64) :: Qs
        integer(int32) :: iN, n

        Qs = self%WRF%thetaS
        n = size(arr_Temperature)

        select type (GCC => self%GCC)
        type is (Type_GCC_NonSegregation_Pa)
            !$omp parallel do schedule(guided) private(iN)
            do iN = 1, n
                self%Qice%pre(iN) = Qs - self%WRF%Calculate_WRF(-GCC%Calculate_GCC(arr_Temperature(iN), rhoW))
            end do
        type is (Type_GCC_Segregation_m)
            if (.not. present(arr_Pw)) stop 'arr_Pw is required'
            !$omp parallel do schedule(guided) private(iN)
            do iN = 1, n
                self%Qice%pre(iN) = Qs - self%WRF%Calculate_WRF(-GCC%Calculate_GCC(arr_Temperature(iN), arr_Pw(iN), rhoW))
            end do
        type is (Type_GCC_Segregation_Pa)
            if (.not. present(arr_Pw)) stop 'arr_Pw is required'
            !$omp parallel do schedule(guided) private(iN)
            do iN = 1, n
                self%Qice%pre(iN) = Qs - self%WRF%Calculate_WRF(-GCC%Calculate_GCC(arr_Temperature(iN), arr_Pw(iN), rhoW))
            end do

        end select

    end subroutine Update_Ice_GCC_rhoW_scalar

    module subroutine Update_Ice_GCC_rhoW_array(self, arr_Temperature, arr_rhoW, arr_Pw)
        implicit none
        class(Type_Ice_GCC), intent(inout) :: self
        real(real64), intent(in) :: arr_Temperature(:)
        real(real64), intent(in) :: arr_rhoW(:)
        real(real64), intent(in), optional :: arr_Pw(:)

        real(real64) :: Qs
        integer(int32) :: iN, n

        Qs = self%WRF%thetaS
        n = size(arr_Temperature)

        select type (GCC => self%GCC)
        type is (Type_GCC_NonSegregation_Pa)
            !$omp parallel do schedule(guided) private(iN)
            do iN = 1, n
                self%Qice%pre(iN) = Qs - self%WRF%Calculate_WRF(-GCC%Calculate_GCC(arr_Temperature(iN), arr_rhoW(iN)))
            end do
        type is (Type_GCC_Segregation_m)
            if (.not. present(arr_Pw)) stop 'arr_Pw is required'
            !$omp parallel do schedule(guided) private(iN)
            do iN = 1, n
                self%Qice%pre(iN) = Qs - self%WRF%Calculate_WRF(-GCC%Calculate_GCC(arr_Temperature(iN), arr_Pw(iN), arr_rhoW(iN)))
            end do
        type is (Type_GCC_Segregation_Pa)
            if (.not. present(arr_Pw)) stop 'arr_Pw is required'
            !$omp parallel do schedule(guided) private(iN)
            do iN = 1, n
                self%Qice%pre(iN) = Qs - self%WRF%Calculate_WRF(-GCC%Calculate_GCC(arr_Temperature(iN), arr_Pw(iN), arr_rhoW(iN)))
            end do
        end select

    end subroutine Update_Ice_GCC_rhoW_array

    module subroutine Update_Ice_GCC_Derivative_Temperature_NonSegregation_m(self, arr_Temperature)
        implicit none
        class(Type_Ice_GCC), intent(inout) :: self
        real(real64), intent(in) :: arr_Temperature(:)

        real(real64) :: Qs
        integer(int32) :: iN, n

        Qs = self%WRF%thetaS
        n = size(arr_Temperature)

        select type (GCC => self%GCC)
        type is (Type_GCC_NonSegregation_m)
            !$omp parallel do schedule(guided) private(iN)
            do iN = 1, n
                self%D_Qice%pre(iN) = self%WRF%Calculate_WRF_Derivative(-GCC%Calculate_GCC(arr_Temperature(iN))) * GCC%Calculate_GCC_Derivative(arr_Temperature(iN))
            end do
        end select
    end subroutine Update_Ice_GCC_Derivative_Temperature_NonSegregation_m

    module subroutine Update_Ice_GCC_Derivative_Temperature_rhoW_scalar(self, arr_Temperature, rhoW, arr_Pw)
        implicit none
        class(Type_Ice_GCC), intent(inout) :: self
        real(real64), intent(in) :: arr_Temperature(:)
        real(real64), intent(in) :: rhoW
        real(real64), intent(in), optional :: arr_Pw(:)

        real(real64) :: Qs
        integer(int32) :: iN, n

        Qs = self%WRF%thetaS
        n = size(arr_Temperature)

        select type (GCC => self%GCC)
        type is (Type_GCC_NonSegregation_Pa)
            !$omp parallel do schedule(guided) private(iN)
            do iN = 1, n
                self%D_Qice%pre(iN) = self%WRF%Calculate_WRF_Derivative(-GCC%Calculate_GCC(arr_Temperature(iN), rhoW)) * GCC%Calculate_GCC_Derivative(arr_Temperature(iN), rhoW)
            end do
        type is (Type_GCC_Segregation_m)
            if (.not. present(arr_Pw)) stop 'arr_Pw is required'
            !$omp parallel do schedule(guided) private(iN)
            do iN = 1, n
                self%D_Qice%pre(iN) = self%WRF%Calculate_WRF_Derivative(-GCC%Calculate_GCC(arr_Temperature(iN), arr_Pw(iN), rhoW)) * GCC%Calculate_GCC_Derivative(arr_Temperature(iN), arr_Pw(iN), rhoW)
            end do
        type is (Type_GCC_Segregation_Pa)
            if (.not. present(arr_Pw)) stop 'arr_Pw is required'
            !$omp parallel do schedule(guided) private(iN)
            do iN = 1, n
                self%D_Qice%pre(iN) = self%WRF%Calculate_WRF_Derivative(-GCC%Calculate_GCC(arr_Temperature(iN), arr_Pw(iN), rhoW)) * GCC%Calculate_GCC_Derivative(arr_Temperature(iN), arr_Pw(iN), rhoW)
            end do
        end select

    end subroutine Update_Ice_GCC_Derivative_Temperature_rhoW_scalar

    module subroutine Update_Ice_GCC_Derivative_Temperature_rhoW_array(self, arr_Temperature, arr_rhoW, arr_Pw)
        implicit none
        class(Type_Ice_GCC), intent(inout) :: self
        real(real64), intent(in) :: arr_Temperature(:)
        real(real64), intent(in) :: arr_rhoW(:)
        real(real64), intent(in), optional :: arr_Pw(:)

        real(real64) :: Qs
        integer(int32) :: iN, n

        Qs = self%WRF%thetaS
        n = size(arr_Temperature)

        select type (GCC => self%GCC)
        type is (Type_GCC_NonSegregation_Pa)
            !$omp parallel do schedule(guided) private(iN)
            do iN = 1, n
                self%D_Qice%pre(iN) = self%WRF%Calculate_WRF_Derivative(-GCC%Calculate_GCC(arr_Temperature(iN), arr_rhoW(iN))) * GCC%Calculate_GCC_Derivative(arr_Temperature(iN), arr_rhoW(iN))
            end do
        type is (Type_GCC_Segregation_m)
            if (.not. present(arr_Pw)) stop 'arr_Pw is required'
            !$omp parallel do schedule(guided) private(iN)
            do iN = 1, n
                self%D_Qice%pre(iN) = self%WRF%Calculate_WRF_Derivative(-GCC%Calculate_GCC(arr_Temperature(iN), arr_Pw(iN), arr_rhoW(iN))) * GCC%Calculate_GCC_Derivative(arr_Temperature(iN), arr_Pw(iN), arr_rhoW(iN))
            end do
        type is (Type_GCC_Segregation_Pa)
            if (.not. present(arr_Pw)) stop 'arr_Pw is required'
            !$omp parallel do schedule(guided) private(iN)
            do iN = 1, n
                self%D_Qice%pre(iN) = self%WRF%Calculate_WRF_Derivative(-GCC%Calculate_GCC(arr_Temperature(iN), arr_Pw(iN), arr_rhoW(iN))) * GCC%Calculate_GCC_Derivative(arr_Temperature(iN), arr_Pw(iN), arr_rhoW(iN))
            end do
        end select

    end subroutine Update_Ice_GCC_Derivative_Temperature_rhoW_array

end submodule Calculate_Ice_GCC_Implementation
