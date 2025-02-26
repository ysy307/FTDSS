module Calculate_Ice
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Calculate_WRF
    use :: Calculate_GCC
    implicit none

    type, abstract :: Abstract_Ice
    end type

    type, extends(Abstract_Ice) :: Type_Ice_TRM
        real(real64) :: Lf !! Latent heat
        real(real64) :: Tf !! Freezing point
    end type

    type, extends(Abstract_Ice) :: Type_Ice_GCC
        class(Abstract_WRF), allocatable :: WRF
        class(Abstract_GCC), allocatable :: GCC
    contains
        procedure, pass(self) :: Set_WRF => Set_Type_Ice_GCC_WRF
        procedure, pass(self) :: Set_GCC => Set_Type_Ice_GCC_GCC
        procedure, pass(self), private :: Calculate_Ice_GCC_NonSegregation_m
        procedure, pass(self), private :: Calculate_Ice_GCC_rhoW_scalar
        procedure, pass(self), private :: Calculate_Ice_GCC_rhoW_array
        generic :: Calculate_Ice => Calculate_Ice_GCC_NonSegregation_m, Calculate_Ice_GCC_rhoW_Scalar, Calculate_Ice_GCC_rhoW_Array
    end type Type_Ice_GCC

    type, extends(Abstract_Ice) :: Type_Ice_EXP
        real(real64) :: Tf !! Freezing point
        real(real64) :: a !! power model parameter
    end type Type_Ice_EXP

contains

    subroutine Set_Type_Ice_GCC_WRF(self, ModelType)
        class(Type_Ice_GCC), intent(inout) :: self
        integer(int32), intent(in) :: ModelType

        if (allocated(self%WRF)) deallocate (self%WRF)
        select case (ModelType)
        case (1)
            allocate (Type_WRF_BC :: self%WRF)
        case (2)
            allocate (Type_WRF_VG :: self%WRF)
        case (3)
            allocate (Type_WRF_KO :: self%WRF)
        case (4)
            allocate (Type_WRF_MVG :: self%WRF)
        case (5)
            allocate (Type_WRF_Durner :: self%WRF)
        case (6)
            allocate (Type_WRF_DVGCH :: self%WRF)
        case default
            stop 'Invalid ModelType'
        end select

    end subroutine Set_Type_Ice_GCC_WRF

    subroutine Set_Type_Ice_GCC_GCC(self, isSegregation, c_unit)
        class(Type_Ice_GCC), intent(inout) :: self
        logical(4), intent(in) :: isSegregation
        character(*), intent(in) :: c_unit

        if (allocated(self%GCC)) deallocate (self%GCC)
        if (isSegregation) then
            select case (c_unit)
            case ('m')
                allocate (Type_GCC_Segregation_m :: self%GCC)
            case ("Pa")
                allocate (Type_GCC_Segregation_Pa :: self%GCC)
            case default
                stop 'Invalid unit'
            end select
        else
            select case (c_unit)
            case ('m')
                allocate (Type_GCC_NonSegregation_m :: self%GCC)
            case ('Pa')
                allocate (Type_GCC_NonSegregation_Pa :: self%GCC)
            case default
                stop 'Invalid unit'
            end select
        end if

    end subroutine Set_Type_Ice_GCC_GCC

    subroutine Calculate_Ice_GCC_NonSegregation_m(self, arr_Qice, arr_Temperature)
        class(Type_Ice_GCC), intent(inout) :: self
        real(real64), intent(in) :: arr_Temperature(:)
        real(real64), intent(inout) :: arr_Qice(:)

        real(real64) :: Qs
        integer(int32) :: i

        Qs = self%WRF%thetaS

        select type (GCC => self%GCC)
        type is (Type_GCC_NonSegregation_m)
            do i = 1, size(arr_Qice)
                arr_Qice(i) = Qs - self%WRF%Calculate_WRF(-GCC%Calculate_GCC(arr_Temperature(i)))
            end do
        end select
    end subroutine Calculate_Ice_GCC_NonSegregation_m

    subroutine Calculate_Ice_GCC_rhoW_scalar(self, arr_Qice, arr_Temperature, rhoW, arr_Pw)
        class(Type_Ice_GCC), intent(inout) :: self
        real(real64), intent(in) :: arr_Temperature(:)
        real(real64), intent(inout) :: arr_Qice(:)
        real(real64), intent(in), optional :: arr_Pw(:)
        real(real64), intent(in) :: rhoW

        real(real64) :: Qs
        integer(int32) :: i

        Qs = self%WRF%thetaS

        select type (GCC => self%GCC)
        type is (Type_GCC_NonSegregation_Pa)
            do i = 1, size(arr_Qice)
                arr_Qice(i) = Qs - self%WRF%Calculate_WRF(-GCC%Calculate_GCC(arr_Temperature(i), rhoW))
            end do
        type is (Type_GCC_Segregation_m)
            if (.not. present(arr_Pw)) stop 'arr_Pw is required'
            do i = 1, size(arr_Qice)
                arr_Qice(i) = Qs - self%WRF%Calculate_WRF(-GCC%Calculate_GCC(arr_Temperature(i), arr_Pw(i), rhoW))
            end do
        type is (Type_GCC_Segregation_Pa)
            if (.not. present(arr_Pw)) stop 'arr_Pw is required'
            do i = 1, size(arr_Qice)
                arr_Qice(i) = Qs - self%WRF%Calculate_WRF(-GCC%Calculate_GCC(arr_Temperature(i), arr_Pw(i), rhoW))
            end do

        end select

    end subroutine Calculate_Ice_GCC_rhoW_scalar

    subroutine Calculate_Ice_GCC_rhoW_array(self, arr_Qice, arr_Temperature, arr_rhoW, arr_Pw)
        class(Type_Ice_GCC), intent(inout) :: self
        real(real64), intent(in) :: arr_Temperature(:)
        real(real64), intent(inout) :: arr_Qice(:)
        real(real64), intent(in) :: arr_rhoW(:)
        real(real64), intent(in), optional :: arr_Pw(:)

        real(real64) :: Qs
        integer(int32) :: i

        Qs = self%WRF%thetaS

        select type (GCC => self%GCC)
        type is (Type_GCC_NonSegregation_Pa)
            do i = 1, size(arr_Qice)
                arr_Qice(i) = Qs - self%WRF%Calculate_WRF(-GCC%Calculate_GCC(arr_Temperature(i), arr_rhoW(i)))
            end do
        type is (Type_GCC_Segregation_m)
            if (.not. present(arr_Pw)) stop 'arr_Pw is required'
            do i = 1, size(arr_Qice)
                arr_Qice(i) = Qs - self%WRF%Calculate_WRF(-GCC%Calculate_GCC(arr_Temperature(i), arr_Pw(i), arr_rhoW(i)))
            end do
        type is (Type_GCC_Segregation_Pa)
            if (.not. present(arr_Pw)) stop 'arr_Pw is required'
            do i = 1, size(arr_Qice)
                arr_Qice(i) = Qs - self%WRF%Calculate_WRF(-GCC%Calculate_GCC(arr_Temperature(i), arr_Pw(i), arr_rhoW(i)))
            end do
        end select

    end subroutine Calculate_Ice_GCC_rhoW_array

end module Calculate_Ice
