submodule(Calculate_Ice) Calculate_Ice_GCC_Implementation
    use, intrinsic :: iso_fortran_env, only: int32, real64
    implicit none
contains
    module subroutine Set_Type_Ice_GCC_WRF(self, ModelType)
        implicit none
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

    module subroutine Set_Type_Ice_GCC_GCC(self, isSegregation, c_unit)
        implicit none
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

    module subroutine Calculate_Ice_GCC_NonSegregation_m(self, arr_Temperature)
        implicit none
        class(Type_Ice_GCC), intent(inout) :: self
        real(real64), intent(in) :: arr_Temperature(:)

        real(real64) :: Qs
        integer(int32) :: i, n

        Qs = self%WRF%thetaS
        n = size(arr_Temperature)

        select type (GCC => self%GCC)
        type is (Type_GCC_NonSegregation_m)
            !$omp parallel do schedule(guided) private(i)
            do i = 1, n
                self%Qice%pre(i) = Qs - self%WRF%Calculate_WRF(-GCC%Calculate_GCC(arr_Temperature(i)))
            end do
        end select
    end subroutine Calculate_Ice_GCC_NonSegregation_m

    module subroutine Calculate_Ice_GCC_rhoW_scalar(self, arr_Temperature, rhoW, arr_Pw)
        implicit none
        class(Type_Ice_GCC), intent(inout) :: self
        real(real64), intent(in) :: arr_Temperature(:)
        real(real64), intent(in) :: rhoW
        real(real64), intent(in), optional :: arr_Pw(:)

        real(real64) :: Qs
        integer(int32) :: i, n

        Qs = self%WRF%thetaS
        n = size(arr_Temperature)

        select type (GCC => self%GCC)
        type is (Type_GCC_NonSegregation_Pa)
            !$omp parallel do schedule(guided) private(i)
            do i = 1, n
                self%Qice%pre(i) = Qs - self%WRF%Calculate_WRF(-GCC%Calculate_GCC(arr_Temperature(i), rhoW))
            end do
        type is (Type_GCC_Segregation_m)
            if (.not. present(arr_Pw)) stop 'arr_Pw is required'
            !$omp parallel do schedule(guided) private(i)
            do i = 1, n
                self%Qice%pre(i) = Qs - self%WRF%Calculate_WRF(-GCC%Calculate_GCC(arr_Temperature(i), arr_Pw(i), rhoW))
            end do
        type is (Type_GCC_Segregation_Pa)
            if (.not. present(arr_Pw)) stop 'arr_Pw is required'
            !$omp parallel do schedule(guided) private(i)
            do i = 1, n
                self%Qice%pre(i) = Qs - self%WRF%Calculate_WRF(-GCC%Calculate_GCC(arr_Temperature(i), arr_Pw(i), rhoW))
            end do

        end select

    end subroutine Calculate_Ice_GCC_rhoW_scalar

    module subroutine Calculate_Ice_GCC_rhoW_array(self, arr_Temperature, arr_rhoW, arr_Pw)
        implicit none
        class(Type_Ice_GCC), intent(inout) :: self
        real(real64), intent(in) :: arr_Temperature(:)
        real(real64), intent(in) :: arr_rhoW(:)
        real(real64), intent(in), optional :: arr_Pw(:)

        real(real64) :: Qs
        integer(int32) :: i, n

        Qs = self%WRF%thetaS
        n = size(arr_Temperature)

        select type (GCC => self%GCC)
        type is (Type_GCC_NonSegregation_Pa)
            !$omp parallel do schedule(guided) private(i)
            do i = 1, n
                self%Qice%pre(i) = Qs - self%WRF%Calculate_WRF(-GCC%Calculate_GCC(arr_Temperature(i), arr_rhoW(i)))
            end do
        type is (Type_GCC_Segregation_m)
            if (.not. present(arr_Pw)) stop 'arr_Pw is required'
            !$omp parallel do schedule(guided) private(i)
            do i = 1, n
                self%Qice%pre(i) = Qs - self%WRF%Calculate_WRF(-GCC%Calculate_GCC(arr_Temperature(i), arr_Pw(i), arr_rhoW(i)))
            end do
        type is (Type_GCC_Segregation_Pa)
            if (.not. present(arr_Pw)) stop 'arr_Pw is required'
            !$omp parallel do schedule(guided) private(i)
            do i = 1, n
                self%Qice%pre(i) = Qs - self%WRF%Calculate_WRF(-GCC%Calculate_GCC(arr_Temperature(i), arr_Pw(i), arr_rhoW(i)))
            end do
        end select

    end subroutine Calculate_Ice_GCC_rhoW_array

    module subroutine Calculate_Ice_GCC_Derivative_Temperature_NonSegregation_m(self, arr_Temperature)
        implicit none
        class(Type_Ice_GCC), intent(inout) :: self
        real(real64), intent(in) :: arr_Temperature(:)

        real(real64) :: Qs
        integer(int32) :: i, n

        Qs = self%WRF%thetaS
        n = size(arr_Temperature)

        select type (GCC => self%GCC)
        type is (Type_GCC_NonSegregation_m)
            !$omp parallel do schedule(guided) private(i)
            do i = 1, n
                self%D_Qice%pre(i) = self%WRF%Calculate_WRF_Derivative(-GCC%Calculate_GCC(arr_Temperature(i))) * GCC%Calculate_GCC_Derivative(arr_Temperature(i))
            end do
        end select
    end subroutine Calculate_Ice_GCC_Derivative_Temperature_NonSegregation_m

    module subroutine Calculate_Ice_GCC_Derivative_Temperature_rhoW_scalar(self, arr_Temperature, rhoW, arr_Pw)
        implicit none
        class(Type_Ice_GCC), intent(inout) :: self
        real(real64), intent(in) :: arr_Temperature(:)
        real(real64), intent(in) :: rhoW
        real(real64), intent(in), optional :: arr_Pw(:)

        real(real64) :: Qs
        integer(int32) :: i, n

        Qs = self%WRF%thetaS
        n = size(arr_Temperature)

        select type (GCC => self%GCC)
        type is (Type_GCC_NonSegregation_Pa)
            !$omp parallel do schedule(guided) private(i)
            do i = 1, n
                self%D_Qice%pre(i) = self%WRF%Calculate_WRF_Derivative(-GCC%Calculate_GCC(arr_Temperature(i), rhoW)) * GCC%Calculate_GCC_Derivative(arr_Temperature(i), rhoW)
            end do
        type is (Type_GCC_Segregation_m)
            if (.not. present(arr_Pw)) stop 'arr_Pw is required'
            !$omp parallel do schedule(guided) private(i)
            do i = 1, n
                self%D_Qice%pre(i) = self%WRF%Calculate_WRF_Derivative(-GCC%Calculate_GCC(arr_Temperature(i), arr_Pw(i), rhoW)) * GCC%Calculate_GCC_Derivative(arr_Temperature(i), arr_Pw(i), rhoW)
            end do
        type is (Type_GCC_Segregation_Pa)
            if (.not. present(arr_Pw)) stop 'arr_Pw is required'
            !$omp parallel do schedule(guided) private(i)
            do i = 1, n
                self%D_Qice%pre(i) = self%WRF%Calculate_WRF_Derivative(-GCC%Calculate_GCC(arr_Temperature(i), arr_Pw(i), rhoW)) * GCC%Calculate_GCC_Derivative(arr_Temperature(i), arr_Pw(i), rhoW)
            end do
        end select

    end subroutine Calculate_Ice_GCC_Derivative_Temperature_rhoW_scalar

    module subroutine Calculate_Ice_GCC_Derivative_Temperature_rhoW_array(self, arr_Temperature, arr_rhoW, arr_Pw)
        implicit none
        class(Type_Ice_GCC), intent(inout) :: self
        real(real64), intent(in) :: arr_Temperature(:)
        real(real64), intent(in) :: arr_rhoW(:)
        real(real64), intent(in), optional :: arr_Pw(:)

        real(real64) :: Qs
        integer(int32) :: i, n

        Qs = self%WRF%thetaS
        n = size(arr_Temperature)

        select type (GCC => self%GCC)
        type is (Type_GCC_NonSegregation_Pa)
            !$omp parallel do schedule(guided) private(i)
            do i = 1, n
                self%D_Qice%pre(i) = self%WRF%Calculate_WRF_Derivative(-GCC%Calculate_GCC(arr_Temperature(i), arr_rhoW(i))) * GCC%Calculate_GCC_Derivative(arr_Temperature(i), arr_rhoW(i))
            end do
        type is (Type_GCC_Segregation_m)
            if (.not. present(arr_Pw)) stop 'arr_Pw is required'
            !$omp parallel do schedule(guided) private(i)
            do i = 1, n
                self%D_Qice%pre(i) = self%WRF%Calculate_WRF_Derivative(-GCC%Calculate_GCC(arr_Temperature(i), arr_Pw(i), arr_rhoW(i))) * GCC%Calculate_GCC_Derivative(arr_Temperature(i), arr_Pw(i), arr_rhoW(i))
            end do
        type is (Type_GCC_Segregation_Pa)
            if (.not. present(arr_Pw)) stop 'arr_Pw is required'
            !$omp parallel do schedule(guided) private(i)
            do i = 1, n
                self%D_Qice%pre(i) = self%WRF%Calculate_WRF_Derivative(-GCC%Calculate_GCC(arr_Temperature(i), arr_Pw(i), arr_rhoW(i))) * GCC%Calculate_GCC_Derivative(arr_Temperature(i), arr_Pw(i), arr_rhoW(i))
            end do
        end select

    end subroutine Calculate_Ice_GCC_Derivative_Temperature_rhoW_array

end submodule Calculate_Ice_GCC_Implementation
