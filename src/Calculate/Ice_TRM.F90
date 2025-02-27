submodule(Calculate_Ice) Calculate_Ice_TRM_Implementation
    use, intrinsic :: iso_fortran_env, only: int32, real64
    implicit none

contains
    module function Construct_Type_Ice_TRM(Lf, Tf, nsize) result(structure)
        use :: Allocate_Allocate, only:Allocate_Array
        implicit none
        real(real64), intent(in) :: Lf
        real(real64), intent(in) :: Tf
        integer(int32), intent(in) :: nsize
        class(Abstract_Ice), allocatable :: structure

        if (allocated(structure)) deallocate (structure)
        allocate (Type_Ice_TRM :: structure)

        select type (this => structure)
        type is (Type_Ice_TRM)
            this%Lf = Lf
            this%Tf = Tf

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

        end select

    end function Construct_Type_Ice_TRM

    module function Construct_Type_Ice_TRM_minimum() result(structure)
        implicit none
        class(Abstract_Ice), allocatable :: structure

        if (allocated(structure)) deallocate (structure)
        allocate (Type_Ice_TRM :: structure)

    end function Construct_Type_Ice_TRM_minimum

    module subroutine Update_Ice_TRM_scalar(self, arr_Temperature, arr_Si, rhoW, arr_Cp)
        implicit none
        class(Type_Ice_TRM), intent(inout) :: self
        type(Variables), intent(inout) :: arr_Temperature
        type(Variables), intent(inout) :: arr_Si
        real(real64), intent(in) :: rhoW
        real(real64), intent(in) :: arr_Cp(:)

        integer(int32) :: iN, n
        real(real64) :: C, tmpSi

        n = size(arr_Temperature%pre(:))

        !$omp parallel do schedule(guided) private(iN, C, tmpSi)
        do iN = 1, n
            C = arr_Cp(iN) / (self%Qw%pre(iN) * rhoW * self%Lf)
            tmpSi = self%Si%old(iN) + C * (self%Tf - arr_Temperature%new(iN))

            if (tmpSi <= 0.0d0 .and. self%Si%old(iN) == 0.0d0) then
                self%Si%new(iN) = 0.0d0
            else if (tmpSi >= 1.0d0 .and. self%Si%old(iN) == 1.0d0) then
                self%Si%new(iN) = 1.0d0
            else if (0.0d0 < tmpSi .and. tmpSi < 1.0d0 .and. self%Si%old(iN) <= 1.0d0) then
                arr_Temperature%new(iN) = self%Tf
                self%Si%new(iN) = tmpSi
            else if (0.0d0 < self%Si%old(iN) .and. self%Si%old(iN) < 1.0d0 .and. tmpSi >= 1.0d0) then
                arr_Temperature%new(iN) = self%Tf + (1.0d0 - tmpSi) / C
                self%Si%new(iN) = 1.0d0
            end if
        end do

    end subroutine Update_Ice_TRM_scalar

    module subroutine Update_Ice_TRM_array(self, arr_Temperature, arr_Si, arr_rhoW, arr_Cp)
        implicit none
        class(Type_Ice_TRM), intent(inout) :: self
        type(Variables), intent(inout) :: arr_Temperature
        type(Variables), intent(inout) :: arr_Si
        real(real64), intent(in) :: arr_rhoW(:)
        real(real64), intent(in) :: arr_Cp(:)

        integer(int32) :: iN, n
        real(real64) :: C, tmpSi

        n = size(arr_Temperature%pre(:))

        !$omp parallel do schedule(guided) private(iN, C, tmpSi)
        do iN = 1, n
            C = arr_Cp(iN) / (self%Qw%pre(iN) * arr_rhoW(iN) * self%Lf)
            tmpSi = self%Si%old(iN) + C * (self%Tf - arr_Temperature%new(iN))

            if (tmpSi <= 0.0d0 .and. self%Si%old(iN) == 0.0d0) then
                self%Si%new(iN) = 0.0d0
            else if (tmpSi >= 1.0d0 .and. self%Si%old(iN) == 1.0d0) then
                self%Si%new(iN) = 1.0d0
            else if (0.0d0 < tmpSi .and. tmpSi < 1.0d0 .and. self%Si%old(iN) <= 1.0d0) then
                arr_Temperature%new(iN) = self%Tf
                self%Si%new(iN) = tmpSi
            else if (0.0d0 < self%Si%old(iN) .and. self%Si%old(iN) < 1.0d0 .and. tmpSi >= 1.0d0) then
                arr_Temperature%new(iN) = self%Tf + (1.0d0 - tmpSi) / C
                self%Si%new(iN) = 1.0d0
            end if
        end do

    end subroutine Update_Ice_TRM_array

end submodule Calculate_Ice_TRM_Implementation
