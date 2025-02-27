submodule(Calculate_Ice) Calculate_Ice_EXP_Implementation
    use, intrinsic :: iso_fortran_env, only: int32, real64
    implicit none
contains
    module function Construct_Type_Ice_EXP(phi, Tf, a, nsize) result(structure)
        use, intrinsic :: iso_fortran_env, only: real64
        use :: Allocate_Allocate, only:Allocate_Array
        implicit none
        real(real64), intent(in) :: phi
        real(real64), intent(in) :: Tf
        real(real64), intent(in) :: a
        integer(int32), intent(in) :: nsize
        class(Abstract_Ice), allocatable :: structure

        if (allocated(structure)) deallocate (structure)
        allocate (Type_Ice_EXP :: structure)

        select type (this => structure)
        type is (Type_Ice_EXP)
            this%phi = phi
            this%Tf = Tf
            this%a = a

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

            call Allocate_Array(this%D_Qice%old, nsize)
            call Allocate_Array(this%D_Qice%pre, nsize)
            call Allocate_Array(this%D_Qice%new, nsize)
            this%D_Qice%old(:) = 0.0d0
            this%D_Qice%pre(:) = 0.0d0
            this%D_Qice%new(:) = 0.0d0

            call Allocate_Array(this%Si%old, nsize)
            call Allocate_Array(this%Si%pre, nsize)
            call Allocate_Array(this%Si%new, nsize)
            this%Si%old(:) = 0.0d0
            this%Si%pre(:) = 0.0d0
            this%Si%new(:) = 0.0d0

        end select

    end function Construct_Type_Ice_EXP

    module function Construct_Type_Ice_EXP_minimum() result(structure)
        implicit none
        class(Abstract_Ice), allocatable :: structure

        if (allocated(structure)) deallocate (structure)
        allocate (Type_Ice_EXP :: structure)

    end function Construct_Type_Ice_EXP_minimum

    module function Calculate_Ice_EXP(self, Temperature) result(Qice)
        !$omp declare simd uniform(self, Temperature)
        implicit none
        class(Type_Ice_EXP), intent(inout) :: self
        real(real64), intent(in) :: Temperature
        real(real64) :: Qice

        if (Temperature < self%Tf) then
            Qice = self%phi * (1.0d0 - (1.0d0 - Temperature + self%Tf)**self%a)
        else
            Qice = 0.0d0
        end if

    end function Calculate_Ice_EXP

    module function Calculate_Ice_EXP_Derivative_Temperature(self, Temperature) result(D_Qice)
        !$omp declare simd uniform(self, Temperature)
        implicit none
        class(Type_Ice_EXP), intent(inout) :: self
        real(real64), intent(in) :: Temperature
        real(real64) :: D_Qice

        if (Temperature < self%Tf) then
            D_Qice = self%phi * self%a * (1.0d0 - Temperature + self%Tf)**(self%a - 1.0d0)
        else
            D_Qice = 0.0d0
        end if

    end function Calculate_Ice_EXP_Derivative_Temperature

    module subroutine Update_Ice_EXP(self, arr_Temperature)
        implicit none
        class(Type_Ice_EXP), intent(inout) :: self
        real(real64), intent(in) :: arr_Temperature(:)

        integer(int32) :: iN, n

        n = size(arr_Temperature)

        !$omp parallel do schedule(guided) private(iN)
        do iN = 1, n
            if (arr_Temperature(iN) < self%Tf) then
                self%Qice%pre(iN) = self%Calculate_Ice(arr_Temperature(iN))
            else
                self%Qice%pre(iN) = 0.0d0
            end if
        end do

    end subroutine Update_Ice_EXP

    module subroutine Update_Ice_EXP_Derivative_Temperature(self, arr_Temperature)
        implicit none
        class(Type_Ice_EXP), intent(inout) :: self
        real(real64), intent(in) :: arr_Temperature(:)

        integer(int32) :: iN, n

        n = size(arr_Temperature)

        !$omp parallel do schedule(guided) private(iN)
        do iN = 1, n
            if (arr_Temperature(iN) < self%Tf) then
                self%D_Qice%pre(iN) = self%Calculate_Ice_Derivative(arr_Temperature(iN))
            else
                self%Qice%pre(iN) = 0.0d0
            end if
        end do

    end subroutine Update_Ice_EXP_Derivative_Temperature

end submodule Calculate_Ice_EXP_Implementation
