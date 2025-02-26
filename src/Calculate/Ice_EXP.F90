submodule(Calculate_Ice) Calculate_Ice_EXP_Implementation
    use, intrinsic :: iso_fortran_env, only: int32, real64
    implicit none
contains
    module subroutine Calculate_Ice_EXP(self, arr_Temperature)
        !> $Q_\mathrm{ice}(T) = \phi \left(1 - \left(1 - T + T_f\right)^a\right)$
        implicit none
        class(Type_Ice_EXP), intent(inout) :: self
        real(real64), intent(in) :: arr_Temperature(:)

        integer(int32) :: i, n

        n = size(arr_Temperature)

        !$omp parallel do schedule(guided) private(i)
        do i = 1, n
            if (arr_Temperature(i) < self%Tf) then
                self%Qice%pre(i) = self%phi * (1.0d0 - (1.0d0 - arr_Temperature(i) + self%Tf)**self%a)
            else
                self%Qice%pre(i) = 0.0d0
            end if
        end do

    end subroutine Calculate_Ice_EXP

    module subroutine Calculate_Ice_EXP_Derivative_Temperature(self, arr_Temperature)
        implicit none
        class(Type_Ice_EXP), intent(inout) :: self
        real(real64), intent(in) :: arr_Temperature(:)

        integer(int32) :: i, n

        n = size(arr_Temperature)

        !$omp parallel do schedule(guided) private(i)
        do i = 1, n
            if (arr_Temperature(i) < self%Tf) then
                self%D_Qice%pre(i) = self%phi * self%a * (1.0d0 - arr_Temperature(i) + self%Tf)**(self%a - 1.0d0)
            else
                self%Qice%pre(i) = 0.0d0
            end if
        end do

    end subroutine Calculate_Ice_EXP_Derivative_Temperature

end submodule Calculate_Ice_EXP_Implementation
