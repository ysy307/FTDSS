submodule(calculate_gcc) gcc_non_segregation_m
    implicit none
contains

    module function construct_type_gcc_nonseg_m(Tf, Lf) result(property)
        implicit none
        real(real64), intent(in) :: Tf
        real(real64), intent(in) :: Lf
        class(abst_gcc), allocatable :: property

        if (allocated(property)) deallocate (property)
        allocate (type_gcc_non_segregation_m :: property)

        select type (this => property)
        type is (type_gcc_non_segregation_m)
            this%Lf = Lf
            this%Tf = Tf
        end select

    end function construct_type_gcc_nonseg_m

    module pure elemental function calc_gcc_nonseg_m(self, state) result(suction)
        implicit none
        class(type_gcc_non_segregation_m), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64) :: suction

        if (state%temperature <= self%Tf) then
            suction = -self%Lf * log((state%temperature + self%TtoK) / (self%Tf + self%TtoK)) / self%g
        else
            suction = 0.0d0
        end if

    end function calc_gcc_nonseg_m

    module pure elemental function deriv_gcc_nonseg_m(self, state) result(suction_derivative)
        implicit none
        class(type_gcc_non_segregation_m), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64) :: suction_derivative

        if (state%temperature <= self%Tf) then
            suction_derivative = -self%Lf / ((state%temperature + self%TtoK) * self%g)
        else
            suction_derivative = 0.0d0
        end if
    end function deriv_gcc_nonseg_m

    module pure elemental function deriv_2nd_gcc_nonseg_m(self, state) result(suction_derivative)
        implicit none
        class(type_gcc_non_segregation_m), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64) :: suction_derivative

        if (state%temperature <= self%Tf) then
            suction_derivative = self%Lf / ((state%temperature + self%TtoK)**2.0d0 * self%g)
        else
            suction_derivative = 0.0d0
        end if
    end function deriv_2nd_gcc_nonseg_m

end submodule gcc_non_segregation_m
