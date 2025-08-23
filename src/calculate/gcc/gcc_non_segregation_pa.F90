submodule(Calculate_GCC) gcc_non_segregation_pa
    implicit none
contains

    module function construct_type_gcc_nonseg_pa(Tf, Lf) result(property)
        implicit none
        real(real64), intent(in) :: Tf
        real(real64), intent(in) :: Lf
        class(abst_gcc), allocatable :: property

        if (allocated(property)) deallocate (property)
        allocate (type_gcc_non_segregation_pa :: property)

        select type (this => property)
        type is (type_gcc_non_segregation_pa)
            this%Lf = Lf
            this%Tf = Tf
        end select

    end function construct_type_gcc_nonseg_pa

    module pure elemental function calc_gcc_nonseg_pa(self, state) result(suction)
        implicit none
        class(type_gcc_non_segregation_pa), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64) :: suction

        if (state%temperature <= self%Tf) then
            suction = -self%Lf * state%density_water * log((state%temperature + self%TtoK) / (self%Tf + self%TtoK))
        else
            suction = 0.0d0
        end if

    end function calc_gcc_nonseg_pa

    module pure elemental function deriv_gcc_nonseg_pa(self, state) result(suction_derivative)
        implicit none
        class(type_gcc_non_segregation_pa), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64) :: suction_derivative

        if (state%temperature <= self%Tf) then
            suction_derivative = -self%Lf * state%density_water / (state%temperature + self%TtoK)
        else
            suction_derivative = 0.0d0
        end if

    end function deriv_gcc_nonseg_pa

    module pure elemental function deriv_2nd_gcc_nonseg_pa(self, state) result(suction_derivative)
        implicit none
        class(type_gcc_non_segregation_pa), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64) :: suction_derivative

        if (state%temperature <= self%Tf) then
            suction_derivative = self%Lf * state%density_water / (state%temperature + self%TtoK)**2.0d0
        else
            suction_derivative = 0.0d0
        end if

    end function deriv_2nd_gcc_nonseg_pa

end submodule gcc_non_segregation_pa
