submodule(physics_models_gcc) gcc_segregation_m
    implicit none
contains

    module function construct_type_gcc_seg_m(Tf, Lf) result(property)
        implicit none
        real(real64), intent(in) :: Tf
        real(real64), intent(in) :: Lf
        class(abst_gcc), allocatable :: property

        if (allocated(property)) deallocate (property)
        allocate (type_gcc_segregation_m :: property)

        select type (this => property)
        type is (type_gcc_segregation_m)
            this%Lf = Lf
            this%Tf = Tf
        end select

    end function construct_type_gcc_seg_m

    module pure elemental function calc_gcc_seg_m(self, state) result(suction)
        implicit none
        class(type_gcc_segregation_m), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64) :: suction

        if (state%temperature <= self%Tf) then
            suction = ((state%density_ice / state%density_water - 1.0d0) * state%pressure - &
                       self%Lf * state%density_ice * log((state%temperature + self%TtoK) / (self%Tf + self%TtoK))) / &
                      (state%density_water * self%g)
        else
            suction = 0.0d0
        end if

    end function calc_gcc_seg_m

    module pure elemental function deriv_gcc_seg_m(self, state) result(suction_derivative)
        implicit none
        class(type_gcc_segregation_m), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64) :: suction_derivative

        if (state%temperature <= self%Tf) then
            suction_derivative = -self%Lf * state%density_ice / ((state%temperature + self%TtoK) * state%density_water * self%g)
        else
            suction_derivative = 0.0d0
        end if

    end function deriv_gcc_seg_m

    module pure elemental function deriv_2nd_gcc_seg_m(self, state) result(suction_derivative)
        implicit none
        class(type_gcc_segregation_m), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64) :: suction_derivative

        if (state%temperature <= self%Tf) then
            suction_derivative = self%Lf * state%density_ice / ((state%temperature + self%TtoK)**2.0d0 * state%density_water * self%g)
        else
            suction_derivative = 0.0d0
        end if

    end function deriv_2nd_gcc_seg_m

end submodule gcc_segregation_m
