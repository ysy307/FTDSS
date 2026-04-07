!> Segregation Potential model for ice lens formation.
!>
!> Computes the segregation water flux (sink term) at the freezing fringe
!> using the Segregation Potential approach:
!> \( S_{seg} = SP_0 \, |\nabla T| \, f_{fringe}(T) \)
module models_segregation_potential
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core, only: type_state
    implicit none
    private

    public :: type_segregation_potential

    !> Segregation Potential model parameters
    type :: type_segregation_potential
        private
        real(real64) :: SP0 = 0.0d0
        real(real64) :: T_fringe_low = -2.0d0
        real(real64) :: T_fringe_high = 0.0d0
    contains
        procedure, public, pass(self) :: initialize => initialize_segregation_potential
        procedure, public, pass(self) :: calc_sink => calc_segregation_sink
        procedure, public, pass(self) :: is_active => is_active_segregation_potential
    end type type_segregation_potential

contains

    subroutine initialize_segregation_potential(self, SP0, T_fringe_low, T_fringe_high)
        implicit none
        class(type_segregation_potential), intent(inout) :: self
        real(real64), intent(in) :: SP0
        real(real64), intent(in), optional :: T_fringe_low
        real(real64), intent(in), optional :: T_fringe_high

        self%SP0 = SP0
        if (present(T_fringe_low)) self%T_fringe_low = T_fringe_low
        if (present(T_fringe_high)) self%T_fringe_high = T_fringe_high

    end subroutine initialize_segregation_potential

    !> Compute segregation sink term at a point.
    !>
    !> Mathematical definition:
    !> \( S_{seg} = SP_0 \, |\nabla T| \, f_{fringe}(T) \)
    !> where \( f_{fringe} \) is a smooth indicator for the freezing fringe.
    !> The sink is positive (water removal from pore space).
    subroutine calc_segregation_sink(self, state, grad_T_magnitude, S_seg)
        implicit none
        class(type_segregation_potential), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(in) :: grad_T_magnitude
        real(real64), intent(inout) :: S_seg

        real(real64) :: temperature, Qi, Qw
        real(real64) :: f_fringe
        real(real64) :: T_mid, T_half_width, arg_low, arg_high
        real(real64), parameter :: sharpness = 4.0d0

        S_seg = 0.0d0
        if (self%SP0 <= 0.0d0) return

        call state%temperature%get(temperature)
        call state%ice_content%get(Qi)
        call state%water_content%get(Qw)

        ! Fringe requires both ice and liquid water present
        if (Qi <= 0.0d0 .or. Qw <= 0.0d0) return

        ! Smooth fringe indicator using product of two tanh sigmoids
        T_mid = 0.5d0 * (self%T_fringe_low + self%T_fringe_high)
        T_half_width = 0.5d0 * (self%T_fringe_high - self%T_fringe_low)
        if (T_half_width <= 0.0d0) return

        ! Lower bound activation: rises from 0 to 1 as T increases above T_fringe_low
        arg_low = sharpness * (temperature - self%T_fringe_low) / T_half_width
        ! Upper bound activation: drops from 1 to 0 as T increases above T_fringe_high
        arg_high = sharpness * (self%T_fringe_high - temperature) / T_half_width

        ! Clamp arguments to avoid overflow in tanh
        arg_low = max(min(arg_low, 20.0d0), -20.0d0)
        arg_high = max(min(arg_high, 20.0d0), -20.0d0)

        f_fringe = 0.5d0 * (1.0d0 + tanh(arg_low)) * 0.5d0 * (1.0d0 + tanh(arg_high))

        S_seg = self%SP0 * grad_T_magnitude * f_fringe

    end subroutine calc_segregation_sink

    pure function is_active_segregation_potential(self) result(active)
        implicit none
        class(type_segregation_potential), intent(in) :: self
        logical :: active

        active = (self%SP0 > 0.0d0)

    end function is_active_segregation_potential

end module models_segregation_potential
