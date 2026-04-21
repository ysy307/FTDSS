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

    !> Compute volumetric segregation sink term at a point.
    !>
    !> Using Mizoguchi-style SP with units [m/(s·K)], the liquid water intake
    !> rate per unit pore volume within the freezing fringe is
    !> \[ S_{seg} = SP_0 \, |\nabla T| \, f_{fringe}(T) \quad [1/s] \]
    !> where \( f_{fringe} \) is a smooth top-hat indicator on \([T_{low}, T_{high}]\).
    !> The sink is positive (liquid water removed from pore space).
    subroutine calc_segregation_sink(self, state, grad_T_magnitude, S_seg)
        implicit none
        class(type_segregation_potential), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(in) :: grad_T_magnitude
        real(real64), intent(inout) :: S_seg

        real(real64) :: temperature, Qw, Qi, Qi_seg, porosity_val, gas_space
        real(real64) :: f_fringe
        real(real64) :: T_half_width, dT_fringe, arg_low, arg_high
        real(real64) :: gas_frac, gas_gate, gas_eps_abs
        real(real64), parameter :: sharpness = 4.0d0
        real(real64), parameter :: gas_eps_rel = 0.005d0

        S_seg = 0.0d0
        if (self%SP0 <= 0.0d0) return
        if (grad_T_magnitude <= 0.0d0) return

        call state%temperature%get(temperature)
        call state%water_content%get(Qw)

        if (Qw <= 0.0d0) return

        ! Gas-phase saturation gate: C1-smooth ramp on available pore space.
        ! The hard cutoff (gas_space <= 0 => S_seg = 0) is retained as the lower
        ! end of a cubic smoothstep to preserve Jacobian-Residual consistency.
        call state%ice_content%get(Qi)
        call state%ice_content_seg%get(Qi_seg)
        call state%porosity%get(porosity_val)
        gas_space = porosity_val - Qi - Qi_seg - Qw
        gas_eps_abs = gas_eps_rel * max(porosity_val, 1.0d-6)
        if (gas_eps_abs <= 0.0d0) return
        gas_frac = max(0.0d0, min(1.0d0, gas_space / gas_eps_abs))
        gas_gate = gas_frac * gas_frac * (3.0d0 - 2.0d0 * gas_frac)
        if (gas_gate <= 0.0d0) return

        dT_fringe = self%T_fringe_high - self%T_fringe_low
        if (dT_fringe <= 0.0d0) return
        T_half_width = 0.5d0 * dT_fringe

        ! Smooth top-hat indicator on [T_fringe_low, T_fringe_high]
        arg_low = sharpness * (temperature - self%T_fringe_low) / T_half_width
        arg_high = sharpness * (self%T_fringe_high - temperature) / T_half_width
        arg_low = max(min(arg_low, 20.0d0), -20.0d0)
        arg_high = max(min(arg_high, 20.0d0), -20.0d0)
        f_fringe = 0.5d0 * (1.0d0 + tanh(arg_low)) * 0.5d0 * (1.0d0 + tanh(arg_high))

        ! Volumetric sink rate [1/s]: SP0 * |grad T| * f_fringe * gas_gate
        S_seg = self%SP0 * grad_T_magnitude * f_fringe * gas_gate

    end subroutine calc_segregation_sink

    pure function is_active_segregation_potential(self) result(active)
        implicit none
        class(type_segregation_potential), intent(in) :: self
        logical :: active

        active = (self%SP0 > 0.0d0)

    end function is_active_segregation_potential

end module models_segregation_potential
