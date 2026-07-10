module physics_governing_hydraulic
    use, intrinsic :: iso_fortran_env
    use :: module_core
    use :: module_control, only:type_control
    use :: module_input, only:type_input, input_translator
    use :: module_linalg
    use :: module_constitutive, g => gravity_acceleration
    use :: physics_governing_base, only:type_assemble_workspace
    implicit none
    private

    public :: type_hydraulic

    type :: type_hydraulic
        private
        integer(int32) :: computation_type
        integer(int32) :: computation_dimension
        logical :: enable_vapor_transport = .true.
        logical :: enable_fringe_subcell_quadrature = .true.
        logical :: enable_fringe_K_averaging = .false.
        type(type_constitutive_manager) :: physics
    contains
        procedure, pass(self), public :: initialize => initialize_type_hydraulic
        procedure, pass(self), public :: destroy => destroy_type_hydraulic

        ! --- Assembly Procedures ---
        procedure, pass(self), public :: assemble_local => assemble_local_hydraulic
        procedure, pass(self), private :: assemble_local_picard => assemble_local_picard_hydraulic

        ! --- Coefficient Computation Procedures ---
        procedure, pass(self), public :: compute_diffusion_term => compute_diffusion_term_hydraulic
        procedure, pass(self), private :: compute_advective_term => compute_advective_term_hydraulic
        procedure, pass(self), private :: compute_diffusion_term_K_averaged => compute_diffusion_term_K_averaged_hydraulic

        ! --- Coupling Coefficient Procedures ---
        procedure, pass(self), private :: compute_coupling_mass_term => compute_coupling_mass_term_hydraulic
        procedure, pass(self), public :: compute_coupling_diffusion_term => compute_coupling_diffusion_term_hydraulic

        ! --- Helper Procedures ---
        procedure, pass(self), public :: calc_K_wT => calc_K_wT_hydraulic
        procedure, pass(self), public :: calc_K_wP => calc_K_wP_hydraulic
        procedure, pass(self), public :: calc_K_vT => calc_K_vT_hydraulic
        procedure, pass(self), public :: calc_K_vP => calc_K_vP_hydraulic
        procedure, pass(self), public :: is_vapor_transport_enabled => is_vapor_transport_enabled_hydraulic

        procedure, pass(self), public :: update_water_phases => update_water_phases_hydraulic
        procedure, pass(self), public :: calc_effective_density => calc_effective_density_hydraulic
        procedure, pass(self), public :: calc_effective_density_value => calc_effective_density_value_hydraulic
        procedure, pass(self), public :: calc_theta_value => calc_theta_value_hydraulic
        procedure, pass(self), public :: calc_cryo_suction => calc_cryo_suction_hydraulic

        procedure, pass(self), private :: compute_C_eq => compute_C_eq_hydraulic
        procedure, pass(self), private :: compute_transient_term_mixed => compute_transient_term_mixed_hydraulic
        procedure, pass(self), public :: calc_segregation_sink => calc_segregation_sink_hydraulic
    end type type_hydraulic

    interface
        module subroutine initialize_type_hydraulic(self, input, active_region_ids)
            implicit none
            class(type_hydraulic), intent(inout) :: self
            type(type_input), intent(in) :: input
            integer(int32), intent(in) :: active_region_ids(:)
        end subroutine initialize_type_hydraulic

        module subroutine destroy_type_hydraulic(self)
            implicit none
            class(type_hydraulic), intent(inout) :: self
        end subroutine destroy_type_hydraulic

        ! --- Assembly Interfaces ---
        module subroutine assemble_local_hydraulic(self, control, workspace, K_HH, K_HT, F_H)
            implicit none
            class(type_hydraulic), intent(in) :: self
            type(type_control), intent(in) :: control
            type(type_assemble_workspace), intent(inout) :: workspace
            type(type_matrix_dense), intent(inout), optional :: K_HH
            type(type_matrix_dense), intent(inout), optional :: K_HT
            type(type_vector_dp), intent(inout), optional :: F_H
        end subroutine assemble_local_hydraulic

        module subroutine assemble_local_picard_hydraulic(self, control, workspace, K_HH, K_HT, F_H)
            implicit none
            class(type_hydraulic), intent(in) :: self
            type(type_control), intent(in) :: control
            type(type_assemble_workspace), intent(inout) :: workspace
            type(type_matrix_dense), intent(inout), optional :: K_HH
            type(type_matrix_dense), intent(inout), optional :: K_HT
            type(type_vector_dp), intent(inout), optional :: F_H
        end subroutine assemble_local_picard_hydraulic

        ! --- Physics Term Interfaces ---
        module subroutine compute_diffusion_term_hydraulic(self, material_id, state, D_HH)
            implicit none
            class(type_hydraulic), intent(in) :: self
            integer(int32), intent(in) :: material_id
            type(type_state), intent(inout) :: state
            real(real64), intent(inout) :: D_HH(:, :)
        end subroutine compute_diffusion_term_hydraulic

        module subroutine compute_advective_term_hydraulic(self, material_id, state, V_H)
            implicit none
            class(type_hydraulic), intent(in) :: self
            integer(int32), intent(in) :: material_id
            type(type_state), intent(inout) :: state
            real(real64), intent(inout) :: V_H(:)
        end subroutine compute_advective_term_hydraulic

        !> Path-averaged diffusion coefficients D_HH, D_HT over a nodal temperature range.
        !>
        !> \[ \langle D \rangle = \frac{1}{T_{max}-T_{min}} \int_{T_{min}}^{T_{max}} D(T; P, \phi, \dots) \, dT \]
        !>
        !> with all state variables other than \(T\) frozen at the values carried by
        !> state_ref, and the water-phase equilibrium (update_water_phases) re-evaluated
        !> at every sample temperature. Replaces the pointwise evaluation \(D(T_{gp})\) in
        !> elements/subcells whose nodal \(T\) range straddles the steep impedance
        !> transition \(Q(T)\) near \(T_{high}(p_w)\) (see fringe_transition_active in
        !> hydraulic_matrix.F90), where the impedance \(10^{-\Omega Q(T)}\) is smooth in
        !> \(T\) but too steep for the standard low-order element Gauss rule to resolve.
        !>
        !> Assumptions: state_ref carries a consistent (P, porosity, ...) state;
        !> T_min <= T_max. Numerical guarantee: the underlying 5-point Gauss-Legendre
        !> rule on [T_min, T_max] integrates polynomials of degree <= 9 in T exactly;
        !> no closed-form error bound exists for the true (non-polynomial) impedance
        !> profile. Cost: O(1) - 5 update_water_phases + compute_diffusion_term
        !> (+ compute_coupling_diffusion_term when need_D_HT) evaluations, independent
        !> of mesh size. Failure behavior: none (pure evaluation, no iteration).
        module subroutine compute_diffusion_term_K_averaged_hydraulic(self, material_id, state_ref, T_min, T_max, &
                                                                       need_D_HT, D_HH_avg, D_HT_avg)
            implicit none
            class(type_hydraulic), intent(in) :: self
            integer(int32), intent(in) :: material_id
            !> Reference state supplying all fixed (non-temperature) fields.
            type(type_state), intent(in) :: state_ref
            !> Lower bound of the nodal temperature range [degC].
            real(real64), intent(in) :: T_min
            !> Upper bound of the nodal temperature range [degC]; T_max >= T_min.
            real(real64), intent(in) :: T_max
            !> Skip the D_HT average (left at 0) when the coupling flux is not needed.
            logical, intent(in) :: need_D_HT
            !> Path-averaged D_HH [m/s per Pa-equivalent, see compute_diffusion_term].
            real(real64), intent(inout) :: D_HH_avg
            !> Path-averaged D_HT [see compute_coupling_diffusion_term]; 0 if .not. need_D_HT.
            real(real64), intent(inout) :: D_HT_avg
        end subroutine compute_diffusion_term_K_averaged_hydraulic

        ! --- Coupling Coefficient Interfaces ---
        module subroutine compute_coupling_mass_term_hydraulic(self, material_id, state, C_HT)
            implicit none
            class(type_hydraulic), intent(in) :: self
            integer(int32), intent(in) :: material_id
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: C_HT
        end subroutine compute_coupling_mass_term_hydraulic

        module subroutine compute_coupling_diffusion_term_hydraulic(self, material_id, state, D_HT)
            implicit none
            class(type_hydraulic), intent(in) :: self
            integer(int32), intent(in) :: material_id
            type(type_state), intent(inout) :: state
            real(real64), intent(inout) :: D_HT(:, :)
        end subroutine compute_coupling_diffusion_term_hydraulic

        ! --- Helper Interfaces ---
        module subroutine calc_K_wT_hydraulic(self, target_id, state, K_wT)
            implicit none
            class(type_hydraulic), intent(in) :: self
            integer(int32), intent(in) :: target_id
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: K_wT
        end subroutine calc_K_wT_hydraulic

        module subroutine calc_K_wP_hydraulic(self, target_id, state, K_wP)
            implicit none
            class(type_hydraulic), intent(in) :: self
            integer(int32), intent(in) :: target_id
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: K_wP
        end subroutine calc_K_wP_hydraulic

        module subroutine calc_K_vT_hydraulic(self, target_id, state, K_vT)
            implicit none
            class(type_hydraulic), intent(in) :: self
            integer(int32), intent(in) :: target_id
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: K_vT
        end subroutine calc_K_vT_hydraulic

        module subroutine calc_K_vP_hydraulic(self, target_id, state, K_vP)
            implicit none
            class(type_hydraulic), intent(in) :: self
            integer(int32), intent(in) :: target_id
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: K_vP
        end subroutine calc_K_vP_hydraulic

        module pure function is_vapor_transport_enabled_hydraulic(self) result(enabled)
            implicit none
            class(type_hydraulic), intent(in) :: self
            logical :: enabled
        end function is_vapor_transport_enabled_hydraulic

        module subroutine update_water_phases_hydraulic(self, material_id, state)
            implicit none
            class(type_hydraulic), intent(in) :: self
            integer(int32), intent(in) :: material_id
            type(type_state), intent(inout) :: state
        end subroutine update_water_phases_hydraulic

        module subroutine calc_effective_density_hydraulic(self, material_id, state, bdf_coeffs, drho_dt)
            implicit none
            class(type_hydraulic), intent(in) :: self
            integer(int32), intent(in) :: material_id
            type(type_state), intent(in) :: state
            real(real64), intent(in) :: bdf_coeffs(:)
            real(real64), intent(inout) :: drho_dt
        end subroutine calc_effective_density_hydraulic

        !> Evaluate the pore-water effective density at the supplied state.
        !> \( \rho_{eff} = \rho_w \theta_w + \rho_{ice} \theta_{ice} + \rho_w \theta_v^{\star} \) [kg/m3].
        !> Plain (non-time-derivative) conserved storage quantity of the water-mass
        !> balance; phase contents must already be consistent with (T, p_w).
        module subroutine calc_effective_density_value_hydraulic(self, state, rho_eff)
            implicit none
            class(type_hydraulic), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: rho_eff
        end subroutine calc_effective_density_value_hydraulic

        !> Evaluate the mixed water-equivalent content Theta at the supplied state.
        !> \[ \Theta = \theta_l + \frac{\rho_i}{\rho_l}\theta_i + \theta_v \] [-]
        !> (volumetric liquid-water-equivalent fraction). This is the exact storage
        !> variable whose BDF derivative is assembled into the hydraulic residual by
        !> compute_transient_term_mixed; used to build the global mass-bias reference
        !> M_ref = \int_\Omega \Theta \, d\Omega for the mass-bias acceptance gate.
        !> Phase contents must already be consistent with (T, p_w).
        module subroutine calc_theta_value_hydraulic(self, state, theta)
            implicit none
            class(type_hydraulic), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: theta
        end subroutine calc_theta_value_hydraulic

        !> Cryogenic suction psi_cryo(T) [Pa] at the given material/state, used by
        !> the A1 Clapeyron-pressure-constraint closure to evaluate the equilibrium
        !> pressure P_eq(T) = -psi_cryo(T) prescribed at frozen-constrained nodes.
        module subroutine calc_cryo_suction_hydraulic(self, material_id, state, psi_cryo)
            implicit none
            class(type_hydraulic), intent(in) :: self
            integer(int32), intent(in) :: material_id
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: psi_cryo
        end subroutine calc_cryo_suction_hydraulic

        !> Compute equivalent specific moisture capacity C_eq = dTheta/dP [1/Pa].
        !> \[ C_{eq} = \frac{\partial\theta_l}{\partial P}
        !>           + \frac{\rho_i}{\rho_l}\frac{\partial\theta_i}{\partial P}
        !>           + \frac{\partial\theta_v}{\partial P} \]
        module subroutine compute_C_eq_hydraulic(self, material_id, state, C_eq)
            implicit none
            class(type_hydraulic), intent(in) :: self
            integer(int32), intent(in) :: material_id
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: C_eq
        end subroutine compute_C_eq_hydraulic

        !> Compute BDF approximation of dTheta/dt for Mixed formulation.
        !> \[ \frac{d\Theta}{dt} \approx \sum_j \alpha_j \Theta(t_{n+1-j}) \]
        !> where \(\Theta = \theta_l + (\rho_i/\rho_l)\theta_i + \theta_v\).
        module subroutine compute_transient_term_mixed_hydraulic(self, material_id, state, bdf_coeffs, dTheta_dt)
            implicit none
            class(type_hydraulic), intent(in) :: self
            integer(int32), intent(in) :: material_id
            type(type_state), intent(in) :: state
            real(real64), intent(in) :: bdf_coeffs(:)
            real(real64), intent(inout) :: dTheta_dt
        end subroutine compute_transient_term_mixed_hydraulic

        module subroutine calc_segregation_sink_hydraulic(self, material_id, state, dt, S_seg)
            implicit none
            class(type_hydraulic), intent(in) :: self
            integer(int32), intent(in) :: material_id
            type(type_state), intent(in) :: state
            real(real64), intent(in) :: dt
            real(real64), intent(inout) :: S_seg
        end subroutine calc_segregation_sink_hydraulic

    end interface

contains
    module subroutine destroy_type_hydraulic(self)
        implicit none
        class(type_hydraulic), intent(inout) :: self
    end subroutine destroy_type_hydraulic
end module physics_governing_hydraulic
