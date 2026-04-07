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
        type(type_constitutive_manager) :: physics
    contains
        procedure, pass(self), public :: initialize => initialize_type_hydraulic
        procedure, pass(self), public :: destroy => destroy_type_hydraulic

        ! --- Assembly Procedures ---
        procedure, pass(self), public :: assemble_local => assemble_local_hydraulic
        procedure, pass(self), private :: assemble_local_newton => assemble_local_newton_hydraulic
        procedure, pass(self), private :: assemble_local_picard => assemble_local_picard_hydraulic
        procedure, pass(self), private :: assemble_element => assemble_element_hydraulic

        ! --- Coefficient Computation Procedures ---
        procedure, pass(self), private :: compute_mass_term => compute_mass_term_hydraulic
        procedure, pass(self), public :: compute_diffusion_term => compute_diffusion_term_hydraulic
        procedure, pass(self), private :: compute_advective_term => compute_advective_term_hydraulic
        procedure, pass(self), private :: compute_transient_term => compute_transient_term_hydraulic

        ! --- Coupling Coefficient Procedures ---
        procedure, pass(self), private :: compute_coupling_mass_term => compute_coupling_mass_term_hydraulic
        procedure, pass(self), public :: compute_coupling_diffusion_term => compute_coupling_diffusion_term_hydraulic

        ! --- Helper Procedures ---
        procedure, pass(self), public :: calc_K_wT => calc_K_wT_hydraulic
        procedure, pass(self), public :: calc_K_wP => calc_K_wP_hydraulic
        procedure, pass(self), public :: calc_K_vT => calc_K_vT_hydraulic
        procedure, pass(self), public :: calc_K_vP => calc_K_vP_hydraulic

        procedure, pass(self), public :: update_water_phases => update_water_phases_hydraulic
        procedure, pass(self), public :: calc_effective_density => calc_effective_density_hydraulic

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

        module subroutine assemble_local_newton_hydraulic(self, control, workspace, K_HH, K_HT, F_H)
            implicit none
            class(type_hydraulic), intent(in) :: self
            type(type_control), intent(in) :: control
            type(type_assemble_workspace), intent(inout) :: workspace
            type(type_matrix_dense), intent(inout), optional :: K_HH
            type(type_matrix_dense), intent(inout), optional :: K_HT
            type(type_vector_dp), intent(inout), optional :: F_H
        end subroutine assemble_local_newton_hydraulic

        module subroutine assemble_local_picard_hydraulic(self, control, workspace, K_HH, K_HT, F_H)
            implicit none
            class(type_hydraulic), intent(in) :: self
            type(type_control), intent(in) :: control
            type(type_assemble_workspace), intent(inout) :: workspace
            type(type_matrix_dense), intent(inout), optional :: K_HH
            type(type_matrix_dense), intent(inout), optional :: K_HT
            type(type_vector_dp), intent(inout), optional :: F_H
        end subroutine assemble_local_picard_hydraulic

        !> @brief Assemble element Jacobian \(J^e\) and residual \(R^e\) for Mixed-form Newton-Raphson.
        !> Explicit Gauss integration loop with per-GP physics evaluation via type_state.
        !> Applies diagonal min cutoff to \(C_{eq}\) and \(D_{HH}\), then row equilibration.
        !>
        !> \[ R_i^e = \int N_i \frac{d\Theta}{dt}\,d\Omega
        !>          + \int \nabla N_i \cdot (D_{HH}\nabla P + D_{HT}\nabla T + V_H)\,d\Omega \]
        !> \[ J^e_{ii} = \int N_i\,\alpha_0 C_{eq}\,d\Omega
        !>            + \int \nabla N_i \cdot D_{HH}\nabla N_i\,d\Omega \]
        !> \[ J^e_{ij} = \int \nabla N_i \cdot D_{HH}\nabla N_j\,d\Omega \quad (i \neq j) \]
        module subroutine assemble_element_hydraulic(self, material_id, bdf_coeffs, workspace, J_elem, R_elem)
            implicit none
            class(type_hydraulic), intent(in) :: self
            integer(int32), intent(in) :: material_id
            real(real64), intent(in) :: bdf_coeffs(:)
            type(type_assemble_workspace), intent(inout) :: workspace
            real(real64), intent(inout) :: J_elem(:, :)
            real(real64), intent(inout) :: R_elem(:)
        end subroutine assemble_element_hydraulic

        ! --- Physics Term Interfaces ---
        module subroutine compute_mass_term_hydraulic(self, material_id, state, C_HH)
            implicit none
            class(type_hydraulic), intent(in) :: self
            integer(int32), intent(in) :: material_id
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: C_HH
        end subroutine compute_mass_term_hydraulic

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

        module subroutine compute_transient_term_hydraulic(self, material_id, state, bdf_coeffs, drho_dt)
            implicit none
            class(type_hydraulic), intent(in) :: self
            integer(int32), intent(in) :: material_id
            type(type_state), intent(in) :: state
            real(real64), intent(in) :: bdf_coeffs(:)
            real(real64), intent(inout) :: drho_dt
        end subroutine compute_transient_term_hydraulic

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

        module subroutine calc_segregation_sink_hydraulic(self, material_id, state, S_seg)
            implicit none
            class(type_hydraulic), intent(in) :: self
            integer(int32), intent(in) :: material_id
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: S_seg
        end subroutine calc_segregation_sink_hydraulic

    end interface

contains
    module subroutine destroy_type_hydraulic(self)
        implicit none
        class(type_hydraulic), intent(inout) :: self
    end subroutine destroy_type_hydraulic
end module physics_governing_hydraulic
