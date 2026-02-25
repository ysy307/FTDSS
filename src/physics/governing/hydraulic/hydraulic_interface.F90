module main_hydraulic
    use, intrinsic :: iso_fortran_env
    use :: module_core
    use :: module_control, only:type_control
    use :: module_input, only:type_input
    use :: module_physics, g => gravity_acceleration
    use :: module_linalg
    use :: governing_base, only:type_assemble_workspace
    implicit none
    private

    public :: type_hydraulic

    type :: type_hydraulic
        private
        integer(int32) :: computation_type
        integer(int32) :: computation_dimension
        type(type_physics_manager) :: physics
    contains
        procedure, pass(self), public :: initialize => initialize_type_hydraulic
        procedure, pass(self), public :: destroy => destroy_type_hydraulic

        ! --- Assembly Procedures ---
        procedure, pass(self), public :: assemble_local => assemble_local_hydraulic
        procedure, pass(self), private :: assemble_local_newton => assemble_local_newton_hydraulic
        procedure, pass(self), private :: assemble_local_picard => assemble_local_picard_hydraulic

        ! --- Coefficient Computation Procedures ---
        procedure, pass(self), private :: compute_mass_term => compute_mass_term_hydraulic
        procedure, pass(self), private :: compute_diffusion_term => compute_diffusion_term_hydraulic
        procedure, pass(self), private :: compute_advective_term => compute_advective_term_hydraulic
        procedure, pass(self), private :: compute_transient_term => compute_transient_term_hydraulic

        ! --- Helper Procedures ---
        procedure, pass(self), public :: calc_K_wT => calc_K_wT_hydraulic
        procedure, pass(self), public :: calc_K_wP => calc_K_wP_hydraulic
        procedure, pass(self), public :: calc_K_vT => calc_K_vT_hydraulic
        procedure, pass(self), public :: calc_K_vP => calc_K_vP_hydraulic

        procedure, pass(self), public :: update_water_phases => update_water_phases_hydraulic
        procedure, pass(self), public :: calc_effective_density => calc_effective_density_hydraulic
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
        module subroutine assemble_local_hydraulic(self, controls, workspace, K_HH, K_HT, F_H)
            implicit none
            class(type_hydraulic), intent(in) :: self
            type(type_control), intent(in) :: controls
            type(type_assemble_workspace), intent(inout) :: workspace
            type(type_matrix_dense), intent(inout), optional :: K_HH
            type(type_matrix_dense), intent(inout), optional :: K_HT
            type(type_vector_dp), intent(inout), optional :: F_H
        end subroutine assemble_local_hydraulic

        module subroutine assemble_local_newton_hydraulic(self, controls, workspace, K_HH, K_HT, F_H)
            implicit none
            class(type_hydraulic), intent(in) :: self
            type(type_control), intent(in) :: controls
            type(type_assemble_workspace), intent(inout) :: workspace
            type(type_matrix_dense), intent(inout), optional :: K_HH
            type(type_matrix_dense), intent(inout), optional :: K_HT
            type(type_vector_dp), intent(inout), optional :: F_H
        end subroutine assemble_local_newton_hydraulic

        module subroutine assemble_local_picard_hydraulic(self, controls, workspace, K_HH, K_HT, F_H)
            implicit none
            class(type_hydraulic), intent(in) :: self
            type(type_control), intent(in) :: controls
            type(type_assemble_workspace), intent(inout) :: workspace
            type(type_matrix_dense), intent(inout), optional :: K_HH
            type(type_matrix_dense), intent(inout), optional :: K_HT
            type(type_vector_dp), intent(inout), optional :: F_H
        end subroutine assemble_local_picard_hydraulic

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

    end interface

contains
    module subroutine destroy_type_hydraulic(self)
        implicit none
        class(type_hydraulic), intent(inout) :: self
    end subroutine destroy_type_hydraulic
end module main_hydraulic
