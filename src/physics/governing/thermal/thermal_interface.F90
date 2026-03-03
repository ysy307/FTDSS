module governing_thermal
    use, intrinsic :: iso_fortran_env
    use :: module_core
    use :: module_control, only:type_control
    use :: module_input, only:type_input, input_translator
    use :: module_linalg
    use :: module_constitutive, only:type_constitutive_manager
    use :: governing_base, only:type_assemble_workspace
    implicit none
    private

    public :: type_thermal

    ! 定数定義（可読性のため）
    integer, parameter :: SCHEME_TANGENT = 0 ! 厳密な微分（NR法推奨）
    integer, parameter :: SCHEME_SECANT = 1 ! 差分近似（Picard法/相変化安定化用）

    type :: type_thermal
        private
        integer(int32) :: computation_type
        integer(int32) :: computation_dimension
        type(type_constitutive_manager) :: physics
    contains
        procedure, pass(self), public :: initialize => initialize_type_thermal
        procedure, pass(self), public :: destroy => destroy_type_thermal

        procedure, pass(self), public :: assemble_local => assemble_local_thermal
        procedure, pass(self), private :: assemble_local_newton => assemble_local_newton_thermal
        procedure, pass(self), private :: assemble_local_picard => assemble_local_picard_thermal

        procedure, pass(self), private :: compute_mass_term => compute_mass_term_thermal
        procedure, pass(self), private :: compute_diffusion_term => compute_diffusion_term_thermal
        procedure, pass(self), private :: compute_advective_term => compute_advective_term_thermal
        procedure, pass(self), private :: compute_latent_term => compute_latent_term_thermal
        procedure, pass(self), private :: compute_transient_term => compute_transient_term_thermal
        procedure, pass(self), private :: compute_history_term => compute_history_term_thermal

        procedure, pass(self), public :: calc_enthalpy_density => calc_enthalpy_density_thermal
        procedure, pass(self), public :: calc_density_water => calc_density_water_thermal
        procedure, pass(self), public :: calc_density_ice => calc_density_ice_thermal
        procedure, pass(self), public :: calc_density_vapor_saturation => calc_density_vapor_saturation_thermal
        procedure, pass(self), public :: update_water_phases => update_water_phases_thermal
    end type type_thermal

    interface
        module subroutine calc_enthalpy_density_thermal(self, material_id, state, U)
            implicit none
            class(type_thermal), intent(in) :: self
            integer(int32), intent(in) :: material_id
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: U
        end subroutine calc_enthalpy_density_thermal

        module subroutine calc_density_water_thermal(self, state, rho_water)
            implicit none
            class(type_thermal), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: rho_water

        end subroutine calc_density_water_thermal

        module subroutine calc_density_ice_thermal(self, state, rho_ice)
            implicit none
            class(type_thermal), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: rho_ice

        end subroutine calc_density_ice_thermal

        module subroutine calc_density_vapor_saturation_thermal(self, state, rho_vapor_sat)
            implicit none
            class(type_thermal), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: rho_vapor_sat

        end subroutine calc_density_vapor_saturation_thermal

        module subroutine update_water_phases_thermal(self, material_id, state)
            implicit none
            class(type_thermal), intent(in) :: self
            integer(int32), intent(in) :: material_id
            type(type_state), intent(inout) :: state

        end subroutine update_water_phases_thermal

        module subroutine compute_transient_term_thermal(self, material_id, state, bdf_coeffs, dU_dt)
            implicit none
            class(type_thermal), intent(in) :: self
            integer(int32), intent(in) :: material_id
            type(type_state), intent(in) :: state
            real(real64), intent(in) :: bdf_coeffs(:)
            real(real64), intent(inout) :: dU_dt

        end subroutine compute_transient_term_thermal
        module subroutine compute_mass_term_thermal(self, material_id, state, C_TT, scheme_opt)
            implicit none
            class(type_thermal), intent(in) :: self
            integer(int32), intent(in) :: material_id
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: C_TT
            integer(int32), intent(in), optional :: scheme_opt

        end subroutine compute_mass_term_thermal
        
        module subroutine compute_diffusion_term_thermal(self, material_id, state, D_TT)
            implicit none
            class(type_thermal), intent(in) :: self
            integer(int32), intent(in) :: material_id
            type(type_state), intent(inout) :: state
            real(real64), intent(inout) :: D_TT(:, :)
            type(type_state_thc) :: lambda

        end subroutine compute_diffusion_term_thermal

        module subroutine compute_advective_term_thermal(self, material_id, state, V_TT)
            implicit none
            class(type_thermal), intent(in) :: self
            integer(int32), intent(in) :: material_id
            type(type_state), intent(inout) :: state
            real(real64), intent(inout) :: V_TT(:)

        end subroutine compute_advective_term_thermal

        module subroutine compute_latent_term_thermal(self, material_id, state, L_TT)
            implicit none
            class(type_thermal), intent(in) :: self
            integer(int32), intent(in) :: material_id
            type(type_state), intent(inout) :: state
            real(real64), intent(inout) :: L_TT

        end subroutine compute_latent_term_thermal

        module subroutine compute_history_term_thermal(self, material_id, state, bdf_coeffs, history_term)
            implicit none
            class(type_thermal), intent(in) :: self
            integer(int32), intent(in) :: material_id
            type(type_state), intent(in) :: state
            real(real64), intent(in) :: bdf_coeffs(:)
            real(real64), intent(inout) :: history_term

        end subroutine compute_history_term_thermal
    end interface

    interface
        module subroutine initialize_type_thermal(self, input, active_region_ids)
            implicit none
            class(type_thermal), intent(inout) :: self
            type(type_input), intent(in) :: input
            integer(int32), intent(in) :: active_region_ids(:)

        end subroutine initialize_type_thermal

        module subroutine destroy_type_thermal(self)
            implicit none
            class(type_thermal), intent(inout) :: self

        end subroutine destroy_type_thermal

        module subroutine assemble_local_thermal(self, control, workspace, K_TT, K_TH, F_T)
            implicit none
            class(type_thermal), intent(in) :: self
            type(type_control), intent(in) :: control
            type(type_assemble_workspace), intent(inout) :: workspace
            type(type_matrix_dense), intent(inout), optional :: K_TT
            type(type_matrix_dense), intent(inout), optional :: K_TH
            type(type_vector_dp), intent(inout), optional :: F_T

        end subroutine assemble_local_thermal

        module subroutine assemble_local_newton_thermal(self, control, workspace, K_TT, K_TH, F_T)
            implicit none
            class(type_thermal), intent(in) :: self
            type(type_control), intent(in) :: control
            type(type_assemble_workspace), intent(inout) :: workspace
            type(type_matrix_dense), intent(inout), optional :: K_TT
            type(type_matrix_dense), intent(inout), optional :: K_TH
            type(type_vector_dp), intent(inout), optional :: F_T

        end subroutine assemble_local_newton_thermal

        module subroutine assemble_local_picard_thermal(self, control, workspace, K_TT, K_TH, F_T)
            implicit none
            class(type_thermal), intent(in) :: self
            type(type_control), intent(in) :: control
            type(type_assemble_workspace), intent(inout) :: workspace
            type(type_matrix_dense), intent(inout), optional :: K_TT
            type(type_matrix_dense), intent(inout), optional :: K_TH
            type(type_vector_dp), intent(inout), optional :: F_T

        end subroutine assemble_local_picard_thermal

    end interface

contains

end module governing_thermal
