module main_thermal
    use, intrinsic :: iso_fortran_env
    use :: module_core
    use :: module_control, only:type_controls
    use :: module_input, only:type_input
    use :: module_physics, only:type_physics_manager, type_wrf_params, type_hcf_params, type_thc_dispersivity
    use :: main_base, only:type_assemble_workspace
    implicit none
    private

    public :: type_thermal

    type :: type_thermal
        private
        integer(int32) :: computation_type
        integer(int32) :: computation_dimension
        type(type_physics_manager) :: physics
    contains
        procedure, pass(self), public :: initialize => initialize_type_thermal
        procedure, pass(self), public :: destroy => destroy_type_thermal

        procedure, pass(self), public :: compute_C_T => compute_C_T
        procedure, pass(self), public :: compute_mass_term => compute_mass_term_thermal
        procedure, pass(self), public :: compute_diffusion_term => compute_diffusion_term_thermal
        procedure, pass(self), public :: compute_advective_term => compute_advective_term_thermal
        procedure, pass(self), public :: compute_latent_term => compute_latent_term_thermal

        procedure, pass(self), public :: compute_D_T => compute_D_T
        procedure, pass(self), public :: compute_V_T => compute_V_T
        procedure, pass(self), public :: compute_R_T => compute_R_T

        procedure, pass(self), public :: calc_density_water => calc_density_water_thermal
        procedure, pass(self), public :: calc_density_ice => calc_density_ice_thermal
        procedure, pass(self), public :: calc_density_vapor_saturation => calc_density_vapor_saturation_thermal
        procedure, pass(self), public :: deriv_inner_heat_capacity => deriv_inner_heat_capacity_thermal
        procedure, pass(self), public :: update_water_phases => update_water_phases_thermal
    end type type_thermal

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

        module pure elemental subroutine compute_C_T(self, target_material_id, controls, state, C_TT, C_TH)
            implicit none
            class(type_thermal), intent(in) :: self
            integer(int32), intent(in) :: target_material_id
            type(type_controls), intent(in) :: controls
            type(type_state), intent(inout) :: state
            real(real64), intent(inout), optional :: C_TT
            real(real64), intent(inout), optional :: C_TH

        end subroutine compute_C_T

        module subroutine compute_mass_term_thermal(self, target_material_id, state, C_TT)
            implicit none
            class(type_thermal), intent(in) :: self
            integer(int32), intent(in) :: target_material_id
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: C_TT
        end subroutine compute_mass_term_thermal

        module subroutine compute_D_T(self, target_material_id, controls, state, D_TT, D_TH)
            implicit none
            class(type_thermal), intent(in) :: self
            integer(int32), intent(in) :: target_material_id
            type(type_controls), intent(in) :: controls
            type(type_state), intent(inout) :: state
            real(real64), intent(inout), optional :: D_TT(:, :)
            real(real64), intent(inout), optional :: D_TH(:, :)

        end subroutine compute_D_T

        module subroutine compute_diffusion_term_thermal(self, target_material_id, state, D_TT)
            implicit none
            class(type_thermal), intent(in) :: self
            integer(int32), intent(in) :: target_material_id
            type(type_state), intent(inout) :: state
            real(real64), intent(inout) :: D_TT(:, :)

        end subroutine compute_diffusion_term_thermal

        module subroutine compute_advective_term_thermal(self, target_material_id, state, V_TT)
            implicit none
            class(type_thermal), intent(in) :: self
            integer(int32), intent(in) :: target_material_id
            type(type_state), intent(inout) :: state
            real(real64), intent(inout) :: V_TT(:)

        end subroutine compute_advective_term_thermal

        module subroutine compute_latent_term_thermal(self, target_material_id, state, L_TT)
            implicit none
            class(type_thermal), intent(in) :: self
            integer(int32), intent(in) :: target_material_id
            type(type_state), intent(inout) :: state
            real(real64), intent(inout) :: L_TT

        end subroutine compute_latent_term_thermal

        module subroutine compute_V_T(self, target_material_id, controls, state, V_TT, V_TH)
            implicit none
            class(type_thermal), intent(in) :: self
            integer(int32), intent(in) :: target_material_id
            type(type_controls), intent(in) :: controls
            type(type_state), intent(inout) :: state
            real(real64), intent(inout), optional :: V_TT(:)
            real(real64), intent(inout), optional :: V_TH(:)

        end subroutine compute_V_T

        module subroutine compute_R_T(self, target_material_id, controls, state, R_T_C, R_T_D)
            implicit none
            class(type_thermal), intent(in) :: self
            integer(int32), intent(in) :: target_material_id
            type(type_controls), intent(in) :: controls
            type(type_state), intent(inout) :: state
            real(real64), intent(inout) :: R_T_C
            real(real64), intent(inout) :: R_T_D(:)

        end subroutine compute_R_T

        module pure elemental subroutine calc_density_water_thermal(self, state, rho_water)
            implicit none
            class(type_thermal), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: rho_water

        end subroutine calc_density_water_thermal

        module pure elemental subroutine calc_density_ice_thermal(self, state, rho_ice)
            implicit none
            class(type_thermal), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: rho_ice

        end subroutine calc_density_ice_thermal

        module pure elemental subroutine calc_density_vapor_saturation_thermal(self, state, rho_vapor_sat)
            implicit none
            class(type_thermal), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: rho_vapor_sat

        end subroutine calc_density_vapor_saturation_thermal

        module pure elemental subroutine update_water_phases_thermal(self, material_id, state)
            implicit none
            class(type_thermal), intent(in) :: self
            integer(int32), intent(in) :: material_id
            type(type_state), intent(inout) :: state

        end subroutine update_water_phases_thermal

        module subroutine deriv_inner_heat_capacity_thermal(self, material_id, state, controls, dU_dt)
            implicit none
            class(type_thermal), intent(in) :: self
            integer(int32), intent(in) :: material_id
            type(type_state), intent(in) :: state
            type(type_controls), intent(in) :: controls
            real(real64), intent(inout) :: dU_dt

        end subroutine deriv_inner_heat_capacity_thermal

    end interface

contains

end module main_thermal
