module app_ftcms
    use, intrinsic :: iso_fortran_env
    use :: omp_lib
    use :: mpi_f08

    use :: stdlib_optval, only:optval
    use :: stdlib_logger
    use :: module_core
    use :: module_input, only:type_input, input_translator
    use :: module_output, only:type_output_manager

    use :: module_control, only:type_control
    use :: module_domain
    ! use :: module_boundary, only:
    use :: module_initial, only:type_ic_manager
    use :: module_system, only:type_jacobian_matrix, type_residual_vector
    use :: module_constitutive, only:g => gravity_acceleration
    use :: module_linalg

    use :: module_governing
    use :: module_solver
    implicit none

    type :: type_ftcms
        type(type_domain) :: domain

        type(type_variable) :: porosity
        type(type_variable) :: temperature
        type(type_variable) :: pressure

        type(type_variable) :: Qw
        type(type_variable) :: Qi
        type(type_variable) :: Qi_seg
        type(type_variable) :: Qa
        type(type_variable) :: Qv

        type(type_jacobian_matrix) :: K
        type(type_residual_vector) :: F
        type(type_residual_vector) :: du

        type(type_thermal) :: thermal
        type(type_hydraulic) :: hydraulic

        type(type_bc_manager) :: bc(PHYSICS_TYPES%NUM_ID)

        class(abst_solver), allocatable :: solver
        class(abst_solver), allocatable :: solver_thermal

        integer(int32) :: current_physics_id = 0
        integer(int32) :: thermal_start_dof = 0
        integer(int32) :: hydraulic_start_dof = 0
        logical :: hydraulic_has_dirichlet_bc = .false.
        logical :: thermal_has_dirichlet_bc = .false.

        ! Reference mean pressure captured from the initial condition.
        ! Used to pin the null-mode of all-Neumann hydraulic systems without
        ! distorting the absolute pressure level (WRF relies on absolute P).
        logical :: hydraulic_ref_mean_set = .false.
        real(real64) :: hydraulic_ref_mean = 0.0d0

        ! DOF column scaling factors for variable non-dimensionalization
        real(real64), allocatable :: col_scale(:)
        real(real64), allocatable :: col_scale_inv(:)

        type(type_control) :: control
        type(type_output_manager) :: output

    contains
        ! ---- Lifecycle ----
        ! initialize, destroy, reset, etc.
        procedure, public, pass(self) :: initialize => initialize_type_ftcms
        procedure, public, pass(self) :: destroy => destroy_type_ftcms

        ! ---- Mutator ----
        ! set_XXX, increment_XXX, update_XXX, etc.

        ! ---- Algorithm / Operation ----
        ! compute_XXX, check_XXX, solve_XXX, etc.

        ! ---- Inquiry ----
        ! is_XXX, has_XXX, should_XXX, etc.

        ! ---- Getter ----
        ! get_XXX, etc.

        ! ---- Meta / Utility ----
        ! display, to_string, etc.

        ! ---- Operator ----

        procedure, public, pass(self) :: shift => shift_ftcms

        procedure, public, pass(self) :: calc_gradient => calc_gradient_ftcms
        procedure, public, pass(self) :: calc_gradient_temperature => calc_gradient_temperature_ftcms
        procedure, public, pass(self) :: calc_gradient_pressure => calc_gradient_pressure_ftcms

        procedure, public, pass(self) :: calc_water_flux => calc_water_flux_ftcms
        procedure, public, pass(self) :: calc_vapor_flux => calc_vapor_flux_ftcms

        ! --- Boundary Condition Procedures ---
        procedure, public, pass(self) :: apply_bc => apply_bc_ftcms
        procedure, public, pass(self) :: prescribe_dirichlet => prescribe_dirichlet_ftcms
        procedure, private, pass(self) :: freeze_physics_dofs => freeze_physics_dofs_ftcms
        procedure, private, pass(self) :: zero_frozen_increment => zero_frozen_increment_ftcms
        procedure, private, pass(self) :: prescribe_essential_bc_generic
        procedure, private, pass(self) :: apply_natural_bc_generic
        procedure, private, pass(self) :: apply_essential_bc_generic

        procedure, public, pass(self) :: solve => solve_ftcms

        procedure, public, pass(self) :: set_state => set_state_ftcms
        procedure, private, pass(self) :: set_states_from_connectivity => set_states_from_connectivity_ftcms
        procedure, public, pass(self) :: update_physical_properties => update_physical_properties_ftcms
        procedure, private, pass(self) :: update_physical_properties_bulk => update_physical_properties_bulk_ftcms

        procedure, public, pass(self) :: reflect_variables => reflect_variables_ftcms
        procedure, private, pass(self) :: update_nodal_phases => update_nodal_phases_ftcms

        procedure, public, pass(self) :: update_variables => update_variables_ftcms
        procedure, public, pass(self) :: update_segregation_ice => update_segregation_ice_ftcms
        procedure, public, pass(self) :: assemble_local => assemble_local_ftcms
        procedure, public, pass(self) :: assemble => assemble_ftcms
        procedure, private, pass(self) :: assemble_initialize => assemble_initialize_ftcms
        procedure, private, pass(self) :: assemble_destroy => assemble_destroy_ftcms

        procedure, private, pass(self) :: get_variable_increment => get_variable_increment_ftcms
        procedure, private, pass(self) :: get_variable_residual => get_variable_residual_ftcms

        procedure, public, pass(self) :: reset => reset_ftcms

        !> Solve a single time step including the nonlinear iteration loop
        procedure, public, pass(self) :: solve_time_step => solve_time_step_ftcms
        procedure, private, pass(self) :: solve_time_step_staggered => solve_time_step_staggered_ftcms
        procedure, private, pass(self) :: solve_time_step_initial_setup => solve_time_step_initial_setup_ftcms
        procedure, private, pass(self) :: solve_time_step_setup => solve_time_step_setup_ftcms
        procedure, private, pass(self) :: solve_time_step_check_convergence => solve_time_step_check_convergence_ftcms

        procedure, public, pass(self) :: output_fields => output_fields_ftcms
        procedure, public, pass(self) :: output_history => output_history_ftcms

        procedure, public, pass(self) :: is_active_thermal => is_active_thermal_ftcms
        procedure, public, pass(self) :: is_active_hydraulic => is_active_hydraulic_ftcms

        procedure, public, pass(self) :: run => run_ftcms

    end type type_ftcms

    interface
        module subroutine initialize_type_ftcms(self)
            implicit none
            class(type_ftcms), intent(inout) :: self

        end subroutine initialize_type_ftcms

        module subroutine prescribe_essential_bc_generic(self, physics_type, current_time, variable)
            implicit none
            class(type_ftcms), intent(inout), target :: self
            type(type_constant_id), intent(in) :: physics_type
            real(real64), intent(in) :: current_time
            type(type_variable), intent(inout) :: variable
        end subroutine prescribe_essential_bc_generic

        module subroutine apply_natural_bc_generic(self, physics_type, current_time, variable, dof_offset)
            implicit none
            class(type_ftcms), intent(inout), target :: self
            type(type_constant_id), intent(in) :: physics_type
            real(real64), intent(in) :: current_time
            type(type_variable), intent(in) :: variable
            integer(int32), intent(in) :: dof_offset

        end subroutine apply_natural_bc_generic

        module subroutine apply_essential_bc_generic(self, physics_type, current_time, variable, dof_offset)
            implicit none
            class(type_ftcms), intent(inout), target :: self
            type(type_constant_id), intent(in) :: physics_type
            real(real64), intent(in) :: current_time
            type(type_variable), intent(in) :: variable
            integer(int32), intent(in) :: dof_offset

        end subroutine apply_essential_bc_generic

        module subroutine apply_bc_ftcms(self, prescribed)
            implicit none
            class(type_ftcms), intent(inout) :: self
            logical, intent(in), optional :: prescribed

        end subroutine apply_bc_ftcms

        module subroutine prescribe_dirichlet_ftcms(self)
            implicit none
            class(type_ftcms), intent(inout) :: self

        end subroutine prescribe_dirichlet_ftcms

        module subroutine freeze_physics_dofs_ftcms(self, physics_type)
            implicit none
            class(type_ftcms), intent(inout) :: self
            type(type_constant_id), intent(in) :: physics_type
        end subroutine freeze_physics_dofs_ftcms

        module subroutine zero_frozen_increment_ftcms(self, frozen_physics)
            import :: type_ftcms, type_constant_id
            implicit none
            class(type_ftcms), intent(inout) :: self
            type(type_constant_id), intent(in) :: frozen_physics
        end subroutine zero_frozen_increment_ftcms

        module subroutine solve_ftcms(self)
            import :: type_ftcms
            implicit none
            class(type_ftcms), intent(inout) :: self
        end subroutine solve_ftcms

        module subroutine set_state_ftcms(self, node_id, element_id, state, calc_physics)
            implicit none
            class(type_ftcms), intent(inout) :: self
            integer(int32), intent(in) :: node_id
            integer(int32), intent(in) :: element_id
            type(type_state), intent(inout) :: state
            logical, intent(in), optional :: calc_physics
        end subroutine set_state_ftcms

        module subroutine set_states_from_connectivity_ftcms(self, connectivity, element_id, states, calc_physics)
            implicit none
            class(type_ftcms), intent(inout) :: self
            integer(int32), intent(in) :: connectivity(:)
            integer(int32), intent(in) :: element_id
            type(type_state), intent(inout) :: states(:)
            logical, intent(in), optional :: calc_physics
        end subroutine set_states_from_connectivity_ftcms

        module subroutine update_physical_properties_ftcms(self, material_id, state)
            implicit none
            class(type_ftcms), intent(inout) :: self
            integer(int32), intent(in) :: material_id
            type(type_state), intent(inout) :: state
        end subroutine update_physical_properties_ftcms

        module subroutine update_physical_properties_bulk_ftcms(self, material_id, states)
            implicit none
            class(type_ftcms), intent(inout) :: self
            integer(int32), intent(in) :: material_id
            type(type_state), intent(inout) :: states(:)
        end subroutine update_physical_properties_bulk_ftcms

        module subroutine shift_ftcms(self)
            implicit none
            class(type_ftcms), intent(inout) :: self

        end subroutine shift_ftcms

        module subroutine update_variables_ftcms(self)
            implicit none
            class(type_ftcms), intent(inout) :: self

        end subroutine update_variables_ftcms

        module subroutine update_segregation_ice_ftcms(self)
            implicit none
            class(type_ftcms), intent(inout) :: self
        end subroutine update_segregation_ice_ftcms

        module subroutine reflect_variables_ftcms(self, step_scale)
            implicit none
            class(type_ftcms), intent(inout) :: self
            real(real64), intent(in), optional :: step_scale

        end subroutine reflect_variables_ftcms

        module subroutine update_nodal_phases_ftcms(self)
            implicit none
            class(type_ftcms), intent(inout) :: self
        end subroutine update_nodal_phases_ftcms

        module subroutine calc_gradient_ftcms(self, values_vec, grad)
            implicit none
            class(type_ftcms), intent(inout) :: self
            real(real64), intent(in) :: values_vec(:)
            type(type_coordinate_array_dp), intent(inout) :: grad

        end subroutine calc_gradient_ftcms

        module subroutine calc_gradient_temperature_ftcms(self)
            implicit none
            class(type_ftcms), intent(inout) :: self

        end subroutine calc_gradient_temperature_ftcms

        module subroutine calc_gradient_pressure_ftcms(self)
            implicit none
            class(type_ftcms), intent(inout) :: self

        end subroutine calc_gradient_pressure_ftcms

        module subroutine calc_water_flux_ftcms(self, material_id, state, grad_T, grad_P, water_flux)
            implicit none
            class(type_ftcms), intent(inout) :: self
            integer(int32), intent(in) :: material_id
            type(type_state), intent(in) :: state
            type(type_coordinate_dp), intent(in) :: grad_T, grad_P
            type(type_coordinate_dp), intent(inout) :: water_flux

        end subroutine calc_water_flux_ftcms

        module subroutine calc_vapor_flux_ftcms(self, material_id, state, grad_T, grad_P, water_flux)
            implicit none
            class(type_ftcms), intent(inout) :: self
            integer(int32), intent(in) :: material_id
            type(type_state), intent(in) :: state
            type(type_coordinate_dp), intent(in) :: grad_T, grad_P
            type(type_coordinate_dp), intent(inout) :: water_flux

        end subroutine calc_vapor_flux_ftcms

        module subroutine assemble_local_ftcms(self, workspace, local_K_TT, local_K_TH, &
                                               local_K_HH, local_K_HT, local_F_T, local_F_H)
            implicit none
            class(type_ftcms), intent(inout) :: self
            type(type_assemble_workspace), intent(inout) :: workspace
            type(type_matrix_dense), intent(inout), optional :: local_K_TT, local_K_TH, local_K_HH, local_K_HT
            type(type_vector_dp), intent(inout), optional :: local_F_T, local_F_H

        end subroutine assemble_local_ftcms
        module subroutine assemble_initialize_ftcms(self, element_id, workspace, local_K_TT, local_K_TH, &
                                                    local_K_HH, local_K_HT, local_F_T, local_F_H, &
                                                    coordinates, connectivity)
            implicit none

            class(type_ftcms), intent(inout) :: self
            integer(int32), intent(in) :: element_id
            type(type_assemble_workspace), intent(inout) :: workspace
            type(type_matrix_dense), intent(inout), optional :: local_K_TT, local_K_TH, local_K_HH, local_K_HT
            type(type_vector_dp), intent(inout), optional :: local_F_T, local_F_H
            real(real64), allocatable, intent(inout) :: coordinates(:, :)
            integer(int32), pointer, contiguous, intent(inout), optional :: connectivity(:)
        end subroutine assemble_initialize_ftcms

        module subroutine assemble_destroy_ftcms(self, workspace, local_K_TT, local_K_TH, &
                                                 local_K_HH, local_K_HT, local_F_T, local_F_H)
            implicit none
            class(type_ftcms), intent(inout) :: self
            type(type_assemble_workspace), intent(inout) :: workspace
            type(type_matrix_dense), intent(inout), optional :: local_K_TT, local_K_TH, local_K_HH, local_K_HT
            type(type_vector_dp), intent(inout), optional :: local_F_T, local_F_H

        end subroutine assemble_destroy_ftcms

        module subroutine get_variable_increment_ftcms(self, variable_id, variable)
            implicit none
            class(type_ftcms), intent(inout) :: self
            type(type_constant_id), intent(in) :: variable_id
            real(real64), intent(inout), allocatable :: variable(:)

        end subroutine get_variable_increment_ftcms

        module subroutine get_variable_residual_ftcms(self, variable_id, variable)
            implicit none
            class(type_ftcms), intent(inout) :: self
            type(type_constant_id), intent(in) :: variable_id
            real(real64), intent(inout), allocatable :: variable(:)

        end subroutine get_variable_residual_ftcms

        module subroutine reset_ftcms(self)
            implicit none
            class(type_ftcms), intent(inout) :: self

        end subroutine reset_ftcms

        module subroutine assemble_ftcms(self)
            implicit none
            class(type_ftcms), intent(inout) :: self

        end subroutine assemble_ftcms

        module subroutine solve_time_step_initial_setup_ftcms(self)
            implicit none
            class(type_ftcms), intent(inout) :: self

        end subroutine solve_time_step_initial_setup_ftcms

        module subroutine solve_time_step_setup_ftcms(self, prescribe_bc)
            implicit none
            class(type_ftcms), intent(inout) :: self
            logical, intent(inout) :: prescribe_bc

        end subroutine solve_time_step_setup_ftcms

        module subroutine solve_time_step_check_convergence_ftcms(self, target_physics)
            implicit none
            class(type_ftcms), intent(inout), target :: self
            type(type_constant_id), intent(in), optional :: target_physics

        end subroutine solve_time_step_check_convergence_ftcms

        module subroutine solve_time_step_ftcms(self, is_step_converged)
            implicit none
            class(type_ftcms), intent(inout) :: self
            logical, intent(inout) :: is_step_converged

        end subroutine solve_time_step_ftcms

        module subroutine solve_time_step_staggered_ftcms(self, is_step_converged)
            implicit none
            class(type_ftcms), intent(inout) :: self
            logical, intent(inout) :: is_step_converged
        end subroutine solve_time_step_staggered_ftcms

        module subroutine output_fields_ftcms(self)
            implicit none
            class(type_ftcms), intent(inout) :: self

        end subroutine output_fields_ftcms

        module subroutine output_history_ftcms(self)
            implicit none
            class(type_ftcms), intent(inout) :: self

        end subroutine output_history_ftcms

        module subroutine run_ftcms(self)
            implicit none
            class(type_ftcms), intent(inout) :: self

        end subroutine run_ftcms

        module subroutine destroy_type_ftcms(self)
            implicit none
            class(type_ftcms), intent(inout) :: self

        end subroutine destroy_type_ftcms

        module function is_active_thermal_ftcms(self) result(is_active)
            implicit none
            class(type_ftcms), intent(in) :: self
            logical :: is_active

        end function is_active_thermal_ftcms

        module function is_active_hydraulic_ftcms(self) result(is_active)
            implicit none
            class(type_ftcms), intent(in) :: self
            logical :: is_active

        end function is_active_hydraulic_ftcms

    end interface

end module app_ftcms
