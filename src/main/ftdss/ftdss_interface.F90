module main_ftdss
    use, intrinsic :: iso_fortran_env
    use :: mpi_f08
    use :: stdlib_logger
    use :: module_core
    use :: module_input, only:type_input
    use :: module_output, only:type_output

    use :: module_control, only:type_controls
    use :: module_domain, only:type_domain, abst_fe
    use :: module_boundary, only:abst_bc, type_bc_dirichlet
    use :: module_initial, only:type_ic_manager
    use :: module_field, only:type_jacobian_matrix, type_residual_vector
    use :: module_physics, only:g => gravity_acceleration
    use :: module_linalg

    use :: module_thermal, only:type_thermal
    use :: module_hydraulic, only:type_hydraulic
    use :: main_base, only:type_assemble_workspace

    use :: module_solver
    implicit none

    type :: type_ftdss
        type(type_domain) :: domain

        type(type_variable) :: porosity
        type(type_variable) :: temperature
        type(type_variable) :: pressure

        type(type_variable) :: Qw
        type(type_variable) :: Qi
        type(type_variable) :: Qa
        type(type_variable) :: Qv

        type(type_jacobian_matrix) :: K
        type(type_residual_vector) :: F
        type(type_residual_vector) :: u

        type(type_thermal) :: thermal
        type(type_hydraulic) :: hydraulic

        class(abst_solver), allocatable :: solver

        type(type_controls) :: controls
        type(type_output) :: output

    contains
        procedure, public, pass(self) :: initialize => initialize_type_ftdss
        procedure, public, pass(self) :: shift => shift_ftdss

        procedure, public, pass(self) :: calc_gradient => calc_gradient_ftdss
        procedure, public, pass(self) :: calc_gradient_temperature => calc_gradient_temperature_ftdss
        procedure, public, pass(self) :: calc_gradient_pressure => calc_gradient_pressure_ftdss

        procedure, public, pass(self) :: calc_water_flux => calc_water_flux_ftdss
        procedure, public, pass(self) :: calc_vapor_flux => calc_vapor_flux_ftdss

        ! --- Boundary Condition Procedures ---
        procedure, public, pass(self) :: apply_bc => apply_bc_ftdss
        procedure, private, pass(self) :: prescribe_essential_bc_generic
        procedure, private, pass(self) :: apply_natural_bc_generic
        procedure, private, pass(self) :: apply_essential_bc_generic

        !> ソルバー呼び出しルーチン
        procedure, public, pass(self) :: solve => solve_ftdss

        procedure, public, pass(self) :: set_state => set_state_ftdss

        procedure, public, pass(self) :: reflect_variables => reflect_variables_ftdss

        procedure, public, pass(self) :: update_variables => update_variables_ftdss
        procedure, public, pass(self) :: assemble_local => assemble_local_ftdss
        procedure, public, pass(self) :: assemble => assemble_ftdss
        procedure, private, pass(self) :: assemble_initialize => assemble_initialize_ftdss
        procedure, private, pass(self) :: assemble_finalize => assemble_finalize_ftdss

        !> 1タイムステップ分の計算を行う（非線形反復ループを含む）
        procedure, public, pass(self) :: solve_time_step => solve_time_step_ftdss
        procedure, private, pass(self) :: solve_time_step_initial_setup => solve_time_step_initial_setup_ftdss
        procedure, private, pass(self) :: solve_time_step_setup => solve_time_step_setup_ftdss
        procedure, private, pass(self) :: solve_time_step_check_convergence => solve_time_step_check_convergence_ftdss

        procedure, public, pass(self) :: output_fields => output_fields_ftdss
        procedure, public, pass(self) :: output_history => output_history_ftdss
    end type type_ftdss

    interface
        module subroutine initialize_type_ftdss(self)
            implicit none
            class(type_ftdss), intent(inout) :: self

        end subroutine initialize_type_ftdss

        module subroutine prescribe_essential_bc_generic(self, physics_type, current_time, variable)
            implicit none
            class(type_ftdss), intent(inout), target :: self
            integer(int32), intent(in) :: physics_type
            real(real64), intent(in) :: current_time
            type(type_variable), intent(inout) :: variable

        end subroutine prescribe_essential_bc_generic

        module subroutine apply_natural_bc_generic(self, physics_type, current_time, variable, dof_offset)
            implicit none
            class(type_ftdss), intent(inout), target :: self
            integer(int32), intent(in) :: physics_type
            real(real64), intent(in) :: current_time
            type(type_variable), intent(in) :: variable
            integer(int32), intent(in) :: dof_offset

        end subroutine apply_natural_bc_generic

        module subroutine apply_essential_bc_generic(self, physics_type, current_time, variable, dof_offset)
            implicit none
            class(type_ftdss), intent(inout), target :: self
            integer(int32), intent(in) :: physics_type
            real(real64), intent(in) :: current_time
            type(type_variable), intent(in) :: variable
            integer(int32), intent(in) :: dof_offset

        end subroutine apply_essential_bc_generic

        module subroutine apply_bc_ftdss(self, prescribed)
            implicit none
            class(type_ftdss), intent(inout) :: self
            logical, intent(in), optional :: prescribed

        end subroutine apply_bc_ftdss

        module subroutine solve_ftdss(self)
            implicit none
            class(type_ftdss), intent(inout) :: self

        end subroutine solve_ftdss

        module subroutine set_state_ftdss(self, node_id, element_id, state)
            implicit none
            class(type_ftdss), intent(inout) :: self
            integer(int32), intent(in) :: node_id
            integer(int32), intent(in) :: element_id
            type(type_state), intent(inout) :: state

        end subroutine set_state_ftdss

        module subroutine shift_ftdss(self)
            implicit none
            class(type_ftdss), intent(inout) :: self

        end subroutine shift_ftdss

        module subroutine update_variables_ftdss(self)
            implicit none
            class(type_ftdss), intent(inout) :: self

        end subroutine update_variables_ftdss

        module subroutine reflect_variables_ftdss(self)
            implicit none
            class(type_ftdss), intent(inout) :: self

        end subroutine reflect_variables_ftdss

        module subroutine calc_gradient_ftdss(self, values_vec, grad)
            implicit none
            class(type_ftdss), intent(inout) :: self
            real(real64), intent(in) :: values_vec(:)
            type(type_coordinate_array_dp), intent(inout) :: grad

        end subroutine calc_gradient_ftdss

        module subroutine calc_gradient_temperature_ftdss(self)
            implicit none
            class(type_ftdss), intent(inout) :: self

        end subroutine calc_gradient_temperature_ftdss

        module subroutine calc_gradient_pressure_ftdss(self)
            implicit none
            class(type_ftdss), intent(inout) :: self

        end subroutine calc_gradient_pressure_ftdss

        module subroutine calc_water_flux_ftdss(self, material_id, state, grad_T, grad_P, water_flux)
            implicit none
            class(type_ftdss), intent(inout) :: self
            integer(int32), intent(in) :: material_id
            type(type_state), intent(in) :: state
            type(type_coordinate_dp), intent(in) :: grad_T, grad_P
            type(type_coordinate_dp), intent(inout) :: water_flux

        end subroutine calc_water_flux_ftdss

        module subroutine calc_vapor_flux_ftdss(self, material_id, state, grad_T, grad_P, water_flux)
            implicit none
            class(type_ftdss), intent(inout) :: self
            integer(int32), intent(in) :: material_id
            type(type_state), intent(in) :: state
            type(type_coordinate_dp), intent(in) :: grad_T, grad_P
            type(type_coordinate_dp), intent(inout) :: water_flux

        end subroutine calc_vapor_flux_ftdss

        module subroutine assemble_local_ftdss(self, workspace, local_J_TT, local_J_TH, &
                                               local_J_HH, local_J_HT, local_R_T, local_R_H)
            implicit none
            class(type_ftdss), intent(inout) :: self
            type(type_assemble_workspace), intent(inout) :: workspace
            type(type_matrix_dense), intent(inout), optional :: local_J_TT, local_J_TH, local_J_HH, local_J_HT
            type(type_vector_dp), intent(inout), optional :: local_R_T, local_R_H

        end subroutine assemble_local_ftdss
        module subroutine assemble_initialize_ftdss(self, element_id, workspace, local_J_TT, local_J_TH, &
                                                    local_J_HH, local_J_HT, local_R_T, local_R_H)
            implicit none
            class(type_ftdss), intent(inout) :: self
            integer(int32), intent(in) :: element_id
            type(type_assemble_workspace), intent(inout) :: workspace
            type(type_matrix_dense), intent(inout), optional :: local_J_TT, local_J_TH, local_J_HH, local_J_HT
            type(type_vector_dp), intent(inout), optional :: local_R_T, local_R_H

        end subroutine assemble_initialize_ftdss

        module subroutine assemble_finalize_ftdss(self, workspace, local_J_TT, local_J_TH, &
                                                  local_J_HH, local_J_HT, local_R_T, local_R_H)
            implicit none
            class(type_ftdss), intent(inout) :: self
            type(type_assemble_workspace), intent(inout) :: workspace
            type(type_matrix_dense), intent(inout), optional :: local_J_TT, local_J_TH, local_J_HH, local_J_HT
            type(type_vector_dp), intent(inout), optional :: local_R_T, local_R_H

        end subroutine assemble_finalize_ftdss

        module subroutine assemble_ftdss(self)
            implicit none
            class(type_ftdss), intent(inout) :: self

        end subroutine assemble_ftdss

        module subroutine solve_time_step_initial_setup_ftdss(self)
            implicit none
            class(type_ftdss), intent(inout) :: self

        end subroutine solve_time_step_initial_setup_ftdss

        module subroutine solve_time_step_setup_ftdss(self, prescribe_bc)
            implicit none
            class(type_ftdss), intent(inout) :: self
            logical, intent(inout) :: prescribe_bc

        end subroutine solve_time_step_setup_ftdss

        module subroutine solve_time_step_check_convergence_ftdss(self)
            implicit none
            class(type_ftdss), intent(inout), target :: self

        end subroutine solve_time_step_check_convergence_ftdss

        module subroutine solve_time_step_ftdss(self, is_step_converged)
            implicit none
            class(type_ftdss), intent(inout) :: self
            logical, intent(inout) :: is_step_converged

        end subroutine solve_time_step_ftdss

        module subroutine output_fields_ftdss(self)
            implicit none
            class(type_ftdss), intent(inout) :: self

        end subroutine output_fields_ftdss

        module subroutine output_history_ftdss(self)
            implicit none
            class(type_ftdss), intent(inout) :: self

        end subroutine output_history_ftdss

    end interface

end module main_ftdss
