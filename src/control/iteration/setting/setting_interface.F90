module control_iteration_setting
    use, intrinsic :: iso_fortran_env
    use :: module_core
    use :: control_iteration_convergence, only:type_convergence_control
    implicit none
    private

    public :: type_iteration_setting

    !>
    !> Iterator configuration container
    !>
    type :: type_iteration_setting
        !> Maximum number of iterations
        integer(int32), private :: max_iterations = 10
        !> Frequency of updating system matrices
        integer(int32), private :: update_frequency = 1
        !> Convergence control settings
        type(type_convergence_control), private :: convergence_control
    contains
        ! ---- Lifecycle ----
        procedure, public, pass(self) :: initialize => initialize_type_iteration_setting
        ! ---- Mutator ----
        procedure, public, pass(self) :: reset => reset_iteration_setting
        ! ---- Algorithm / Operation ----
        procedure, public, pass(self) :: check_convergence => check_convergence_setting
        procedure, public, pass(self) :: prime_conserved_residual => prime_conserved_residual_setting
        procedure, public, pass(self) :: check_conserved => check_conserved_setting
        procedure, public, pass(self) :: compute_error_norm => compute_error_norm_setting
        ! ---- Inquiry ----
        procedure, public, pass(self) :: is_conserved => is_conserved_setting
        ! ---- Getter ----
        procedure, public, pass(self) :: get_conserved_relaxation => get_conserved_relaxation_setting
        procedure, public, pass(self) :: get_conserved_gates => get_conserved_gates_setting
        procedure, public, pass(self) :: get_max_iterations => get_max_iterations_iteration_setting
        procedure, public, pass(self) :: get_update_frequency => get_update_frequency_iteration_setting
        procedure, public, pass(self) :: get_current_norm => get_current_norm_iteration_setting
        procedure, public, pass(self) :: get_tolerances => get_tolerances_iteration_setting
        procedure, public, pass(self) :: update_reference_values => update_reference_values_iteration_setting
        procedure, public, pass(self) :: set_residual_scale => set_residual_scale_iteration_setting
        procedure, public, pass(self) :: get_residual_floors => get_residual_floors_iteration_setting
        procedure, public, pass(self) :: local_error_block => local_error_block_iteration_setting
        procedure, public, pass(self) :: report_local_error_nodes => report_local_error_nodes_iteration_setting
        procedure, public, pass(self) :: commit_conserved_drift => commit_conserved_drift_iteration_setting
        ! ---- Meta / Utility ----
    end type type_iteration_setting

contains

    subroutine initialize_type_iteration_setting(self, config, reference_values)
        implicit none
        class(type_iteration_setting), intent(inout) :: self
        type(type_config_iteration_nonlinear), intent(in) :: config
        real(real64), intent(in), optional :: reference_values(:)

        self%max_iterations = config%max_iterations
        self%update_frequency = config%update_frequency

        call self%convergence_control%initialize(config, self%max_iterations, reference_values)

    end subroutine initialize_type_iteration_setting

    subroutine reset_iteration_setting(self)
        implicit none
        class(type_iteration_setting), intent(inout) :: self

        call self%convergence_control%reset()

    end subroutine reset_iteration_setting

    function check_convergence_setting(self, physics_type, nonlinear_iter, residual_vector, update_vector) result(is_ok)
        implicit none
        class(type_iteration_setting), intent(inout) :: self
        type(type_constant_id), intent(in) :: physics_type
        integer(int32), intent(in) :: nonlinear_iter
        real(real64), intent(in), optional :: residual_vector(:)
        real(real64), intent(in), optional :: update_vector(:)
        logical :: is_ok

        is_ok = self%convergence_control%check_convergence(physics_type, nonlinear_iter, residual_vector, update_vector)

    end function check_convergence_setting

    pure function is_conserved_setting(self) result(is_conserved)
        implicit none
        class(type_iteration_setting), intent(in) :: self
        logical :: is_conserved

        is_conserved = self%convergence_control%is_conserved()
    end function is_conserved_setting

    pure function get_conserved_relaxation_setting(self) result(omega)
        implicit none
        class(type_iteration_setting), intent(in) :: self
        real(real64) :: omega

        omega = self%convergence_control%get_conserved_relaxation()
    end function get_conserved_relaxation_setting

    subroutine prime_conserved_residual_setting(self, residual_thermal, residual_hydraulic, &
                                                check_thermal, check_hydraulic)
        implicit none
        class(type_iteration_setting), intent(inout) :: self
        real(real64), intent(in), optional :: residual_thermal(:)
        real(real64), intent(in), optional :: residual_hydraulic(:)
        logical, intent(in) :: check_thermal
        logical, intent(in) :: check_hydraulic

        call self%convergence_control%prime_conserved_residual(residual_thermal, residual_hydraulic, &
                                                               check_thermal, check_hydraulic)
    end subroutine prime_conserved_residual_setting

    subroutine get_residual_floors_iteration_setting(self, num_nodes, floor_thermal, floor_hydraulic)
        implicit none
        class(type_iteration_setting), intent(in) :: self
        integer(int32), intent(in) :: num_nodes
        real(real64), intent(inout) :: floor_thermal
        real(real64), intent(inout) :: floor_hydraulic

        call self%convergence_control%get_residual_floors(num_nodes, floor_thermal, floor_hydraulic)
    end subroutine get_residual_floors_iteration_setting

    subroutine set_residual_scale_iteration_setting(self, volume_total, dt, nodal_volume, &
                                                    dH_dT, drho_dp, u_thermal, u_hydraulic, &
                                                    atol_temperature_u, atol_pressure_u, rtol_u)
        implicit none
        class(type_iteration_setting), intent(inout) :: self
        real(real64), intent(in) :: volume_total
        real(real64), intent(in) :: dt
        real(real64), intent(in), optional :: nodal_volume(:)
        real(real64), intent(in), optional :: dH_dT(:)
        real(real64), intent(in), optional :: drho_dp(:)
        real(real64), intent(in), optional :: u_thermal(:)
        real(real64), intent(in), optional :: u_hydraulic(:)
        real(real64), intent(in), optional :: atol_temperature_u
        real(real64), intent(in), optional :: atol_pressure_u
        real(real64), intent(in), optional :: rtol_u

        call self%convergence_control%set_residual_scale(volume_total, dt, nodal_volume, &
                                                          dH_dT, drho_dp, u_thermal, u_hydraulic, &
                                                          atol_temperature_u, atol_pressure_u, rtol_u)
    end subroutine set_residual_scale_iteration_setting

    !> See control_iteration_convergence:local_error_block_convergence_control
    !> for the mathematical definition.
    function local_error_block_iteration_setting(self, physics_type, residual) result(e_local)
        implicit none
        class(type_iteration_setting), intent(in) :: self
        type(type_constant_id), intent(in) :: physics_type
        real(real64), intent(in) :: residual(:)
        real(real64) :: e_local

        e_local = self%convergence_control%local_error_block(physics_type, residual)
    end function local_error_block_iteration_setting

    subroutine report_local_error_nodes_iteration_setting(self, physics_type, residual, label, theta_w, theta_i, porosity)
        implicit none
        class(type_iteration_setting), intent(in) :: self
        type(type_constant_id), intent(in) :: physics_type
        real(real64), intent(in) :: residual(:)
        character(*), intent(in) :: label
        real(real64), intent(in), optional :: theta_w(:), theta_i(:), porosity(:)

        call self%convergence_control%report_local_error_nodes(physics_type, residual, label, theta_w, theta_i, porosity)
    end subroutine report_local_error_nodes_iteration_setting

    !> Fold the pending conserved-quantity drift into the cumulative budget;
    !> see control_iteration_convergence for the acceptance-time contract.
    subroutine commit_conserved_drift_iteration_setting(self)
        implicit none
        class(type_iteration_setting), intent(inout) :: self

        call self%convergence_control%commit_conserved_drift()
    end subroutine commit_conserved_drift_iteration_setting

    !> Gate values and verdicts of the last conserved check (all pass at <= 1).
    pure subroutine get_conserved_gates_setting(self, local_thermal, local_hydraulic, &
                                                balance_thermal, balance_hydraulic, &
                                                dq_effective, local_ok, balance_ok, dq_ok)
        implicit none
        class(type_iteration_setting), intent(in) :: self
        real(real64), intent(inout) :: local_thermal, local_hydraulic
        real(real64), intent(inout) :: balance_thermal, balance_hydraulic
        real(real64), intent(inout) :: dq_effective
        logical, intent(inout) :: local_ok, balance_ok, dq_ok

        call self%convergence_control%get_conserved_gates(local_thermal, local_hydraulic, &
                                                          balance_thermal, balance_hydraulic, &
                                                          dq_effective, local_ok, balance_ok, dq_ok)
    end subroutine get_conserved_gates_setting

    subroutine check_conserved_setting(self, enthalpy, density, residual_thermal, residual_hydraulic, &
                                       check_thermal, check_hydraulic, is_ok, is_diverged)
        implicit none
        class(type_iteration_setting), intent(inout) :: self
        real(real64), intent(in) :: enthalpy(:)
        real(real64), intent(in) :: density(:)
        real(real64), intent(in), optional :: residual_thermal(:)
        real(real64), intent(in), optional :: residual_hydraulic(:)
        logical, intent(in) :: check_thermal
        logical, intent(in) :: check_hydraulic
        logical, intent(inout) :: is_ok
        logical, intent(inout) :: is_diverged

        call self%convergence_control%check_conserved(enthalpy, density, residual_thermal, residual_hydraulic, &
                                                      check_thermal, check_hydraulic, is_ok, is_diverged)
    end subroutine check_conserved_setting

    subroutine compute_error_norm_setting(self, enthalpy_a, density_a, enthalpy_b, density_b, eps)
        implicit none
        class(type_iteration_setting), intent(in) :: self
        real(real64), intent(in) :: enthalpy_a(:)
        real(real64), intent(in) :: density_a(:)
        real(real64), intent(in) :: enthalpy_b(:)
        real(real64), intent(in) :: density_b(:)
        real(real64), intent(inout) :: eps

        call self%convergence_control%compute_error_norm(enthalpy_a, density_a, enthalpy_b, density_b, eps)
    end subroutine compute_error_norm_setting

    pure subroutine get_max_iterations_iteration_setting(self, max_iterations)
        implicit none
        class(type_iteration_setting), intent(in) :: self
        integer(int32), intent(inout) :: max_iterations

        max_iterations = self%max_iterations
    end subroutine get_max_iterations_iteration_setting

    pure subroutine get_update_frequency_iteration_setting(self, update_frequency)
        implicit none
        class(type_iteration_setting), intent(in) :: self
        integer(int32), intent(inout) :: update_frequency

        update_frequency = self%update_frequency
    end subroutine get_update_frequency_iteration_setting

    subroutine get_current_norm_iteration_setting(self, physics_type, criteria_type, norm_type, nonlinear_iter, current_norm)
        implicit none
        class(type_iteration_setting), intent(in) :: self
        type(type_constant_id), intent(in) :: physics_type
        type(type_constant_id), intent(in) :: criteria_type
        type(type_constant_id), intent(in) :: norm_type
        integer(int32), intent(in) :: nonlinear_iter
        real(real64), intent(inout) :: current_norm

        call self%convergence_control%get_current_norm(physics_type, criteria_type, norm_type, nonlinear_iter, current_norm)
    end subroutine get_current_norm_iteration_setting

    subroutine get_tolerances_iteration_setting(self, physics_type, absolute_tolerance, relative_tolerance)
        implicit none
        class(type_iteration_setting), intent(in) :: self
        type(type_constant_id), intent(in) :: physics_type
        real(real64), intent(inout), optional :: absolute_tolerance
        real(real64), intent(inout), optional :: relative_tolerance

        call self%convergence_control%get_tolerances(physics_type, absolute_tolerance, relative_tolerance)
    end subroutine get_tolerances_iteration_setting

    subroutine update_reference_values_iteration_setting(self, reference_values)
        implicit none
        class(type_iteration_setting), intent(inout) :: self
        real(real64), intent(in) :: reference_values(:)

        call self%convergence_control%update_reference_values(reference_values)
    end subroutine update_reference_values_iteration_setting

end module control_iteration_setting
