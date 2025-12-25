module main_thermal
    use, intrinsic :: iso_fortran_env
    use :: module_core
    use :: module_input, only:type_input
    use :: module_physics, only:type_physics_manager, type_wrf_params, type_hcf_params, type_thc_dispersity
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
        procedure, pass(self), public :: compute_C_T => compute_C_T
        procedure, pass(self), public :: compute_D_T => compute_D_T
        procedure, pass(self), public :: compute_V_T => compute_V_T
        procedure, pass(self), public :: compute_R_T => compute_R_T
    end type type_thermal

    !! 一通り計算するのに必要なのが，T, P, dT/dt, dP/dt, grad_T, q_w, q_v

    interface
        module subroutine initialize_type_thermal(self, input, active_region_ids)
            implicit none
            class(type_thermal), intent(inout) :: self
            type(type_input), intent(in) :: input
            integer(int32), intent(in) :: active_region_ids(:)

        end subroutine initialize_type_thermal

        module pure elemental subroutine compute_C_T(self, target_id, state, C_TT, C_TH)
            implicit none
            class(type_thermal), intent(in) :: self
            integer(int32), intent(in) :: target_id
            type(type_state), intent(inout) :: state
            real(real64), intent(inout), optional :: C_TT
            real(real64), intent(inout), optional :: C_TH

        end subroutine compute_C_T

        module pure subroutine compute_D_T(self, target_id, state, D_TT, D_TH)
            implicit none
            class(type_thermal), intent(in) :: self
            integer(int32), intent(in) :: target_id
            type(type_state), intent(inout) :: state
            real(real64), intent(inout), optional :: D_TT(:, :)
            real(real64), intent(inout), optional :: D_TH(:, :)

        end subroutine compute_D_T

        module pure subroutine compute_V_T(self, target_id, state, V_TT, V_TH)
            implicit none
            class(type_thermal), intent(in) :: self
            integer(int32), intent(in) :: target_id
            type(type_state), intent(inout) :: state
            real(real64), intent(inout), optional :: V_TT(:)
            real(real64), intent(inout), optional :: V_TH(:)

        end subroutine compute_V_T

        module pure subroutine compute_R_T(self, target_id, state, R_T_C, R_T_D)
            implicit none
            class(type_thermal), intent(in) :: self
            integer(int32), intent(in) :: target_id
            type(type_state), intent(inout) :: state
            real(real64), intent(inout) :: R_T_C
            real(real64), intent(inout) :: R_T_D(:)

        end subroutine compute_R_T
    end interface

contains

end module main_thermal
