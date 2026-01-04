module main_hydraulic
    use, intrinsic :: iso_fortran_env
    use :: module_core
    use :: module_input, only:type_input
    use :: module_physics, only:type_physics_manager
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
        procedure, pass(self), public :: compute_C_H => compute_C_H
        procedure, pass(self), public :: compute_D_H => compute_D_H
        procedure, pass(self), public :: compute_V_H => compute_V_H
        procedure, pass(self), public :: compute_R_H => compute_R_H
        procedure, pass(self), public :: calc_K_wT => calc_K_wT_hydraulic
        procedure, pass(self), public :: calc_K_wP => calc_K_wP_hydraulic
        procedure, pass(self), public :: calc_K_vT => calc_K_vT_hydraulic
        procedure, pass(self), public :: calc_K_vP => calc_K_vP_hydraulic
    end type type_hydraulic

    !! 一通り計算するのに必要なのが，T, P, dT/dt, dP/dt, grad_H, q_w, q_v

    interface
        module subroutine initialize_type_hydraulic(self, input, active_region_ids)
            implicit none
            class(type_hydraulic), intent(inout) :: self
            type(type_input), intent(in) :: input
            integer(int32), intent(in) :: active_region_ids(:)

        end subroutine initialize_type_hydraulic

        module pure elemental subroutine compute_C_H(self, target_id, state, C_HH, C_HT)
            implicit none
            class(type_hydraulic), intent(in) :: self
            integer(int32), intent(in) :: target_id
            type(type_state), intent(inout) :: state
            real(real64), intent(inout), optional :: C_HH
            real(real64), intent(inout), optional :: C_HT

        end subroutine compute_C_H

        module pure subroutine compute_D_H(self, target_id, state, D_HH, D_HT)
            implicit none
            class(type_hydraulic), intent(in) :: self
            integer(int32), intent(in) :: target_id
            type(type_state), intent(inout) :: state
            real(real64), intent(inout), optional :: D_HH(:, :)
            real(real64), intent(inout), optional :: D_HT(:, :)

        end subroutine compute_D_H

        module pure subroutine compute_V_H(self, target_id, state, V_HH, V_HT)
            implicit none
            class(type_hydraulic), intent(in) :: self
            integer(int32), intent(in) :: target_id
            type(type_state), intent(inout) :: state
            real(real64), intent(inout), optional :: V_HH(:)
            real(real64), intent(inout), optional :: V_HT(:)

        end subroutine compute_V_H

        module pure subroutine compute_R_H(self, target_id, state, R_H_C, R_H_D)
            implicit none
            class(type_hydraulic), intent(in) :: self
            integer(int32), intent(in) :: target_id
            type(type_state), intent(inout) :: state
            real(real64), intent(inout) :: R_H_C
            real(real64), intent(inout) :: R_H_D(:)

        end subroutine compute_R_H

        module pure elemental subroutine calc_K_wT_hydraulic(self, target_id, state, K_wT)
            implicit none
            class(type_hydraulic), intent(in) :: self
            integer(int32), intent(in) :: target_id
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: K_wT

        end subroutine calc_K_wT_hydraulic

        module pure elemental subroutine calc_K_wP_hydraulic(self, target_id, state, K_wP)
            implicit none
            class(type_hydraulic), intent(in) :: self
            integer(int32), intent(in) :: target_id
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: K_wP

        end subroutine calc_K_wP_hydraulic

        module pure elemental subroutine calc_K_vT_hydraulic(self, target_id, state, K_vT)
            implicit none
            class(type_hydraulic), intent(in) :: self
            integer(int32), intent(in) :: target_id
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: K_vT

        end subroutine calc_K_vT_hydraulic

        module pure elemental subroutine calc_K_vP_hydraulic(self, target_id, state, K_vP)
            implicit none
            class(type_hydraulic), intent(in) :: self
            integer(int32), intent(in) :: target_id
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: K_vP

        end subroutine calc_K_vP_hydraulic
    end interface

contains

end module main_hydraulic
