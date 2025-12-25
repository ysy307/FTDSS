submodule(main_hydraulic) hydraulic_base
    implicit none
contains
    module subroutine initialize_type_hydraulic(self, input, active_region_ids)
        implicit none
        class(type_hydraulic), intent(inout) :: self
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: active_region_ids(:)

    end subroutine initialize_type_hydraulic

    module pure elemental subroutine compute_C_H(self, target_id, state, C_HT, C_HH)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: target_id
        type(type_state), intent(inout) :: state
        real(real64), intent(inout), optional :: C_HT
        real(real64), intent(inout), optional :: C_HH

    end subroutine compute_C_H

    module pure subroutine compute_D_H(self, target_id, state, D_HT, D_HH)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: target_id
        type(type_state), intent(inout) :: state
        real(real64), intent(inout), optional :: D_HT(:, :)
        real(real64), intent(inout), optional :: D_HH(:, :)

    end subroutine compute_D_H

    module pure subroutine compute_V_H(self, target_id, state, V_HT, V_HH)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: target_id
        type(type_state), intent(inout) :: state
        real(real64), intent(inout), optional :: V_HT(:)
        real(real64), intent(inout), optional :: V_HH(:)

    end subroutine compute_V_H

    module pure subroutine compute_R_H(self, target_id, state, R_H_C, R_H_D)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: target_id
        type(type_state), intent(inout) :: state
        real(real64), intent(inout) :: R_H_C
        real(real64), intent(inout) :: R_H_D(:)

    end subroutine compute_R_H
end submodule hydraulic_base
