submodule(main_hydraulic) hydraulic_coefficients
    implicit none
contains

    module pure elemental subroutine calc_K_wT_hydraulic(self, target_id, state, K_wT)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: target_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: K_wT

        call self%physics%calc_KlT(target_id, state, K_wT)

    end subroutine calc_K_wT_hydraulic

    module pure elemental subroutine calc_K_wP_hydraulic(self, target_id, state, K_wP)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: target_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: K_wP

        call self%physics%calc_Kflh(target_id, state, K_wP)

    end subroutine calc_K_wP_hydraulic

    module pure elemental subroutine calc_K_vT_hydraulic(self, target_id, state, K_vT)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: target_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: K_vT

        call self%physics%calc_KvT(target_id, state, K_vT)

    end subroutine calc_K_vT_hydraulic

    module pure elemental subroutine calc_K_vP_hydraulic(self, target_id, state, K_vP)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: target_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: K_vP

        call self%physics%calc_Kvh(target_id, state, K_vP)

    end subroutine calc_K_vP_hydraulic

end submodule hydraulic_coefficients
