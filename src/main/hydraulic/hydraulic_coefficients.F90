submodule(main_hydraulic) hydraulic_coefficients
    implicit none
contains

    module subroutine calc_K_wT_hydraulic(self, target_id, state, K_wT)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: target_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: K_wT

        call self%physics%calc_KlT(target_id, state, K_wT)

    end subroutine calc_K_wT_hydraulic

    module subroutine calc_K_wP_hydraulic(self, target_id, state, K_wP)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: target_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: K_wP

        call self%physics%calc_Kflh(target_id, state, K_wP)

    end subroutine calc_K_wP_hydraulic

    module subroutine calc_K_vT_hydraulic(self, target_id, state, K_vT)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: target_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: K_vT

        call self%physics%calc_KvT(target_id, state, K_vT)

    end subroutine calc_K_vT_hydraulic

    module subroutine calc_K_vP_hydraulic(self, target_id, state, K_vP)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: target_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: K_vP

        call self%physics%calc_Kvh(target_id, state, K_vP)

    end subroutine calc_K_vP_hydraulic

    module subroutine update_water_phases_hydraulic(self, material_id, state)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(inout) :: state

        call self%physics%update_water_phases(material_id, state)

    end subroutine update_water_phases_hydraulic

    module subroutine calc_effective_density_hydraulic(self, material_id, state, bdf_coeffs, drho_dt)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        real(real64), intent(in) :: bdf_coeffs(:)
        real(real64), intent(inout) :: drho_dt

        type(type_state) :: local_state
        real(real64), pointer, dimension(:), contiguous :: temperature_history
        real(real64), pointer, dimension(:), contiguous :: pressure_history

        real(real64) :: Qw, Qi, Qv
        real(real64) :: rho_w, rho_i
        real(real64) :: Uj
        integer(int32) :: j, n

        call state%temperature_history%get(temperature_history)
        call state%pressure_history%get(pressure_history)

        n = size(bdf_coeffs)
        drho_dt = 0.0d0
        call local_state%copy(state)

        do j = 1, n
            call local_state%temperature%set(temperature_history(j))
            call local_state%pressure%set(pressure_history(j))

            call self%update_water_phases(material_id, local_state)

            call local_state%water_content%get(Qw)
            call local_state%ice_content%get(Qi)
            call local_state%vapor_content%get(Qv)

            call self%physics%calc_density_water(local_state, rho_w)
            call self%physics%calc_density_ice(local_state, rho_i)

            ! 有効密度（保存量）
            Uj = rho_w * Qw &
                 + rho_i * Qi &
                 + rho_w * Qv

            drho_dt = drho_dt + bdf_coeffs(j) * Uj
        end do

    end subroutine calc_effective_density_hydraulic

end submodule hydraulic_coefficients
