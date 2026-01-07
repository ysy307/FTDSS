submodule(main_thermal) thermal_coefficients
    implicit none
contains

    module pure elemental subroutine calc_density_water_thermal(self, state, rho_water)
        implicit none
        class(type_thermal), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: rho_water

        call self%physics%calc_density_water(state, rho_water)

    end subroutine calc_density_water_thermal

    module pure elemental subroutine calc_density_ice_thermal(self, state, rho_ice)
        implicit none
        class(type_thermal), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: rho_ice

        call self%physics%calc_density_ice(state, rho_ice)

    end subroutine calc_density_ice_thermal

    module pure elemental subroutine calc_density_vapor_saturation_thermal(self, state, rho_vapor_sat)
        implicit none
        class(type_thermal), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: rho_vapor_sat

        call self%physics%calc_density_vapor_saturation(state, rho_vapor_sat)

    end subroutine calc_density_vapor_saturation_thermal

    module pure elemental subroutine update_water_phases_thermal(self, material_id, state)
        implicit none
        class(type_thermal), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(inout) :: state

        call self%physics%update_water_phases(material_id, state)

    end subroutine update_water_phases_thermal

    module pure subroutine calc_inner_heat_capacity_thermal(self, material_id, state, bdf_coeffs, dU_dt)
        implicit none
        class(type_thermal), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        real(real64), intent(in) :: bdf_coeffs(:)
        real(real64), intent(inout) :: dU_dt

        type(type_state) :: local_state
        real(real64), allocatable :: temperature_history(:)
        real(real64), allocatable :: pressure_history(:)

        real(real64) :: porosity, Qw, Qi, Qv
        real(real64) :: rho_s, rho_w, rho_i
        real(real64) :: c_s, c_w, c_i, c_v
        real(real64) :: Lf, Lv
        real(real64) :: T, Uj
        integer :: j, n

        call state%temperature_history%get(temperature_history)
        call state%pressure_history%get(pressure_history)
        call state%porosity%get(porosity)

        n = size(bdf_coeffs)
        dU_dt = 0.0d0
        call local_state%copy(state)

        do j = 1, n
            call local_state%temperature%set(temperature_history(j))
            call local_state%pressure%set(pressure_history(j))
            if (temperature_history(j) < -40.0d0 .or. pressure_history(j) < 0.0d0) then
                error stop "Error: Sub-zero temperature or pressure in thermal heat capacity calculation."
            end if

            call self%update_water_phases(material_id, local_state)

            call local_state%temperature%get(T)
            call local_state%water_content%get(Qw)
            call local_state%ice_content%get(Qi)
            call local_state%vapor_content%get(Qv)

            call self%physics%get_density_solid(material_id, rho_s)
            call self%physics%calc_density_water(local_state, rho_w)
            call self%physics%calc_density_ice(local_state, rho_i)

            call self%physics%get_specific_heat_solid(material_id, c_s)
            call self%physics%calc_specific_heat_water(local_state, c_w)
            call self%physics%calc_specific_heat_ice(local_state, c_i)
            call self%physics%calc_specific_heat_vapor(local_state, c_v)

            call self%physics%calc_latent_heat_fusion(material_id, local_state, Lf)
            call self%physics%calc_latent_heat_vaporization(material_id, local_state, Lv)

            Uj = c_s * rho_s * (1.0d0 - porosity) * T &
                 + c_w * rho_w * Qw * T &
                 + c_i * rho_i * Qi * T &
                 + c_v * rho_w * Qv * T &
                 - rho_i * Lf * Qi &
                 + rho_w * Lv * Qv

            dU_dt = dU_dt + bdf_coeffs(j) * Uj
        end do

    end subroutine calc_inner_heat_capacity_thermal

end submodule thermal_coefficients
