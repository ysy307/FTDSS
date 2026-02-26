submodule(materials_thermal_conductivity) thermal_conductivity_multiphase
    implicit none
contains

    module subroutine calc_thc_gp_1phase(self, state, lambda)
        implicit none
        class(type_thc_1phase), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: lambda

        lambda = self%material1

    end subroutine calc_thc_gp_1phase

    module subroutine calc_thc_dispersivity_gp_1phase(self, state, lambda)
        implicit none
        class(type_thc_1phase), intent(in) :: self
        type(type_state), intent(in) :: state
        type(type_thc_dispersivity), intent(inout) :: lambda

        real(real64) :: lambda_0
        type(type_coordinate_dp), pointer :: water_flux

        call state%get(water_flux=water_flux)

        call self%calc(state, lambda_0)
        call self%calc_lambda_dispersivity(lambda_0, 0.0d0, water_flux%x, water_flux%y, water_flux%z, lambda)

    end subroutine calc_thc_dispersivity_gp_1phase

    module subroutine calc_thc_gp_2phase(self, state, lambda)
        implicit none
        class(type_thc_2phase), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: lambda

        real(real64) :: phi1, phi2

        call self%get_phi(state, phi1, phi2)

        call calc_thc_2(self%material1, phi1, self%material2, phi2, lambda)

    end subroutine calc_thc_gp_2phase

    module subroutine calc_thc_dispersivity_gp_2phase(self, state, lambda)
        implicit none
        class(type_thc_2phase), intent(in) :: self
        type(type_state), intent(in) :: state
        type(type_thc_dispersivity), intent(inout) :: lambda

        real(real64) :: lambda_0
        type(type_coordinate_dp), pointer :: water_flux

        real(real64) :: htc_water, rho_water, cp_water

        call state%get(water_flux=water_flux)

        call self%calc_water_density(state, rho_water)
        call self%calc_water_cp(state, cp_water)
        htc_water = rho_water * cp_water

        call self%calc(state, lambda_0)
        call self%calc_lambda_dispersivity(lambda_0, htc_water, water_flux%x, water_flux%y, water_flux%z, lambda)

    end subroutine calc_thc_dispersivity_gp_2phase

    module subroutine calc_thc_gp_3phase(self, state, lambda)
        implicit none
        class(type_thc_3phase), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: lambda

        real(real64) :: phi1, phi2, phi3

        call self%get_phi(state, phi1, phi2, phi3)

        call calc_thc_3(self%material1, phi1, self%material2, phi2, self%material3, phi3, lambda)

    end subroutine calc_thc_gp_3phase

    module subroutine calc_thc_dispersivity_gp_3phase(self, state, lambda)
        implicit none
        class(type_thc_3phase), intent(in) :: self
        type(type_state), intent(in) :: state
        type(type_thc_dispersivity), intent(inout) :: lambda

        real(real64) :: lambda_0
        type(type_coordinate_dp), pointer :: water_flux

        real(real64) :: qx, qy, qz
        real(real64) :: htc_water, rho_water, cp_water

        call self%calc_water_density(state, rho_water)
        call self%calc_water_cp(state, cp_water)
        htc_water = rho_water * cp_water

        call state%get(water_flux=water_flux)
        if (associated(water_flux)) then
            qx = water_flux%x
            qy = water_flux%y
            qz = water_flux%z
        else
            qx = 0.0d0
            qy = 0.0d0
            qz = 0.0d0
        end if

        call self%calc(state, lambda_0)
        call self%calc_lambda_dispersivity(lambda_0, htc_water, qx, qy, qz, lambda)

    end subroutine calc_thc_dispersivity_gp_3phase

    module subroutine calc_thc_gp_4phase(self, state, lambda)
        implicit none
        class(type_thc_4phase), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: lambda

        real(real64) :: phi1, phi2, phi3, phi4

        call self%get_phi(state, phi1, phi2, phi3, phi4)

        if (phi4 > 0.0d0) then
            call calc_thc_4_vadoze(self%params(1), self%params(2), self%params(3), self%params(4), &
                                   self%params(5), self%params(6), phi2, phi3, phi4, lambda)
        else
            call calc_thc_4(self%material1, phi1, self%material2, phi2, self%material3, phi3, self%material4, phi4, lambda)
        end if

    end subroutine calc_thc_gp_4phase

    module subroutine calc_thc_dispersivity_gp_4phase(self, state, lambda)
        implicit none
        class(type_thc_4phase), intent(in) :: self
        type(type_state), intent(in) :: state
        type(type_thc_dispersivity), intent(inout) :: lambda

        real(real64) :: lambda_0
        type(type_coordinate_dp), pointer :: water_flux

        real(real64) :: qx, qy, qz
        real(real64) :: htc_water, rho_water, cp_water

        call self%calc_water_density(state, rho_water)
        call self%calc_water_cp(state, cp_water)
        htc_water = rho_water * cp_water
        call state%get(water_flux=water_flux)
        if (associated(water_flux)) then
            qx = water_flux%x
            qy = water_flux%y
            qz = water_flux%z
        else
            qx = 0.0d0
            qy = 0.0d0
            qz = 0.0d0
        end if

        call self%calc(state, lambda_0)

        call self%calc_lambda_dispersivity(lambda_0, htc_water, qx, qy, qz, lambda)

    end subroutine calc_thc_dispersivity_gp_4phase

end submodule thermal_conductivity_multiphase
