submodule(physics_material_thermal_conductivity) thermal_conductivity_4phase
    implicit none
contains
    module subroutine initialize_type_thc_4phase(self, material_id, physics_info, water, ice)
        implicit none
        class(type_thc_4phase), intent(inout) :: self
        integer(int32), intent(in) :: material_id
        type(type_physics_info), intent(in) :: physics_info
        type(type_iapws97), intent(in), target :: water
        type(type_iapws06), intent(in), target :: ice

        self%material_id = material_id

        self%material1 = physics_info%solid
        self%material2 = physics_info%water
        self%material3 = physics_info%ice
        self%material4 = physics_info%vapor

        if (allocated(physics_info%dispersity)) then
            call allocate_array(self%dispersity, source=physics_info%dispersity)
            self%use_dispersity = .true.
        end if

        if (allocated(physics_info%params)) then
            call allocate_array(self%params, source=physics_info%params)
        end if

        self%water => water
        self%ice => ice

    end subroutine initialize_type_thc_4phase

    module pure elemental subroutine calc_thc_gp_4phase(self, state, lambda)
        implicit none
        class(type_thc_4phase), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: lambda

        real(real64) :: phi1, phi2, phi3, phi4

        phi1 = 1.0d0 - state%porosity
        phi2 = state%water_content
        phi3 = state%ice_content
        phi4 = 1.0d0 - phi1 - phi2 - phi3

        if (phi4 > 0.0d0) then
            call calc_thc_4_vadoze(self%params(1), self%params(2), self%params(3), self%params(4), &
                                   self%params(5), self%params(6), phi2, phi3, phi4, lambda)
        else
            call calc_thc_4(self%material1, phi1, self%material2, phi2, self%material3, phi3, self%material4, phi4, lambda)
        end if

    end subroutine calc_thc_gp_4phase

    module pure elemental subroutine calc_thc_dispersity_gp_4phase(self, state, lambda)
        implicit none
        class(type_thc_4phase), intent(in) :: self
        type(type_state), intent(in) :: state
        type(type_thc_dispersity), intent(inout) :: lambda

        real(real64) :: lambda_0

        real(real64) :: phi1, phi2, phi3, phi4
        real(real64) :: htc_water, rho_water, cp_water
        real(real64) :: temp_K

        phi1 = 1.0d0 - state%porosity
        phi2 = state%water_content
        phi3 = state%ice_content
        phi4 = 1.0d0 - phi1 - phi2 - phi3

        temp_K = state%temperature + TtoK

        if (associated(self%water)) then
            call self%water%calc_rho(temp_K, state%pressure, rho_water)
            call self%water%calc_cp(temp_K, state%pressure, cp_water)
        else
            rho_water = 1000.0d0
            cp_water = 4181.3d0
        end if
        htc_water = rho_water * cp_water

        call self%calc(state, lambda_0)

        if (self%use_dispersity) then
            call calc_thc_dispersity(lambda_0=lambda_0, lambda_T=self%dispersity(1), lambda_L=self%dispersity(2), &
                                     htc_water=htc_water, q_x=state%water_flux%x, q_y=state%water_flux%y, q_z=state%water_flux%z, &
                                     lambda=lambda)
        else
            call lambda%reset()
            lambda%lambda_xx = lambda_0
            lambda%lambda_yy = lambda_0
            lambda%lambda_zz = lambda_0
        end if

    end subroutine calc_thc_dispersity_gp_4phase

end submodule thermal_conductivity_4phase
