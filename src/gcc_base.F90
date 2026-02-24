!>
!> @brief Implementation of GCC models.
!>
!> This submodule implements the methods for calculating suction and its derivatives
!> for both non-segregation and segregation GCC models.
!> Calculations are performed in Pascal [Pa].
!>
submodule(physics_models_phase_change_liquid_solid_gcc) gcc_base
    implicit none

contains

    !>
    !> @brief Initialize the GCC holder object.
    !>
    module subroutine initialize_holder_gccs(self, material_id, config, water, ice)
        implicit none
        class(holder_gccs), intent(inout) :: self
        integer(int32), intent(in) :: material_id
        class(type_config_gcc), intent(in) :: config
        type(type_iapws97), target, intent(in) :: water
        type(type_iapws06), target, intent(in) :: ice

        select case (config%gcc_model%ID)
        case (GCC_TYPES%NON_SEGREGATION%ID)
            allocate (type_gcc_non_segregation :: self%p)
        case (GCC_TYPES%SEGREGATION%ID)
            allocate (type_gcc_segregation :: self%p)
        end select

        if (allocated(self%p)) then
            call self%p%initialize(material_id, water, ice)
        end if
    end subroutine initialize_holder_gccs

    ! --------------------------------------------------------------------------
    ! Holder Wrapper Methods
    ! --------------------------------------------------------------------------

    module subroutine calc_holder(self, state, suction)
        implicit none
        class(holder_gccs), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: suction
        if (allocated(self%p)) then
            call self%p%calc(state, suction)
        else
            suction = 0.0d0
        end if
    end subroutine calc_holder

    module subroutine deriv_temperature_holder(self, state, deriv)
        implicit none
        class(holder_gccs), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: deriv
        if (allocated(self%p)) then
            call self%p%deriv_temperature(state, deriv)
        else
            deriv = 0.0d0
        end if
    end subroutine deriv_temperature_holder

    module subroutine deriv_pressure_holder(self, state, deriv)
        implicit none
        class(holder_gccs), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: deriv
        if (allocated(self%p)) then
            call self%p%deriv_pressure(state, deriv)
        else
            deriv = 0.0d0
        end if
    end subroutine deriv_pressure_holder

    !>
    !> @brief Initialize the abstract GCC base.
    !>
    module subroutine initialize_abst_gcc(self, material_id, water, ice)
        implicit none
        class(abst_gcc), intent(inout) :: self
        integer(int32), intent(in) :: material_id
        type(type_iapws97), target, intent(in) :: water
        type(type_iapws06), target, intent(in) :: ice

        self%material_id = material_id
        self%water => water
        self%ice => ice

        self%initialized = .true.
    end subroutine initialize_abst_gcc

    ! ==========================================================================
    ! Non-Segregation Implementation
    ! ==========================================================================

    module subroutine calc_gcc_nonseg(self, state, suction)
        implicit none
        class(type_gcc_non_segregation), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: suction

        real(real64) :: temperature, temperature_K
        real(real64) :: rho_water

        call state%temperature%get(temperature)
        call self%shift_temperature_absolute(temperature, temperature_K)
        call self%calc_rho_water(state, rho_water)

        if (temperature <= Tf0) then
            suction = -lf * rho_water * log(temperature_K / Tf0_K)
        else
            suction = 0.0d0
        end if
    end subroutine calc_gcc_nonseg

    module subroutine deriv_temp_gcc_nonseg(self, state, suction_derivative)
        implicit none
        class(type_gcc_non_segregation), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: suction_derivative

        real(real64) :: temperature, temperature_K
        real(real64) :: rho_water

        call state%temperature%get(temperature)
        call self%shift_temperature_absolute(temperature, temperature_K)
        call self%calc_rho_water(state, rho_water)

        if (temperature <= Tf0) then
            suction_derivative = -lf * rho_water / temperature_K
        else
            suction_derivative = 0.0d0
        end if
    end subroutine deriv_temp_gcc_nonseg

    module subroutine deriv_pres_gcc_nonseg(self, state, suction_derivative)
        implicit none
        class(type_gcc_non_segregation), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: suction_derivative
        ! Non-segregation model: d(Suction)/dP = 0
        suction_derivative = 0.0d0
    end subroutine deriv_pres_gcc_nonseg

    module subroutine deriv2_temp_gcc_nonseg(self, state, suction_derivative)
        implicit none
        class(type_gcc_non_segregation), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: suction_derivative

        real(real64) :: temperature, temperature_K
        real(real64) :: rho_water

        call state%temperature%get(temperature)
        call self%shift_temperature_absolute(temperature, temperature_K)
        call self%calc_rho_water(state, rho_water)

        if (temperature <= Tf0) then
            suction_derivative = lf * rho_water / (temperature_K * temperature_K)
        else
            suction_derivative = 0.0d0
        end if
    end subroutine deriv2_temp_gcc_nonseg

    module subroutine deriv_pressure_ice_water_nonseg(self, state, deriv)
        implicit none
        class(type_gcc_non_segregation), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: deriv

        ! Non-segregation model: dP_ice/dP_w = 0
        deriv = 0.0d0
    end subroutine deriv_pressure_ice_water_nonseg

    ! ==========================================================================
    ! Segregation Implementation
    ! ==========================================================================

    module subroutine calc_gcc_seg(self, state, suction)
        implicit none
        class(type_gcc_segregation), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: suction

        real(real64) :: temperature, temperature_K
        real(real64) :: pressure
        real(real64) :: rho_water, rho_ice

        call state%temperature%get(temperature)
        call state%pressure%get(pressure)
        call self%shift_temperature_absolute(temperature, temperature_K)
        call self%calc_rho_water(state, rho_water)
        call self%calc_rho_ice(state, rho_ice)

        if (temperature <= Tf0) then
            suction = (rho_ice / rho_water - 1.0d0) * pressure - lf * rho_ice * log(temperature_K / Tf0_K)
        else
            suction = 0.0d0
        end if
    end subroutine calc_gcc_seg

    module subroutine deriv_temp_gcc_seg(self, state, suction_derivative)
        implicit none
        class(type_gcc_segregation), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: suction_derivative

        real(real64) :: temperature, temperature_K
        real(real64) :: rho_ice

        call state%temperature%get(temperature)
        call self%shift_temperature_absolute(temperature, temperature_K)
        call self%calc_rho_ice(state, rho_ice)

        if (temperature <= Tf0) then
            suction_derivative = (-lf * rho_ice / temperature_K)
        else
            suction_derivative = 0.0d0
        end if
    end subroutine deriv_temp_gcc_seg

    module subroutine deriv_pres_gcc_seg(self, state, suction_derivative)
        implicit none
        class(type_gcc_segregation), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: suction_derivative

        real(real64) :: temperature
        real(real64) :: rho_water, rho_ice

        call state%temperature%get(temperature)

        ! 凍結状態でのみ圧力微分が値を持ちます（非凍結時はサクション0固定のため微分0）
        if (temperature <= Tf0) then
            call self%calc_rho_water(state, rho_water)
            call self%calc_rho_ice(state, rho_ice)
            ! d(Suction)/dP = (rho_ice / rho_water - 1.0)
            suction_derivative = (rho_ice / rho_water - 1.0d0)
        else
            suction_derivative = 0.0d0
        end if
    end subroutine deriv_pres_gcc_seg

    module subroutine deriv2_temp_gcc_seg(self, state, suction_derivative)
        implicit none
        class(type_gcc_segregation), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: suction_derivative

        real(real64) :: temperature, temperature_K
        real(real64) :: rho_ice

        call state%temperature%get(temperature)
        call self%shift_temperature_absolute(temperature, temperature_K)
        call self%calc_rho_ice(state, rho_ice)

        if (temperature <= Tf0) then
            suction_derivative = (lf * rho_ice / (temperature_K * temperature_K))
        else
            suction_derivative = 0.0d0
        end if
    end subroutine deriv2_temp_gcc_seg

    module subroutine deriv_pressure_ice_water_seg(self, state, deriv)
        implicit none
        class(type_gcc_segregation), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: deriv

        real(real64) :: temperature
        real(real64) :: rho_water, rho_ice

        call state%temperature%get(temperature)

        ! 凍結状態でのみ値を持ちます（非凍結時はサクション0固定のため微分0）
        if (temperature <= Tf0) then
            call self%calc_rho_water(state, rho_water)
            call self%calc_rho_ice(state, rho_ice)
            ! dP_ice/dP_w = rho_ice / rho_water
            deriv = rho_ice / rho_water
        else
            deriv = 0.0d0
        end if
    end subroutine deriv_pressure_ice_water_seg

end submodule gcc_base
