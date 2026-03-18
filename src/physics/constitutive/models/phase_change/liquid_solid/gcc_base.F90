!> Implementation of Generalized Clausius-Clapeyron models.
!>
!> Algorithm overview:
!> - Evaluates the algebraic Generalized Clausius-Clapeyron formulas.
!> - Non-segregation utilizes: \( \psi = -L_f \rho_w \ln(T / T_{f0}) \)
!> - Segregation utilizes: \( \psi = \left(\frac{\rho_i}{\rho_w} - 1\right) P - L_f \rho_i \ln(T / T_{f0}) \)
submodule(models_phase_change_gcc) gcc_base
    implicit none

contains

    !> Initialize the polymorphic GCC holder.
    !> Allocates the specific GCC model based on the configuration ID.
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

    ! ==========================================================================
    ! Holder Wrapper Methods
    ! ==========================================================================

    !> Dispatch calculation of suction to the allocated model.
    !> Assigns zero if no model is allocated.
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

    !> Dispatch calculation of temperature derivative to the allocated model.
    !> Assigns zero if no model is allocated.
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

    !> Dispatch calculation of pressure derivative to the allocated model.
    !> Assigns zero if no model is allocated.
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

    !> Initialize the abstract GCC base component.
    !> Sets the material ID and establishes pointers to the IAPWS models.
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

    !> Calculate suction for the non-segregation model.
    !> Suction is calculated solely from the temperature depression below the freezing point.
    module subroutine calc_gcc_nonseg(self, state, suction)
        implicit none
        class(type_gcc_non_segregation), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: suction

        real(real64) :: temperature, temperature_K
        real(real64) :: rho_water
        real(real64) :: temperature_ratio

        call state%temperature%get(temperature)
        call self%shift_temperature_absolute(temperature, temperature_K)
        call self%calc_rho_water(state, rho_water)

        if (temperature <= Tf0) then
            temperature_ratio = max(temperature_K, 1.0d0) / Tf0_K
            suction = -lf * rho_water * log(temperature_ratio)
        else
            suction = 0.0d0
        end if
    end subroutine calc_gcc_nonseg

    !> Calculate temperature derivative of suction for the non-segregation model.
    !> Uses the exact derivative of the logarithmic function.
    module subroutine deriv_temp_gcc_nonseg(self, state, suction_derivative)
        implicit none
        class(type_gcc_non_segregation), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: suction_derivative

        real(real64) :: temperature, temperature_K
        real(real64) :: rho_water
        real(real64) :: temperature_safe

        call state%temperature%get(temperature)
        call self%shift_temperature_absolute(temperature, temperature_K)
        call self%calc_rho_water(state, rho_water)

        if (temperature <= Tf0) then
            temperature_safe = max(temperature_K, 1.0d0)
            suction_derivative = -lf * rho_water / temperature_safe
        else
            suction_derivative = 0.0d0
        end if
    end subroutine deriv_temp_gcc_nonseg

    !> Calculate pressure derivative of suction for the non-segregation model.
    !> Since suction is independent of macroscopic pressure, this always returns zero.
    module subroutine deriv_pres_gcc_nonseg(self, state, suction_derivative)
        implicit none
        class(type_gcc_non_segregation), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: suction_derivative

        suction_derivative = 0.0d0
    end subroutine deriv_pres_gcc_nonseg

    !> Calculate second derivative of suction w.r.t temperature for the non-segregation model.
    module subroutine deriv2_temp_gcc_nonseg(self, state, suction_derivative)
        implicit none
        class(type_gcc_non_segregation), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: suction_derivative

        real(real64) :: temperature, temperature_K
        real(real64) :: rho_water
        real(real64) :: temperature_safe

        call state%temperature%get(temperature)
        call self%shift_temperature_absolute(temperature, temperature_K)
        call self%calc_rho_water(state, rho_water)

        if (temperature <= Tf0) then
            temperature_safe = max(temperature_K, 1.0d0)
            suction_derivative = lf * rho_water / (temperature_safe * temperature_safe)
        else
            suction_derivative = 0.0d0
        end if
    end subroutine deriv2_temp_gcc_nonseg

    !> Calculate derivative of ice pressure w.r.t water pressure for the non-segregation model.
    !> Assumes equilibrium where \( P_i = P_w - \psi(T) \), hence the derivative is exactly 1.
    module subroutine deriv_pressure_ice_water_nonseg(self, state, deriv)
        implicit none
        class(type_gcc_non_segregation), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: deriv

        deriv = 1.0d0
    end subroutine deriv_pressure_ice_water_nonseg

    ! ==========================================================================
    ! Segregation Implementation
    ! ==========================================================================

    !> Calculate suction for the segregation model.
    !> Incorporates both temperature and macroscopic pressure dependencies.
    module subroutine calc_gcc_seg(self, state, suction)
        implicit none
        class(type_gcc_segregation), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: suction

        real(real64) :: temperature, temperature_K
        real(real64) :: pressure
        real(real64) :: rho_water, rho_ice
        real(real64) :: temperature_ratio

        call state%temperature%get(temperature)
        call state%pressure%get(pressure)
        call self%shift_temperature_absolute(temperature, temperature_K)
        call self%calc_rho_water(state, rho_water)
        call self%calc_rho_ice(state, rho_ice)

        if (temperature <= Tf0) then
            temperature_ratio = max(temperature_K, 1.0d0) / Tf0_K
            suction = (rho_ice / rho_water - 1.0d0) * pressure - lf * rho_ice * log(temperature_ratio)
        else
            suction = 0.0d0
        end if
    end subroutine calc_gcc_seg

    !> Calculate temperature derivative of suction for the segregation model.
    !> Accounts for the temperature-dependent term in the GCC equation.
    module subroutine deriv_temp_gcc_seg(self, state, suction_derivative)
        implicit none
        class(type_gcc_segregation), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: suction_derivative

        real(real64) :: temperature, temperature_K
        real(real64) :: rho_ice
        real(real64) :: temperature_safe

        call state%temperature%get(temperature)
        call self%shift_temperature_absolute(temperature, temperature_K)
        call self%calc_rho_ice(state, rho_ice)

        if (temperature <= Tf0) then
            temperature_safe = max(temperature_K, 1.0d0)
            suction_derivative = (-lf * rho_ice / temperature_safe)
        else
            suction_derivative = 0.0d0
        end if
    end subroutine deriv_temp_gcc_seg

    !> Calculate pressure derivative of suction for the segregation model.
    !> Evaluates the density ratio component \( (\rho_i / \rho_w - 1) \).
    module subroutine deriv_pres_gcc_seg(self, state, suction_derivative)
        implicit none
        class(type_gcc_segregation), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: suction_derivative

        real(real64) :: temperature
        real(real64) :: rho_water, rho_ice

        call state%temperature%get(temperature)

        if (temperature <= Tf0) then
            call self%calc_rho_water(state, rho_water)
            call self%calc_rho_ice(state, rho_ice)
            suction_derivative = (rho_ice / rho_water - 1.0d0)
        else
            suction_derivative = 0.0d0
        end if
    end subroutine deriv_pres_gcc_seg

    !> Calculate second derivative of suction w.r.t temperature for the segregation model.
    module subroutine deriv2_temp_gcc_seg(self, state, suction_derivative)
        implicit none
        class(type_gcc_segregation), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: suction_derivative

        real(real64) :: temperature, temperature_K
        real(real64) :: rho_ice
        real(real64) :: temperature_safe

        call state%temperature%get(temperature)
        call self%shift_temperature_absolute(temperature, temperature_K)
        call self%calc_rho_ice(state, rho_ice)

        if (temperature <= Tf0) then
            temperature_safe = max(temperature_K, 1.0d0)
            suction_derivative = (lf * rho_ice / (temperature_safe * temperature_safe))
        else
            suction_derivative = 0.0d0
        end if
    end subroutine deriv2_temp_gcc_seg

    !> Calculate derivative of ice pressure w.r.t water pressure for the segregation model.
    !> Assumes equilibrium state where the derivative depends on the density ratio.
    module subroutine deriv_pressure_ice_water_seg(self, state, deriv)
        implicit none
        class(type_gcc_segregation), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: deriv

        real(real64) :: temperature
        real(real64) :: rho_water, rho_ice

        call state%temperature%get(temperature)

        if (temperature <= Tf0) then
            call self%calc_rho_water(state, rho_water)
            call self%calc_rho_ice(state, rho_ice)
            deriv = rho_ice / rho_water
        else
            deriv = 0.0d0
        end if
    end subroutine deriv_pressure_ice_water_seg

end submodule gcc_base
