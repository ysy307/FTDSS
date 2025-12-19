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
    module subroutine initialize_holder_gccs(self, material_id, gcc_id, water, ice)
        implicit none
        !> The holder object
        class(holder_gccs), intent(inout) :: self
        !> Material identifier
        integer(int32), intent(in) :: material_id
        !> GCC model identifier (Non-segregation or Segregation)
        integer(int32), intent(in) :: gcc_id
        !> Water property object
        type(type_iapws97), target, intent(in) :: water
        !> Ice property object
        type(type_iapws06), target, intent(in) :: ice

        select case (gcc_id)
        case (GCC_NON_SEGREGATION)
            allocate (type_gcc_non_segregation :: self%p)
        case (GCC_SEGREGATION)
            allocate (type_gcc_segregation :: self%p)
        end select

        if (allocated(self%p)) then
            call self%p%initialize(material_id, water, ice)
        end if
    end subroutine initialize_holder_gccs

    !>
    !> @brief Initialize the abstract GCC base.
    !>
    module subroutine initialize_abst_gcc(self, material_id, water, ice)
        implicit none
        !> Abstract GCC object
        class(abst_gcc), intent(inout) :: self
        !> Material identifier
        integer(int32), intent(in) :: material_id
        !> Water property object
        type(type_iapws97), target, intent(in) :: water
        !> Ice property object
        type(type_iapws06), target, intent(in) :: ice

        self%material_id = material_id
        self%water => water
        self%ice => ice
    end subroutine initialize_abst_gcc

    module pure elemental subroutine shift_temperature_absolute_abst_gcc(self, temperature_degree, temperature_K)
        implicit none
        !> GCC object
        class(abst_gcc), intent(in) :: self
        !> Temperature in degree Celsius
        real(real64), intent(in) :: temperature_degree
        !> Temperature in Kelvin
        real(real64), intent(inout) :: temperature_K

        temperature_K = temperature_degree + T_to_K
    end subroutine shift_temperature_absolute_abst_gcc

    module pure elemental subroutine shift_pressure_absolute_abst_gcc(self, pressure_gauge, pressure_absolute)
        implicit none
        !> GCC object
        class(abst_gcc), intent(in) :: self
        !> Gauge pressure [Pa]
        real(real64), intent(in) :: pressure_gauge
        !> Absolute pressure [Pa]
        real(real64), intent(inout) :: pressure_absolute

        if (pressure_gauge < 0.0d0) then
            pressure_absolute = P_atm
        else
            pressure_absolute = P_atm + pressure_gauge
        end if
    end subroutine shift_pressure_absolute_abst_gcc

    !>
    !> @brief Calculate suction for GCC without segregation [Pa].
    !>
    !> Uses the Clapeyron equation tailored for non-segregated ice-water systems.
    !> Suction is 0 when temperature is above freezing point.
    !>
    module pure elemental subroutine calc_gcc_nonseg(self, state, suction)
        implicit none
        !> GCC object
        class(type_gcc_non_segregation), intent(in) :: self
        !> Thermodynamic state
        type(type_state), intent(in) :: state
        !> Calculated suction [Pa]
        real(real64), intent(inout) :: suction

        real(real64) :: temperature_K, pressure_absolute
        real(real64) :: rho_water

        call self%shift_temperature_absolute(state%temperature, temperature_K)
        call self%shift_pressure_absolute(state%pressure, pressure_absolute)
        call self%water%calc_rho(temperature_K, pressure_absolute, rho_water)

        if (state%temperature <= Tf0) then
            ! Apply generalized Clausius-Clapeyron equation for non-segregation
            ! Result in Pa (J/m^3)
            suction = -lf * rho_water * log(temperature_K / Tf0_K)
        else
            suction = 0.0d0
        end if
    end subroutine calc_gcc_nonseg

    !>
    !> @brief Calculate first derivative of suction with respect to temperature [Pa/K].
    !>
    module pure elemental subroutine deriv_gcc_nonseg(self, state, suction_derivative)
        implicit none
        !> GCC object
        class(type_gcc_non_segregation), intent(in) :: self
        !> Thermodynamic state
        type(type_state), intent(in) :: state
        !> Derivative of suction w.r.t temperature [Pa/K]
        real(real64), intent(inout) :: suction_derivative

        real(real64) :: temperature_K, pressure_absolute
        real(real64) :: rho_water

        call self%shift_temperature_absolute(state%temperature, temperature_K)
        call self%shift_pressure_absolute(state%pressure, pressure_absolute)
        call self%water%calc_rho(temperature_K, pressure_absolute, rho_water)

        if (state%temperature <= Tf0) then
            ! Derivative of suction w.r.t temperature [Pa/K]
            suction_derivative = -lf * rho_water / temperature_K
        else
            suction_derivative = 0.0d0
        end if
    end subroutine deriv_gcc_nonseg

    !>
    !> @brief Calculate second derivative of suction with respect to temperature [Pa/K^2].
    !>
    module pure elemental subroutine deriv_2nd_gcc_nonseg(self, state, suction_derivative)
        implicit none
        !> GCC object
        class(type_gcc_non_segregation), intent(in) :: self
        !> Thermodynamic state
        type(type_state), intent(in) :: state
        !> Second derivative of suction w.r.t temperature [Pa/K^2]
        real(real64), intent(inout) :: suction_derivative

        real(real64) :: temperature_K, pressure_absolute
        real(real64) :: rho_water

        call self%shift_temperature_absolute(state%temperature, temperature_K)
        call self%shift_pressure_absolute(state%pressure, pressure_absolute)
        call self%water%calc_rho(temperature_K, pressure_absolute, rho_water)

        if (state%temperature <= Tf0) then
            ! Second derivative of suction w.r.t temperature [Pa/K^2]
            suction_derivative = lf * rho_water / (temperature_K * temperature_K)
        else
            suction_derivative = 0.0d0
        end if
    end subroutine deriv_2nd_gcc_nonseg

    !>
    !> @brief Calculate suction for GCC with segregation [Pa].
    !>
    !> Considers the density difference between ice and water, and the pressure effect.
    !>
    module pure elemental subroutine calc_gcc_seg(self, state, suction)
        implicit none
        !> GCC object
        class(type_gcc_segregation), intent(in) :: self
        !> Thermodynamic state
        type(type_state), intent(in) :: state
        !> Calculated suction [Pa]
        real(real64), intent(inout) :: suction

        real(real64) :: temperature_K, pressure_absolute
        real(real64) :: rho_water, rho_ice

        call self%shift_temperature_absolute(state%temperature, temperature_K)
        call self%shift_pressure_absolute(state%pressure, pressure_absolute)
        call self%water%calc_rho(temperature_K, pressure_absolute, rho_water)
        call self%ice%calc_rho(temperature_K, pressure_absolute, rho_ice)

        if (state%temperature <= Tf0) then
            ! Generalized Clausius-Clapeyron equation for segregation
            ! Result in Pa
            suction = (rho_ice / rho_water - 1.0d0) * state%pressure - lf * rho_ice * log(temperature_K / Tf0_K)
        else
            suction = 0.0d0
        end if
    end subroutine calc_gcc_seg

    !>
    !> @brief Calculate first derivative of suction for GCC with segregation [Pa/K].
    !>
    module pure elemental subroutine deriv_gcc_seg(self, state, suction_derivative)
        implicit none
        !> GCC object
        class(type_gcc_segregation), intent(in) :: self
        !> Thermodynamic state
        type(type_state), intent(in) :: state
        !> Derivative of suction w.r.t temperature [Pa/K]
        real(real64), intent(inout) :: suction_derivative

        real(real64) :: temperature_K, pressure_absolute
        real(real64) :: rho_ice

        call self%shift_temperature_absolute(state%temperature, temperature_K)
        call self%shift_pressure_absolute(state%pressure, pressure_absolute)
        call self%ice%calc_rho(temperature_K, pressure_absolute, rho_ice)

        if (state%temperature <= Tf0) then
            suction_derivative = (-lf * rho_ice / temperature_K)
        else
            suction_derivative = 0.0d0
        end if
    end subroutine deriv_gcc_seg

    !>
    !> @brief Calculate second derivative of suction for GCC with segregation [Pa/K^2].
    !>
    module pure elemental subroutine deriv_2nd_gcc_seg(self, state, suction_derivative)
        implicit none
        !> GCC object
        class(type_gcc_segregation), intent(in) :: self
        !> Thermodynamic state
        type(type_state), intent(in) :: state
        !> Second derivative of suction w.r.t temperature [Pa/K^2]
        real(real64), intent(inout) :: suction_derivative

        real(real64) :: temperature_K, pressure_absolute
        real(real64) :: rho_ice

        call self%shift_temperature_absolute(state%temperature, temperature_K)
        call self%shift_pressure_absolute(state%pressure, pressure_absolute)
        call self%ice%calc_rho(temperature_K, pressure_absolute, rho_ice)

        if (state%temperature <= Tf0) then
            suction_derivative = (lf * rho_ice / (temperature_K * temperature_K))
        else
            suction_derivative = 0.0d0
        end if
    end subroutine deriv_2nd_gcc_seg

end submodule gcc_base
