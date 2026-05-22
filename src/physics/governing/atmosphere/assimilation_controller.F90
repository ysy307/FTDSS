!> Assimilation controller: orchestrates ETKF cycle and updates surface boundary conditions.
!>
!> Surface energy balance linearized around Ts_ast:
!> \[ -\lambda \partial_z T \big|_s = G^* - h_\text{eff}(T_s - T_s^*) \]
!> \[ h_\text{eff} = h_R + h_H + h_E, \quad T_\text{ref} = T_s^* + G^*/h_\text{eff} \]
module physics_governing_atmosphere_controller
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: physics_governing_atmosphere_ensemble, only: type_ensemble_manager, type_atmos_state
    use :: physics_governing_atmosphere_observation, only: type_observation_manager
    use :: physics_governing_atmosphere_etkf, only: type_etkf_solver
    use :: physics_governing_boundary_manager, only: type_bc_manager
    implicit none
    private

    public :: type_da_config
    public :: type_assimilation_controller

    real(real64), parameter :: sigma_sb_atm = 5.670374419d-8  ! Stefan-Boltzmann [W/m2/K4]
    real(real64), parameter :: rho_air_atm  = 1.2d0           ! air density [kg/m3]
    real(real64), parameter :: c_air_atm    = 1005.0d0        ! specific heat air [J/kg/K]
    real(real64), parameter :: rho_w_atm    = 1000.0d0        ! water density [kg/m3]
    real(real64), parameter :: Rv_atm       = 461.526d0       ! specific gas const water [J/kg/K]
    real(real64), parameter :: T0_K_atm     = 273.15d0

    !> All parameters loaded from DataAssimilation.json.
    type :: type_da_config
        integer(int32)     :: ensemble_size       = 30
        real(real64)       :: max_height          = 200.0d0
        integer(int32)     :: num_nodes           = 50
        character(len=256) :: csv_file            = 'obs_data.csv'
        real(real64)       :: interval_seconds    = 3600.0d0
        real(real64)       :: sigma_T             = 0.5d0
        real(real64)       :: sigma_q             = 1.0d-4
        real(real64)       :: sigma_U             = 0.5d0
        real(real64)       :: z0                  = 0.01d0
        real(real64)       :: albedo              = 0.2d0
        real(real64)       :: emissivity          = 0.95d0
        real(real64)       :: Pmin                = -1.0d5
        real(real64)       :: Pmax                = 0.0d0
        real(real64)       :: lambda_soil         = 1.5d0
        real(real64)       :: stomatal_resistance = 50.0d0
        character(len=32)  :: reference_datetime  = '2000-01-01T00:00:00'
        real(real64)       :: latitude            = 35.67d0  ! deg N (Fuchu default)
        real(real64)       :: longitude           = 139.48d0 ! deg E (Fuchu default)
        real(real64)       :: tau_atm             = 0.6d0    ! clear-sky transmittance
    end type type_da_config

    !> Orchestrates ETKF cycles and surface BC parameter updates.
    type :: type_assimilation_controller
        type(type_ensemble_manager)   :: ensemble
        type(type_observation_manager):: obs_manager
        type(type_etkf_solver)        :: etkf_solver
        ! Surface state updated externally before each cycle
        real(real64) :: Ts_ast       = 0.0d0   ! surface temperature [C]
        real(real64) :: Pwater_curr  = 0.0d0   ! surface water pressure [m]
        ! Surface parameters
        real(real64) :: z0           = 0.01d0
        real(real64) :: z_ref_U      = 9.2d0
        real(real64) :: eps_s        = 0.95d0
        real(real64) :: albedo       = 0.2d0
        real(real64) :: Pmin         = -1.0d5
        real(real64) :: Pmax         = 0.0d0
        real(real64) :: r_s          = 50.0d0  ! stomatal resistance [s/m]
        ! Radiation/precipitation state
        real(real64) :: Rs           = 0.0d0   ! shortwave radiation [W/m2]
        real(real64) :: Rl_down      = 300.0d0 ! downward longwave radiation [W/m2]
        real(real64) :: precip       = 0.0d0   ! precipitation rate [m/s]
        ! Solar geometry parameters
        real(real64) :: latitude     = 35.67d0  ! deg N
        real(real64) :: longitude    = 139.48d0 ! deg E
        real(real64) :: tau_atm      = 0.6d0    ! clear-sky transmittance
        ! BC entity IDs
        integer(int32) :: bc_entity_thermal   = 3
        integer(int32) :: bc_entity_hydraulic = 3
        logical :: is_initialized = .false.
    contains
        procedure, public, pass(self) :: initialize         => initialize_da_controller
        procedure, public, pass(self) :: destroy            => destroy_da_controller
        procedure, public, pass(self) :: set_surface_state  => set_surface_state_controller
        procedure, public, pass(self) :: execute_assimilation_cycle
        procedure, private, pass(self) :: compute_friction_velocity
        procedure, private, pass(self) :: compute_surface_energy_balance
        procedure, private, pass(self) :: compute_moisture_bc
        procedure, private, pass(self) :: update_solar_radiation
    end type type_assimilation_controller

contains

    subroutine initialize_da_controller(self, config, bc_entity_thermal, bc_entity_hydraulic)
        implicit none
        class(type_assimilation_controller), intent(inout) :: self
        type(type_da_config), intent(in) :: config
        integer(int32), intent(in) :: bc_entity_thermal
        integer(int32), intent(in) :: bc_entity_hydraulic

        self%z0                  = config%z0
        self%eps_s               = config%emissivity
        self%albedo              = config%albedo
        self%Pmin                = config%Pmin
        self%Pmax                = config%Pmax
        self%r_s                 = config%stomatal_resistance
        self%latitude            = config%latitude
        self%longitude           = config%longitude
        self%tau_atm             = config%tau_atm
        self%bc_entity_thermal   = bc_entity_thermal
        self%bc_entity_hydraulic = bc_entity_hydraulic

        call self%ensemble%initialize(config%ensemble_size, config%max_height, config%num_nodes)
        call self%ensemble%add_perturbation(config%sigma_T, config%sigma_q, config%sigma_U)

        call self%obs_manager%initialize( &
            trim(config%csv_file), &
            trim(config%reference_datetime), &
            config%interval_seconds, &
            config%sigma_T, config%sigma_q, config%sigma_U)

        call self%etkf_solver%initialize(config%ensemble_size)

        self%is_initialized = .true.
        write (*, '(A,I0,A,F7.1,A,I0,A)') '[DA] Controller initialized: ', &
            config%ensemble_size, ' members, max_height=', config%max_height, &
            ' m, num_nodes=', config%num_nodes, '.'
    end subroutine initialize_da_controller

    subroutine destroy_da_controller(self)
        implicit none
        class(type_assimilation_controller), intent(inout) :: self

        call self%ensemble%destroy()
        call self%obs_manager%destroy()
        self%is_initialized = .false.
    end subroutine destroy_da_controller

    !> Updates surface temperature and water pressure from current FEM solution.
    subroutine set_surface_state_controller(self, Ts, Pwater)
        implicit none
        class(type_assimilation_controller), intent(inout) :: self
        real(real64), intent(in) :: Ts
        real(real64), intent(in) :: Pwater

        self%Ts_ast      = Ts
        self%Pwater_curr = Pwater
    end subroutine set_surface_state_controller

    !> Main cycle: ETKF analysis -> surface energy balance -> BC update.
    !> @param[in] current_time  elapsed simulation time [s]
    !> @param[in] current_doy   day of year (used for solar geometry)
    !> @param[inout] bc_thermal    bc_manager for thermal physics (ID=3 Robin)
    !> @param[inout] bc_hydraulic  bc_manager for hydraulic physics (ID=3 Seepage)
    subroutine execute_assimilation_cycle(self, current_time, current_doy, &
                                          bc_thermal, bc_hydraulic)
        implicit none
        class(type_assimilation_controller), intent(inout) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(in) :: current_doy
        type(type_bc_manager), intent(inout) :: bc_thermal
        type(type_bc_manager), intent(inout) :: bc_hydraulic

        type(type_atmos_state) :: mean_state
        real(real64) :: y(3), R(3, 3), R_diag(3)
        logical :: is_available
        integer(int32) :: ii
        real(real64) :: Ta, qa, Ua
        real(real64) :: u_ast, r_H, r_v
        real(real64) :: h_eff, G_ast, T_ref
        real(real64) :: q_potential
        real(real64) :: bc_vals_th(3), bc_vals_hyd(3)

        if (.not. self%is_initialized) return

        ! Update solar shortwave from orbital geometry
        call self%update_solar_radiation(current_doy)

        call self%obs_manager%get_observation(current_time, y, R, is_available)
        if (.not. is_available) return

        do ii = 1, 3
            R_diag(ii) = R(ii, ii)
        end do

        ! ETKF analysis: pass y and R_diag directly to avoid double get_observation call
        call self%etkf_solver%calculate_analysis(self%ensemble, self%obs_manager, y, R_diag)

        ! Re-inject perturbations to prevent ensemble collapse between obs epochs.
        ! Magnitude: half the observation sigma (maintains spread ~ obs uncertainty).
        call self%ensemble%add_perturbation( &
            0.5d0 * self%obs_manager%sigma_T, &
            0.5d0 * self%obs_manager%sigma_q, &
            0.5d0 * self%obs_manager%sigma_U)

        ! Analysis mean
        call self%ensemble%compute_mean(mean_state)
        Ta = mean_state%T(1)
        qa = mean_state%q(1)
        Ua = mean_state%U(1)

        ! Aerodynamic resistances
        call self%compute_friction_velocity(Ua, u_ast, r_H, r_v)

        ! Surface energy balance -> Robin BC parameters
        call self%compute_surface_energy_balance(Ta, qa, r_H, r_v, h_eff, G_ast, T_ref)

        ! Thermal: values(1)=h_eff, values(2)=T_ref for Robin BC: flux=-h_eff*(T_s-T_ref)
        bc_vals_th(1) = h_eff
        bc_vals_th(2) = T_ref
        bc_vals_th(3) = 0.0d0
        call bc_thermal%update_bc_data(self%bc_entity_thermal, bc_vals_th)

        ! Moisture BC
        call self%compute_moisture_bc(qa, Ua, q_potential)

        ! Hydraulic: values(1)=q_potential, values(2)=Pmin, values(3)=Pmax (seepage BC)
        bc_vals_hyd(1) = q_potential
        bc_vals_hyd(2) = self%Pmin
        bc_vals_hyd(3) = self%Pmax
        call bc_hydraulic%update_bc_data(self%bc_entity_hydraulic, bc_vals_hyd)

        write (*, '(A,ES10.3,A,F7.2,A,F7.3,A,ES10.3,A,F8.3,A,F8.3)') &
            '[DA] t=', current_time, ' DOY=', current_doy, &
            ' Ta=', Ta, ' qa=', qa, ' h_eff=', h_eff, ' G*=', G_ast

        call mean_state%destroy()
    end subroutine execute_assimilation_cycle

    !> Computes friction velocity u_ast and aerodynamic resistances r_H, r_v.
    !> \( u_* = \kappa U / \ln(z/z_0) \), \( r_H = \ln^2(z/z_0)/(\kappa^2 U) \)
    subroutine compute_friction_velocity(self, Ua, u_ast, r_H, r_v)
        implicit none
        class(type_assimilation_controller), intent(in) :: self
        real(real64), intent(in) :: Ua
        real(real64), intent(inout) :: u_ast
        real(real64), intent(inout) :: r_H
        real(real64), intent(inout) :: r_v

        real(real64), parameter :: kappa = 0.4d0
        real(real64) :: ln_ratio, U_eff

        U_eff    = max(Ua, 0.1d0)
        ln_ratio = log(self%z_ref_U / max(self%z0, 1.0d-6))
        u_ast    = kappa * U_eff / ln_ratio
        r_H      = ln_ratio**2 / (kappa**2 * U_eff)
        r_v      = r_H  ! neutral stability; no Monin-Obukhov correction
    end subroutine compute_friction_velocity

    !> Computes linearized Robin BC parameters from surface energy balance.
    !> \( h_R = 4\varepsilon\sigma T_s^3 \), \( h_H = \rho_a c_a / r_H \),
    !> \( h_E = L_v \partial_T \rho_{vs} / (r_v + r_s) \)
    subroutine compute_surface_energy_balance(self, Ta, qa, r_H, r_v, h_eff, G_ast, T_ref)
        implicit none
        class(type_assimilation_controller), intent(in) :: self
        real(real64), intent(in) :: Ta, qa, r_H, r_v
        real(real64), intent(inout) :: h_eff, G_ast, T_ref

        real(real64) :: Ts_K, Lv, rho_vs_s, rho_vs_a
        real(real64) :: H_flux, E_pot, LvE, Rnet
        real(real64) :: h_R, h_H, h_E, drho_vs_dT

        Ts_K    = self%Ts_ast + T0_K_atm
        Lv      = 2.501d6 - 2361.0d0 * self%Ts_ast

        rho_vs_s = sat_vapor_density_atm(self%Ts_ast)
        rho_vs_a = qa * rho_air_atm

        Rnet    = self%Rs * (1.0d0 - self%albedo) + self%eps_s * self%Rl_down &
                  - self%eps_s * sigma_sb_atm * Ts_K**4

        H_flux  = rho_air_atm * c_air_atm / r_H * (self%Ts_ast - Ta)
        E_pot   = max(0.0d0, (rho_vs_s - rho_vs_a) / (r_v + self%r_s))
        LvE     = Lv * E_pot
        G_ast   = Rnet - H_flux - LvE

        ! Linearization
        h_R = 4.0d0 * self%eps_s * sigma_sb_atm * Ts_K**3
        h_H = rho_air_atm * c_air_atm / r_H
        drho_vs_dT = Lv * rho_vs_s / (Rv_atm * Ts_K**2)
        h_E = Lv * drho_vs_dT / (r_v + self%r_s)
        h_eff = h_R + h_H + h_E

        if (h_eff > 0.0d0) then
            T_ref = self%Ts_ast + G_ast / h_eff
        else
            T_ref = self%Ts_ast
        end if
    end subroutine compute_surface_energy_balance

    !> Computes net moisture flux q_potential [m/s] for seepage BC.
    !> Positive: net evaporation (drying); negative: net infiltration.
    subroutine compute_moisture_bc(self, qa, Ua, q_potential)
        implicit none
        class(type_assimilation_controller), intent(in) :: self
        real(real64), intent(in) :: qa, Ua
        real(real64), intent(inout) :: q_potential

        real(real64) :: rho_vs_s, rho_vs_a, r_H, r_v, u_ast
        real(real64) :: E_pot_kg, E_pot_ms

        call self%compute_friction_velocity(Ua, u_ast, r_H, r_v)
        rho_vs_s  = sat_vapor_density_atm(self%Ts_ast)
        rho_vs_a  = qa * rho_air_atm
        E_pot_kg  = max(0.0d0, (rho_vs_s - rho_vs_a) / (r_v + self%r_s))
        E_pot_ms  = E_pot_kg / rho_w_atm
        q_potential = E_pot_ms - self%precip
    end subroutine compute_moisture_bc

    !> Updates self%Rs using Spencer (1971) solar geometry.
    !> \( R_s = S_0 \max(0, \cos\theta_z) \tau \)
    !> where \(\theta_z\) is solar zenith angle from orbital elements.
    subroutine update_solar_radiation(self, current_doy)
        implicit none
        class(type_assimilation_controller), intent(inout) :: self
        real(real64), intent(in) :: current_doy

        real(real64), parameter :: pi_loc = acos(-1.0d0)
        real(real64), parameter :: S0_loc = 1361.0d0  ! solar constant [W/m2]
        real(real64) :: B, decl, EoT_h, utc_h, solar_h, omega, cos_theta, lat_rad

        B = 2.0d0 * pi_loc * (current_doy - 1.0d0) / 365.0d0

        ! Declination [rad] — Spencer (1971)
        decl = 0.006918d0 - 0.399912d0*cos(B)   + 0.070257d0*sin(B) &
             - 0.006758d0*cos(2.0d0*B) + 0.000907d0*sin(2.0d0*B) &
             - 0.002697d0*cos(3.0d0*B) + 0.00148d0 *sin(3.0d0*B)

        ! Equation of Time [hours]
        EoT_h = (0.000075d0 + 0.001868d0*cos(B)   - 0.032077d0*sin(B) &
               - 0.014615d0*cos(2.0d0*B) - 0.04089d0 *sin(2.0d0*B)) * (229.18d0/60.0d0)

        ! UTC fractional hour from DOY (DOY=1.0 → 2026-01-01 00:00 UTC)
        utc_h = mod((current_doy - 1.0d0) * 24.0d0, 24.0d0)

        ! Apparent Solar Time [h]: longitude correction (15 deg/h) + Equation of Time
        solar_h = mod(utc_h + self%longitude / 15.0d0 + EoT_h + 48.0d0, 24.0d0)

        ! Hour angle [rad]: 0 at solar noon
        omega = (solar_h - 12.0d0) * pi_loc / 12.0d0

        ! Cosine of zenith angle
        lat_rad = self%latitude * pi_loc / 180.0d0
        cos_theta = sin(lat_rad)*sin(decl) + cos(lat_rad)*cos(decl)*cos(omega)

        self%Rs = max(0.0d0, S0_loc * max(0.0d0, cos_theta) * self%tau_atm)
    end subroutine update_solar_radiation

    !> Saturation vapor density [kg/m3] using Buck equation.
    !> \[ e_s = 611.2 \exp\!\left(\frac{17.67\,T_C}{T_C + 243.5}\right)\,[\text{Pa}] \]
    pure function sat_vapor_density_atm(T_C) result(rho_vs)
        implicit none
        real(real64), intent(in) :: T_C
        real(real64) :: rho_vs

        real(real64) :: T_K, e_sat

        T_K   = T_C + T0_K_atm
        e_sat = 611.2d0 * exp(17.67d0 * T_C / (T_C + 243.5d0))
        rho_vs = e_sat / (Rv_atm * T_K)
    end function sat_vapor_density_atm

end module physics_governing_atmosphere_controller
