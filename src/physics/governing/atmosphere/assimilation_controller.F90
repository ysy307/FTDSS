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

    !> Single upper-boundary record (ERA5 ML135, ~50m height).
    type :: type_upper_bc_record
        real(real64) :: time_seconds = -1.0d0
        real(real64) :: T_upper      = 0.0d0   ! temperature [C]
        real(real64) :: q_upper      = 0.0d0   ! specific humidity [kg/kg]
        real(real64) :: U_upper      = 0.0d0   ! wind speed [m/s]
    end type type_upper_bc_record

    !> All parameters loaded from DataAssimilation.json.
    type :: type_da_config
        integer(int32)     :: ensemble_size       = 30
        real(real64)       :: max_height          = 200.0d0
        integer(int32)     :: num_nodes           = 50
        character(len=256) :: csv_file            = 'obs_data.csv'
        character(len=256) :: upper_bc_file       = 'upper_bc.csv'
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
        real(real64)       :: utc_offset_hours    = 0.0d0    ! local time = UTC + offset
    end type type_da_config

    !> Orchestrates ETKF cycles and surface BC parameter updates.
    type :: type_assimilation_controller
        type(type_ensemble_manager)   :: ensemble
        type(type_observation_manager):: obs_manager
        type(type_etkf_solver)        :: etkf_solver
        ! Upper boundary (ERA5 ML135) time series
        type(type_upper_bc_record), allocatable :: upper_bc_records(:)
        integer(int32) :: n_upper_bc = 0
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
        real(real64) :: utc_offset_hours = 0.0d0 ! local time = UTC + offset
        ! BC entity IDs
        integer(int32) :: bc_entity_thermal   = 3
        integer(int32) :: bc_entity_hydraulic = 3
        ! Surface state log
        integer(int32) :: log_io_unit   = -1
        real(real64)   :: last_log_time = -9999.0d0
        real(real64)   :: log_interval_s = 300.0d0
        ! Stored atmospheric state from last ETKF cycle (for inter-cycle logging)
        real(real64)   :: Ta_last = 0.0d0
        real(real64)   :: qa_last = 0.0d0
        real(real64)   :: Ua_last = 0.0d0
        logical :: has_first_obs  = .false.
        logical :: is_initialized = .false.
    contains
        procedure, public, pass(self) :: initialize         => initialize_da_controller
        procedure, public, pass(self) :: destroy            => destroy_da_controller
        procedure, public, pass(self) :: set_surface_state  => set_surface_state_controller
        procedure, public, pass(self) :: execute_assimilation_cycle
        procedure, private, pass(self) :: propagate_atmosphere
        procedure, private, pass(self) :: read_upper_bc
        procedure, private, pass(self) :: apply_upper_bc
        procedure, private, pass(self) :: write_surface_log
        procedure, private, pass(self) :: compute_friction_velocity
        procedure, private, pass(self) :: compute_surface_energy_balance
        procedure, private, pass(self) :: compute_surface_energy_diag
        procedure, private, pass(self) :: compute_moisture_bc
        procedure, private, pass(self) :: update_solar_radiation
        procedure, private, pass(self) :: update_longwave_radiation
    end type type_assimilation_controller

contains

    subroutine initialize_da_controller(self, config, bc_entity_thermal, bc_entity_hydraulic)
        implicit none
        class(type_assimilation_controller), intent(inout) :: self
        type(type_da_config), intent(in) :: config
        integer(int32), intent(in) :: bc_entity_thermal
        integer(int32), intent(in) :: bc_entity_hydraulic

        character(len=512) :: project_path, log_path
        integer(int32) :: status_env, stat, tmp_io_unit

        self%z0                  = config%z0
        self%eps_s               = config%emissivity
        self%albedo              = config%albedo
        self%Pmin                = config%Pmin
        self%Pmax                = config%Pmax
        self%r_s                 = config%stomatal_resistance
        self%latitude            = config%latitude
        self%longitude           = config%longitude
        self%tau_atm             = config%tau_atm
        self%utc_offset_hours    = config%utc_offset_hours
        self%bc_entity_thermal   = bc_entity_thermal
        self%bc_entity_hydraulic = bc_entity_hydraulic

        call self%ensemble%initialize(config%ensemble_size, config%max_height, config%num_nodes)
        call self%ensemble%add_perturbation(config%sigma_T, config%sigma_q, config%sigma_U)

        call self%obs_manager%initialize( &
            trim(config%csv_file), &
            trim(config%reference_datetime), &
            config%interval_seconds, &
            config%sigma_T, config%sigma_q, config%sigma_U)

        call self%read_upper_bc(trim(config%upper_bc_file), trim(config%reference_datetime))

        call self%etkf_solver%initialize(config%ensemble_size)

        ! Open surface state log
        call get_environment_variable('FTCMS_PROJECT_PATH', project_path, status=status_env)
        if (status_env == 0) then
            log_path = trim(project_path)//'/Output/da_surface.csv'
            open (newunit=tmp_io_unit, file=trim(log_path), status='replace', &
                  action='write', iostat=stat)
            if (stat == 0) then
                self%log_io_unit = tmp_io_unit
                write (self%log_io_unit, '(A)') &
                    'time_s,DOY,Ta,qa,Ua,Ts,Rs,Rnet,H,LE,G_ast,h_eff,T_ref,E_pot_ms,precip,q_pot'
                flush (self%log_io_unit)
            end if
        end if

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
        if (allocated(self%upper_bc_records)) deallocate (self%upper_bc_records)
        if (self%log_io_unit /= -1) close (self%log_io_unit)
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
        real(real64) :: Rnet, H_flux, LvE, E_pot_ms
        real(real64) :: q_potential
        real(real64) :: bc_vals_th(3), bc_vals_hyd(3)
        real(real64) :: precip_rate

        if (.not. self%is_initialized) return

        ! Update solar shortwave from orbital geometry
        call self%update_solar_radiation(current_doy)

        ! Apply ERA5 ML135 upper Dirichlet BC to all ensemble members every step
        call self%apply_upper_bc(current_time)

        call self%obs_manager%get_observation(current_time, y, R, is_available, precip_rate)
        if (is_available) then
            do ii = 1, 3
                R_diag(ii) = R(ii, ii)
            end do

            ! ETKF analysis: pass y and R_diag directly
            call self%etkf_solver%calculate_analysis(self%ensemble, self%obs_manager, y, R_diag)

            ! Re-inject perturbations to prevent ensemble collapse
            call self%ensemble%add_perturbation( &
                0.5d0 * self%obs_manager%sigma_T, &
                0.5d0 * self%obs_manager%sigma_q, &
                0.5d0 * self%obs_manager%sigma_U)

            ! Re-apply upper BC after perturbation to keep top node anchored
            call self%apply_upper_bc(current_time)

            ! Analysis mean
            call self%ensemble%compute_mean(mean_state)
            self%Ta_last = mean_state%T(1)
            self%qa_last = mean_state%q(1)
            self%Ua_last = mean_state%U(1)
            self%precip  = precip_rate
            self%has_first_obs = .true.
            call mean_state%destroy()
        else
            ! Between DA cycles: ensure we have a baseline Ta, qa, Ua
            if (.not. self%has_first_obs) then
                call self%ensemble%compute_mean(mean_state)
                self%Ta_last = mean_state%T(1)
                self%qa_last = mean_state%q(1)
                self%Ua_last = mean_state%U(1)
                call mean_state%destroy()
            end if

            ! Log at specified intervals even without observations
            if (self%has_first_obs .and. self%log_io_unit /= -1 .and. &
                current_time - self%last_log_time >= self%log_interval_s - 1.0d0) then
                call self%write_surface_log(current_time, current_doy, &
                                            self%Ta_last, self%qa_last, self%Ua_last)
            end if
        end if

        ! --- ALWAYS update surface energy balance and BC parameters ---
        Ta = self%Ta_last
        qa = self%qa_last
        Ua = self%Ua_last

        ! Physical propagation of atmospheric state (1D vertical diffusion)
        ! This ensures T(1) doesn't stay stagnant between DA cycles.
        call self%propagate_atmosphere(current_time)

        ! Re-fetch mean state after propagation
        call self%ensemble%compute_mean(mean_state)
        Ta = mean_state%T(1)
        qa = mean_state%q(1)
        Ua = mean_state%U(1)
        call mean_state%destroy()

        ! Update downwelling longwave from current Ta, qa
        call self%update_longwave_radiation(Ta, qa)

        ! Aerodynamic resistances
        call self%compute_friction_velocity(Ua, u_ast, r_H, r_v)

        ! Surface energy balance -> Robin BC parameters
        call self%compute_surface_energy_balance(Ta, qa, r_H, r_v, h_eff, G_ast, T_ref)

        ! Thermal: values(1)=h_eff, values(2)=T_ref for Robin BC
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

        if (is_available) then
            write (*, '(A,ES10.3,A,F7.2,A,F7.3,A,ES10.3,A,F8.3,A,F8.3)') &
                '[DA] t=', current_time, ' DOY=', current_doy, &
                ' Ta=', Ta, ' qa=', qa, ' h_eff=', h_eff, ' G*=', G_ast
            call self%write_surface_log(current_time, current_doy, Ta, qa, Ua)
        end if
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
    !> Linearization is performed around \( T_a \) (NOT \( T_s \)) to avoid the
    !> self-referential loop \( T_\text{ref} = T_s + G^*/h \) that drives G* -> 0.
    !> \( G(T_a) = R_\text{net}(T_a) - L_v E(T_a) \) (H=0 at \( T_s=T_a \))
    !> \( T_\text{ref} = T_a + G(T_a)/h_\text{eff} \)
    !> G_ast is the diagnostic ground heat flux evaluated at the actual \( T_s \).
    subroutine compute_surface_energy_balance(self, Ta, qa, r_H, r_v, h_eff, G_ast, T_ref)
        implicit none
        class(type_assimilation_controller), intent(in) :: self
        real(real64), intent(in) :: Ta, qa, r_H, r_v
        real(real64), intent(inout) :: h_eff, G_ast, T_ref

        real(real64) :: Ta_K, Ts_K, Lv_a, Lv_s
        real(real64) :: rho_vs_Ta, rho_vs_Ts, rho_va
        real(real64) :: H_flux, E_pot_Ta, LvE_Ta, Rnet_Ta, G_at_Ta
        real(real64) :: E_pot_Ts, LvE_Ts, Rnet_Ts
        real(real64) :: h_R, h_H, h_E, drho_vs_dT_a

        ! ---- BC linearization around Ta ----
        Ta_K     = Ta + T0_K_atm
        Lv_a     = 2.501d6 - 2361.0d0 * Ta
        rho_vs_Ta = sat_vapor_density_atm(Ta)
        rho_va   = qa * rho_air_atm

        ! G at T_s = Ta: sensible heat vanishes, H=0
        Rnet_Ta  = self%Rs * (1.0d0 - self%albedo) + self%eps_s * self%Rl_down &
                   - self%eps_s * sigma_sb_atm * Ta_K**4
        E_pot_Ta = max(0.0d0, (rho_vs_Ta - rho_va) / (r_v + self%r_s))
        LvE_Ta   = Lv_a * E_pot_Ta
        G_at_Ta  = Rnet_Ta - LvE_Ta

        ! h_eff linearization coefficients at Ta
        h_R = 4.0d0 * self%eps_s * sigma_sb_atm * Ta_K**3
        h_H = rho_air_atm * c_air_atm / r_H
        drho_vs_dT_a = Lv_a * rho_vs_Ta / (Rv_atm * Ta_K**2)
        h_E = Lv_a * drho_vs_dT_a / (r_v + self%r_s)
        h_eff = h_R + h_H + h_E

        if (h_eff > 0.0d0) then
            T_ref = Ta + G_at_Ta / h_eff
        else
            T_ref = Ta
        end if

        ! ---- Diagnostic G_ast at actual Ts_ast ----
        Ts_K     = self%Ts_ast + T0_K_atm
        Lv_s     = 2.501d6 - 2361.0d0 * self%Ts_ast
        rho_vs_Ts = sat_vapor_density_atm(self%Ts_ast)
        Rnet_Ts  = self%Rs * (1.0d0 - self%albedo) + self%eps_s * self%Rl_down &
                   - self%eps_s * sigma_sb_atm * Ts_K**4
        H_flux   = rho_air_atm * c_air_atm / r_H * (self%Ts_ast - Ta)
        E_pot_Ts = max(0.0d0, (rho_vs_Ts - rho_va) / (r_v + self%r_s))
        LvE_Ts   = Lv_s * E_pot_Ts
        G_ast    = Rnet_Ts - H_flux - LvE_Ts
    end subroutine compute_surface_energy_balance

    !> Same physics as compute_surface_energy_balance but returns diagnostic fluxes.
    subroutine compute_surface_energy_diag(self, Ta, qa, r_H, r_v, Rnet, H_flux, LvE, E_pot_ms)
        implicit none
        class(type_assimilation_controller), intent(in) :: self
        real(real64), intent(in) :: Ta, qa, r_H, r_v
        real(real64), intent(inout) :: Rnet, H_flux, LvE, E_pot_ms

        real(real64) :: Ts_K, Lv, rho_vs_s, rho_vs_a, E_pot

        Ts_K    = self%Ts_ast + T0_K_atm
        Lv      = 2.501d6 - 2361.0d0 * self%Ts_ast
        rho_vs_s = sat_vapor_density_atm(self%Ts_ast)
        rho_vs_a = qa * rho_air_atm
        Rnet    = self%Rs * (1.0d0 - self%albedo) + self%eps_s * self%Rl_down &
                  - self%eps_s * sigma_sb_atm * Ts_K**4
        H_flux  = rho_air_atm * c_air_atm / r_H * (self%Ts_ast - Ta)
        E_pot   = max(0.0d0, (rho_vs_s - rho_vs_a) / (r_v + self%r_s))
        LvE     = Lv * E_pot
        E_pot_ms = E_pot / rho_w_atm
    end subroutine compute_surface_energy_diag

    !> Reads upper_bc.csv from FTCMS_PROJECT_PATH/Input/.
    !> Expected header: valid_time,T_upper,q_upper,U_upper
    subroutine read_upper_bc(self, csv_filename, ref_datetime_str)
        implicit none
        class(type_assimilation_controller), intent(inout) :: self
        character(len=*), intent(in) :: csv_filename
        character(len=*), intent(in) :: ref_datetime_str

        character(len=512) :: project_path, filepath, line
        character(len=32)  :: dt_str
        integer(int32) :: io_unit, stat, n_lines, i, status_env
        integer(int32) :: yr, mo, dy, ref_yr, ref_mo, ref_dy
        real(real64) :: T_v, q_v, U_v, hr_frac, elapsed

        call get_environment_variable('FTCMS_PROJECT_PATH', project_path, status=status_env)
        if (status_env /= 0) return

        ! Parse reference datetime
        read (ref_datetime_str(1:4),  *, iostat=stat) ref_yr
        read (ref_datetime_str(6:7),  *, iostat=stat) ref_mo
        read (ref_datetime_str(9:10), *, iostat=stat) ref_dy

        filepath = trim(project_path)//'/Input/'//trim(csv_filename)
        open (newunit=io_unit, file=trim(filepath), status='old', action='read', iostat=stat)
        if (stat /= 0) then
            write (*, '(A,A)') '[DA] WARNING: cannot open upper_bc CSV: ', trim(filepath)
            return
        end if

        n_lines = 0
        read (io_unit, '(A)', iostat=stat)  ! skip header
        do
            read (io_unit, '(A)', iostat=stat) line
            if (stat /= 0) exit
            if (len_trim(line) > 0) n_lines = n_lines + 1
        end do
        rewind (io_unit)
        read (io_unit, '(A)', iostat=stat)

        if (allocated(self%upper_bc_records)) deallocate (self%upper_bc_records)
        allocate (self%upper_bc_records(n_lines))
        self%n_upper_bc = 0

        do i = 1, n_lines
            read (io_unit, '(A)', iostat=stat) line
            if (stat /= 0) exit
            if (len_trim(line) == 0) cycle

            call parse_upper_bc_line(line, dt_str, T_v, q_v, U_v, stat)
            if (stat /= 0) cycle

            read (dt_str(1:4),  *, iostat=stat) yr
            read (dt_str(6:7),  *, iostat=stat) mo
            read (dt_str(9:10), *, iostat=stat) dy
            hr_frac = parse_hour_frac_ctrl(trim(dt_str))
            elapsed = real(julian_day_ctrl(yr,mo,dy) - julian_day_ctrl(ref_yr,ref_mo,ref_dy), real64) &
                      * 86400.0d0 + hr_frac * 3600.0d0

            self%n_upper_bc = self%n_upper_bc + 1
            self%upper_bc_records(self%n_upper_bc)%time_seconds = elapsed
            self%upper_bc_records(self%n_upper_bc)%T_upper      = T_v
            self%upper_bc_records(self%n_upper_bc)%q_upper      = q_v
            self%upper_bc_records(self%n_upper_bc)%U_upper      = U_v
        end do
        close (io_unit)
        write (*, '(A,I0,A)') '[DA] Loaded ', self%n_upper_bc, ' upper BC records from '//trim(csv_filename)
    end subroutine read_upper_bc

    !> Sets the top node of every ensemble member to the ERA5 ML135 value at current_time.
    subroutine apply_upper_bc(self, current_time)
        implicit none
        class(type_assimilation_controller), intent(inout) :: self
        real(real64), intent(in) :: current_time

        integer(int32) :: i, best, k
        real(real64) :: dt, best_dt, T_bc, q_bc, U_bc, n_nodes_r
        integer(int32) :: n_nodes

        if (self%n_upper_bc == 0) return

        best    = 1
        best_dt = huge(1.0d0)
        do i = 1, self%n_upper_bc
            dt = abs(self%upper_bc_records(i)%time_seconds - current_time)
            if (dt < best_dt) then
                best_dt = dt
                best    = i
            end if
        end do

        T_bc = self%upper_bc_records(best)%T_upper
        q_bc = self%upper_bc_records(best)%q_upper
        U_bc = self%upper_bc_records(best)%U_upper

        n_nodes = self%ensemble%n_nodes
        do k = 1, self%ensemble%n_members
            self%ensemble%members(k)%T(n_nodes) = T_bc
            self%ensemble%members(k)%q(n_nodes) = q_bc
            self%ensemble%members(k)%U(n_nodes) = U_bc
        end do
    end subroutine apply_upper_bc

    pure function julian_day_ctrl(yr, mo, dy) result(jdn)
        implicit none
        integer(int32), intent(in) :: yr, mo, dy
        integer(int32) :: jdn, a, y, m
        a = (14 - mo) / 12
        y = yr + 4800 - a
        m = mo + 12*a - 3
        jdn = dy + (153*m + 2)/5 + 365*y + y/4 - y/100 + y/400 - 32045
    end function julian_day_ctrl

    pure function parse_hour_frac_ctrl(dt_str) result(hr_frac)
        implicit none
        character(len=*), intent(in) :: dt_str
        real(real64) :: hr_frac
        integer(int32) :: hr, mn, sc, stat, t_pos
        hr_frac = 0.0d0
        t_pos = index(dt_str, 'T')
        if (t_pos == 0) then
            ! Space-separated: "YYYY-MM-DD HH:MM:SS"
            if (len_trim(dt_str) >= 19) then
                read (dt_str(12:13), *, iostat=stat) hr
                read (dt_str(15:16), *, iostat=stat) mn
                read (dt_str(18:19), *, iostat=stat) sc
            else
                return
            end if
        else
            if (len_trim(dt_str) < t_pos + 7) return
            read (dt_str(t_pos+1:t_pos+2), *, iostat=stat) hr
            read (dt_str(t_pos+4:t_pos+5), *, iostat=stat) mn
            read (dt_str(t_pos+7:t_pos+8), *, iostat=stat) sc
        end if
        hr_frac = real(hr, real64) + real(mn, real64)/60.0d0 + real(sc, real64)/3600.0d0
    end function parse_hour_frac_ctrl

    subroutine parse_upper_bc_line(line, dt_str, T_v, q_v, U_v, stat)
        implicit none
        character(len=*), intent(in) :: line
        character(len=32), intent(inout) :: dt_str
        real(real64), intent(inout) :: T_v, q_v, U_v
        integer(int32), intent(inout) :: stat

        integer(int32) :: c1, c2, c3

        stat = 1
        c1 = index(line, ',')
        if (c1 == 0) return
        c2 = c1 + index(line(c1+1:), ',')
        if (c2 == c1) return
        c3 = c2 + index(line(c2+1:), ',')
        if (c3 == c2) return

        dt_str = line(1:c1-1)
        read (line(c1+1:c2-1), *, iostat=stat) T_v; if (stat /= 0) return
        read (line(c2+1:c3-1), *, iostat=stat) q_v; if (stat /= 0) return
        read (line(c3+1:),     *, iostat=stat) U_v
    end subroutine parse_upper_bc_line

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

        ! UTC fractional hour from local DOY (local = UTC + offset)
        utc_h = mod((current_doy - 1.0d0) * 24.0d0 - self%utc_offset_hours + 48.0d0, 24.0d0)

        ! Apparent Solar Time [h]: longitude correction (15 deg/h) + Equation of Time
        solar_h = mod(utc_h + self%longitude / 15.0d0 + EoT_h + 48.0d0, 24.0d0)

        ! Hour angle [rad]: 0 at solar noon
        omega = (solar_h - 12.0d0) * pi_loc / 12.0d0

        ! Cosine of zenith angle
        lat_rad = self%latitude * pi_loc / 180.0d0
        cos_theta = sin(lat_rad)*sin(decl) + cos(lat_rad)*cos(decl)*cos(omega)

        self%Rs = max(0.0d0, S0_loc * max(0.0d0, cos_theta) * self%tau_atm)
    end subroutine update_solar_radiation

    !> Estimates downwelling longwave radiation from Ta and qa (Brutsaert 1975).
    !> \( \varepsilon_a = 1.24\,(e_a/T_a)^{1/7} \), \( R_{l\downarrow} = \varepsilon_a \sigma T_a^4 \)
    !> where \( e_a \) [hPa] is the near-surface vapor pressure derived from specific humidity.
    subroutine update_longwave_radiation(self, Ta, qa)
        implicit none
        class(type_assimilation_controller), intent(inout) :: self
        real(real64), intent(in) :: Ta  ! air temperature [C]
        real(real64), intent(in) :: qa  ! specific humidity [kg/kg]

        real(real64), parameter :: P_sfc = 101325.0d0  ! surface pressure [Pa]
        real(real64), parameter :: eps_r = 0.622d0      ! ratio mol weight vapor/dry air
        real(real64) :: Ta_K, ea_hPa, eps_a

        Ta_K   = Ta + T0_K_atm
        ! Vapor pressure [hPa] from specific humidity
        ea_hPa = qa * P_sfc / (eps_r + qa) * 0.01d0
        ! Brutsaert (1975) clear-sky emissivity
        eps_a  = 1.24d0 * (ea_hPa / Ta_K)**(1.0d0/7.0d0)
        eps_a  = min(eps_a, 1.0d0)
        self%Rl_down = eps_a * sigma_sb_atm * Ta_K**4
    end subroutine update_longwave_radiation

    !> Performs physical propagation of the 1D atmospheric column using vertical diffusion.
    !> Uses an implicit (Backward Euler) scheme for stability with 300s time steps.
    subroutine propagate_atmosphere(self, current_time)
        implicit none
        class(type_assimilation_controller), intent(inout) :: self
        real(real64), intent(in) :: current_time

        integer(int32) :: k, i, n
        real(real64) :: dt, dz, Kz, alpha
        real(real64), allocatable :: a(:), b(:), c(:), r_T(:), r_q(:)
        real(real64) :: u_ast, r_H, r_v

        if (self%last_log_time < -9000.0d0) then
            self%last_log_time = current_time
            return
        end if
        dt = current_time - self%last_log_time
        if (dt <= 0.0d0) return

        n = self%ensemble%n_nodes
        allocate (a(n), b(n), c(n), r_T(n), r_q(n))

        do k = 1, self%ensemble%n_members
            ! Estimate eddy diffusivity Kz from surface friction
            call self%compute_friction_velocity(self%ensemble%members(k)%U(1), u_ast, r_H, r_v)
            Kz = max(0.1d0, 0.4d0 * u_ast * 10.0d0) ! Kz at ~10m

            ! Build tridiagonal matrix for implicit diffusion
            ! T_i^{n+1} - T_i^n = dt * Kz * (T_{i+1} - 2*T_i + T_{i-1}) / dz^2
            ! -alpha*T_{i-1} + (1+2*alpha)*T_i - alpha*T_{i+1} = T_i^n
            do i = 2, n - 1
                dz = self%ensemble%members(k)%z(i+1) - self%ensemble%members(k)%z(i)
                alpha = Kz * dt / (dz**2)
                a(i) = -alpha
                b(i) = 1.0d0 + 2.0d0 * alpha
                c(i) = -alpha
                r_T(i) = self%ensemble%members(k)%T(i)
                r_q(i) = self%ensemble%members(k)%q(i)
            end do

            ! Bottom BC (z=0): Dirichlet BC from soil surface
            a(1) = 0.0d0
            b(1) = 1.0d0
            c(1) = 0.0d0
            r_T(1) = self%Ts_ast
            r_q(1) = self%qa_last ! Fallback or more sophisticated coupling if needed

            ! Top BC (z=50m): Dirichlet (already set by apply_upper_bc)
            a(n) = 0.0d0
            b(n) = 1.0d0
            c(n) = 0.0d0
            r_T(n) = self%ensemble%members(k)%T(n)
            r_q(n) = self%ensemble%members(k)%q(n)

            ! Simple Thomas algorithm (Tridiagonal matrix solver)
            do i = 2, n
                alpha = a(i) / b(i-1)
                b(i) = b(i) - alpha * c(i-1)
                r_T(i) = r_T(i) - alpha * r_T(i-1)
                r_q(i) = r_q(i) - alpha * r_q(i-1)
            end do
            self%ensemble%members(k)%T(n) = r_T(n) / b(n)
            self%ensemble%members(k)%q(n) = r_q(n) / b(n)
            do i = n - 1, 1, -1
                self%ensemble%members(k)%T(i) = (r_T(i) - c(i) * self%ensemble%members(k)%T(i+1)) / b(i)
                self%ensemble%members(k)%q(i) = (r_q(i) - c(i) * self%ensemble%members(k)%q(i+1)) / b(i)
            end do
        end do

        deallocate (a, b, c, r_T, r_q)
    end subroutine propagate_atmosphere

    !> Computes surface energy diagnostics and writes one row to the surface log CSV.
    subroutine write_surface_log(self, current_time, current_doy, Ta, qa, Ua)
        implicit none
        class(type_assimilation_controller), intent(inout) :: self
        real(real64), intent(in) :: current_time, current_doy, Ta, qa, Ua

        real(real64) :: u_ast, r_H, r_v
        real(real64) :: h_eff, G_ast, T_ref
        real(real64) :: Rnet, H_flux, LvE, E_pot_ms, q_potential

        if (self%log_io_unit == -1) return

        call self%compute_friction_velocity(Ua, u_ast, r_H, r_v)
        call self%compute_surface_energy_balance(Ta, qa, r_H, r_v, h_eff, G_ast, T_ref)
        call self%compute_surface_energy_diag(Ta, qa, r_H, r_v, Rnet, H_flux, LvE, E_pot_ms)
        call self%compute_moisture_bc(qa, Ua, q_potential)

        write (self%log_io_unit, '(ES15.8,15(",",ES15.8))') &
            current_time, current_doy, Ta, qa, Ua, self%Ts_ast, &
            self%Rs, Rnet, H_flux, LvE, G_ast, h_eff, T_ref, E_pot_ms, self%precip, q_potential
        flush (self%log_io_unit)

        self%last_log_time = current_time
    end subroutine write_surface_log

    !> Saturation vapor density [kg/m3] using Buck (1981) equations.
    !> Handles both water (T > 0) and ice (T <= 0) phases.
    pure function sat_vapor_density_atm(T_C) result(rho_vs)
        implicit none
        real(real64), intent(in) :: T_C
        real(real64) :: rho_vs

        real(real64) :: T_K, e_sat

        T_K = T_C + T0_K_atm
        if (T_C > 0.0d0) then
            ! Buck (1981) for water
            e_sat = 611.21d0 * exp(17.502d0 * T_C / (T_C + 240.97d0))
        else
            ! Buck (1981) for ice
            e_sat = 611.15d0 * exp(22.452d0 * T_C / (T_C + 272.55d0))
        end if
        rho_vs = e_sat / (Rv_atm * T_K)
    end function sat_vapor_density_atm

end module physics_governing_atmosphere_controller
