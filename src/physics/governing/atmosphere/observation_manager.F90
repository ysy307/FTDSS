!> Observation manager: reads CSV time-series and provides observation vectors for ETKF.
module physics_governing_atmosphere_observation
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: physics_governing_atmosphere_ensemble, only: type_atmos_state
    implicit none
    private

    public :: type_obs_record
    public :: type_observation_manager

    integer(int32), parameter :: N_OBS_ATM = 3  ! T at z_T, q at z_T, U at z_U

    !> Single observation record parsed from CSV.
    type :: type_obs_record
        real(real64) :: time_seconds = -1.0d0  ! elapsed from reference datetime [s]
        real(real64) :: doy          = 0.0d0
        real(real64) :: T_obs        = 0.0d0   ! temperature [C]
        real(real64) :: q_obs        = 0.0d0   ! specific humidity [kg/kg]
        real(real64) :: U_obs        = 0.0d0   ! wind speed [m/s]
        real(real64) :: precip_rate  = 0.0d0   ! precipitation [m/s]
        logical      :: is_valid     = .false.
    end type type_obs_record

    !> Manages CSV observation data and constructs observation operators.
    type :: type_observation_manager
        type(type_obs_record), allocatable :: records(:)
        integer(int32) :: n_records        = 0
        real(real64)   :: z_T              = 50.0d0   ! obs height for T and q [m] (ERA5 level 135 ~54m, top of column)
        real(real64)   :: z_U              = 50.0d0   ! obs height for U [m]
        real(real64)   :: interval_seconds = 3600.0d0
        real(real64)   :: sigma_T          = 0.5d0
        real(real64)   :: sigma_q          = 1.0d-4
        real(real64)   :: sigma_U          = 0.5d0
        integer(int32) :: ref_year         = 2000
        integer(int32) :: ref_month        = 1
        integer(int32) :: ref_day          = 1
        integer(int32) :: last_assim_idx   = -1  ! prevents re-assimilating same obs epoch
    contains
        procedure, public, pass(self) :: initialize    => initialize_obs_manager
        procedure, public, pass(self) :: destroy       => destroy_obs_manager
        procedure, public, pass(self) :: read_csv      => read_csv_obs_manager
        procedure, public, pass(self) :: get_observation => get_observation_obs_manager
        procedure, public, pass(self) :: build_H       => build_H_obs_manager
        procedure, private, pass(self) :: parse_datetime_obs
        procedure, private, pass(self) :: elapsed_seconds_from_ref
    end type type_observation_manager

contains

    subroutine initialize_obs_manager(self, csv_filename, ref_datetime_str, &
                                      interval, sigma_T, sigma_q, sigma_U)
        implicit none
        class(type_observation_manager), intent(inout) :: self
        character(len=*), intent(in) :: csv_filename
        character(len=*), intent(in) :: ref_datetime_str  ! "YYYY-MM-DDTHH:MM:SS"
        real(real64), intent(in) :: interval
        real(real64), intent(in) :: sigma_T, sigma_q, sigma_U

        self%interval_seconds = interval
        self%sigma_T          = sigma_T
        self%sigma_q          = sigma_q
        self%sigma_U          = sigma_U

        call self%parse_datetime_obs(ref_datetime_str, self%ref_year, self%ref_month, self%ref_day)
        call self%read_csv(csv_filename)
    end subroutine initialize_obs_manager

    subroutine destroy_obs_manager(self)
        implicit none
        class(type_observation_manager), intent(inout) :: self

        if (allocated(self%records)) deallocate (self%records)
        self%n_records = 0
    end subroutine destroy_obs_manager

    !> Reads CSV from FTCMS_PROJECT_PATH/Input/<csv_filename>.
    !> Expected header: valid_time,T_obs,q_obs,U_obs,precip_rate
    subroutine read_csv_obs_manager(self, csv_filename)
        implicit none
        class(type_observation_manager), intent(inout) :: self
        character(len=*), intent(in) :: csv_filename

        character(len=512) :: project_path, filepath, line
        character(len=32)  :: dt_str
        integer(int32) :: io_unit, stat, n_lines, i, status_env
        integer(int32) :: yr, mo, dy
        real(real64) :: T_v, q_v, U_v, pr_v, hr_frac, elapsed

        call get_environment_variable('FTCMS_PROJECT_PATH', project_path, status=status_env)
        if (status_env /= 0) then
            write (*, '(A)') '[DA] WARNING: FTCMS_PROJECT_PATH not set; skipping CSV read.'
            return
        end if

        filepath = trim(project_path)//'/Input/'//trim(csv_filename)

        open (newunit=io_unit, file=trim(filepath), status='old', action='read', iostat=stat)
        if (stat /= 0) then
            write (*, '(A,A)') '[DA] WARNING: cannot open observation CSV: ', trim(filepath)
            return
        end if

        ! Count data lines (skip header)
        n_lines = 0
        read (io_unit, '(A)', iostat=stat)
        do
            read (io_unit, '(A)', iostat=stat) line
            if (stat /= 0) exit
            if (len_trim(line) > 0) n_lines = n_lines + 1
        end do

        rewind (io_unit)
        read (io_unit, '(A)', iostat=stat)

        if (allocated(self%records)) deallocate (self%records)
        allocate (self%records(n_lines))
        self%n_records = 0

        do i = 1, n_lines
            read (io_unit, '(A)', iostat=stat) line
            if (stat /= 0) exit
            if (len_trim(line) == 0) cycle

            call parse_csv_line_atm(line, dt_str, T_v, q_v, U_v, pr_v, stat)
            if (stat /= 0) cycle

            call self%parse_datetime_obs(trim(dt_str), yr, mo, dy)
            hr_frac = parse_time_of_day_atm(trim(dt_str))
            elapsed = self%elapsed_seconds_from_ref(yr, mo, dy, hr_frac)

            self%n_records = self%n_records + 1
            self%records(self%n_records)%time_seconds = elapsed
            self%records(self%n_records)%doy          = day_of_year_atm(yr, mo, dy) + hr_frac / 24.0d0
            self%records(self%n_records)%T_obs         = T_v
            self%records(self%n_records)%q_obs         = q_v
            self%records(self%n_records)%U_obs         = U_v
            self%records(self%n_records)%precip_rate    = pr_v
            self%records(self%n_records)%is_valid       = .true.
        end do

        close (io_unit)
        write (*, '(A,I0,A,A)') '[DA] Loaded ', self%n_records, ' obs records from ', trim(csv_filename)
    end subroutine read_csv_obs_manager

    !> Returns observation vector y(3), error covariance R(3,3), availability flag, and precipitation.
    subroutine get_observation_obs_manager(self, current_time, y, R, is_available, precip_rate)
        implicit none
        class(type_observation_manager), intent(inout) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(inout) :: y(N_OBS_ATM)
        real(real64), intent(inout) :: R(N_OBS_ATM, N_OBS_ATM)
        logical, intent(inout) :: is_available
        real(real64), intent(inout) :: precip_rate

        integer(int32) :: i, best_idx
        real(real64) :: dt, best_dt, tol

        is_available  = .false.
        y             = 0.0d0
        R             = 0.0d0
        precip_rate   = 0.0d0

        ! tol = half a 300s timestep: assimilates only when current_time is at an obs epoch.
        tol      = 150.0d0
        best_dt  = huge(1.0d0)
        best_idx = -1

        do i = 1, self%n_records
            if (.not. self%records(i)%is_valid) cycle
            ! Causal: do not use future observations
            if (self%records(i)%time_seconds > current_time + tol) cycle
            dt = abs(self%records(i)%time_seconds - current_time)
            if (dt < best_dt) then
                best_dt  = dt
                best_idx = i
            end if
        end do

        if (best_idx < 0 .or. best_dt > tol) return
        if (best_idx == self%last_assim_idx) return  ! same epoch already assimilated

        self%last_assim_idx = best_idx
        is_available = .true.
        y(1) = self%records(best_idx)%T_obs
        y(2) = self%records(best_idx)%q_obs
        y(3) = self%records(best_idx)%U_obs
        precip_rate = self%records(best_idx)%precip_rate

        R(1, 1) = self%sigma_T**2
        R(2, 2) = self%sigma_q**2
        R(3, 3) = self%sigma_U**2
    end subroutine get_observation_obs_manager

    !> Builds observation operator H(3, 3*n_nodes) using linear interpolation.
    !> State ordering: [T(1:n), q(1:n), U(1:n)].
    subroutine build_H_obs_manager(self, mean_state, H_mat)
        implicit none
        class(type_observation_manager), intent(inout) :: self
        type(type_atmos_state), intent(in) :: mean_state
        real(real64), allocatable, intent(inout) :: H_mat(:, :)

        integer(int32) :: n, k_lo, k_hi
        real(real64) :: alpha_T, alpha_U

        n = mean_state%n_nodes
        if (allocated(H_mat)) deallocate (H_mat)
        allocate (H_mat(N_OBS_ATM, 3*n))
        H_mat = 0.0d0

        call find_interp_indices_atm(mean_state%z, self%z_T, k_lo, k_hi, alpha_T)
        if (k_lo >= 1 .and. k_hi <= n) then
            H_mat(1, k_lo)         = 1.0d0 - alpha_T
            H_mat(1, k_hi)         = alpha_T
            H_mat(2, n + k_lo)     = 1.0d0 - alpha_T
            H_mat(2, n + k_hi)     = alpha_T
        end if

        call find_interp_indices_atm(mean_state%z, self%z_U, k_lo, k_hi, alpha_U)
        if (k_lo >= 1 .and. k_hi <= n) then
            H_mat(3, 2*n + k_lo)   = 1.0d0 - alpha_U
            H_mat(3, 2*n + k_hi)   = alpha_U
        end if
    end subroutine build_H_obs_manager

    subroutine find_interp_indices_atm(z, z_target, k_lo, k_hi, alpha)
        implicit none
        real(real64), intent(in) :: z(:)
        real(real64), intent(in) :: z_target
        integer(int32), intent(inout) :: k_lo, k_hi
        real(real64), intent(inout) :: alpha

        integer(int32) :: n, k

        n     = size(z)
        k_lo  = 1
        k_hi  = 1
        alpha = 0.0d0

        if (n < 2) return

        if (z_target <= z(1)) then
            k_lo = 1; k_hi = 2; alpha = 0.0d0; return
        end if
        if (z_target >= z(n)) then
            k_lo = n - 1; k_hi = n; alpha = 1.0d0; return
        end if

        do k = 1, n - 1
            if (z(k) <= z_target .and. z_target <= z(k + 1)) then
                k_lo  = k
                k_hi  = k + 1
                alpha = (z_target - z(k)) / (z(k + 1) - z(k))
                return
            end if
        end do
    end subroutine find_interp_indices_atm

    subroutine parse_datetime_obs(self, dt_str, yr, mo, dy)
        implicit none
        class(type_observation_manager), intent(in) :: self
        character(len=*), intent(in) :: dt_str
        integer(int32), intent(inout) :: yr, mo, dy

        integer(int32) :: stat

        read (dt_str(1:4), *, iostat=stat) yr
        read (dt_str(6:7), *, iostat=stat) mo
        read (dt_str(9:10), *, iostat=stat) dy
    end subroutine parse_datetime_obs

    pure function elapsed_seconds_from_ref(self, yr, mo, dy, hr_frac) result(elapsed)
        implicit none
        class(type_observation_manager), intent(in) :: self
        integer(int32), intent(in) :: yr, mo, dy
        real(real64), intent(in) :: hr_frac
        real(real64) :: elapsed

        elapsed = real(julian_day_atm(yr, mo, dy) - julian_day_atm(self%ref_year, self%ref_month, self%ref_day), &
                       real64) * 86400.0d0 + hr_frac * 3600.0d0
    end function elapsed_seconds_from_ref

    pure function parse_time_of_day_atm(dt_str) result(hr_frac)
        implicit none
        character(len=*), intent(in) :: dt_str
        real(real64) :: hr_frac

        integer(int32) :: hr, mn, sc, stat, t_pos

        hr_frac = 0.0d0
        t_pos   = index(dt_str, 'T')
        if (t_pos == 0) then
            ! Space-separated: "YYYY-MM-DD HH:MM:SS"
            if (len_trim(dt_str) >= 19) then
                read (dt_str(12:13), *, iostat=stat) hr
                read (dt_str(15:16), *, iostat=stat) mn
                read (dt_str(18:19), *, iostat=stat) sc
                hr_frac = real(hr, real64) + real(mn, real64)/60.0d0 + real(sc, real64)/3600.0d0
            end if
            return
        end if
        if (len_trim(dt_str) < t_pos + 7) return
        read (dt_str(t_pos + 1:t_pos + 2), *, iostat=stat) hr
        read (dt_str(t_pos + 4:t_pos + 5), *, iostat=stat) mn
        read (dt_str(t_pos + 7:t_pos + 8), *, iostat=stat) sc
        hr_frac = real(hr, real64) + real(mn, real64)/60.0d0 + real(sc, real64)/3600.0d0
    end function parse_time_of_day_atm

    pure function julian_day_atm(yr, mo, dy) result(jdn)
        implicit none
        integer(int32), intent(in) :: yr, mo, dy
        integer(int32) :: jdn, a, y, m

        a = (14 - mo) / 12
        y = yr + 4800 - a
        m = mo + 12*a - 3
        jdn = dy + (153*m + 2)/5 + 365*y + y/4 - y/100 + y/400 - 32045
    end function julian_day_atm

    pure function day_of_year_atm(yr, mo, dy) result(doy)
        implicit none
        integer(int32), intent(in) :: yr, mo, dy
        real(real64) :: doy

        doy = real(julian_day_atm(yr, mo, dy) - julian_day_atm(yr, 1, 1) + 1, real64)
    end function day_of_year_atm

    subroutine parse_csv_line_atm(line, dt_str, T_v, q_v, U_v, pr_v, stat)
        implicit none
        character(len=*), intent(in) :: line
        character(len=32), intent(inout) :: dt_str
        real(real64), intent(inout) :: T_v, q_v, U_v, pr_v
        integer(int32), intent(inout) :: stat

        integer(int32) :: c1, c2, c3, c4

        stat  = 1
        pr_v  = 0.0d0
        c1 = index(line, ',')
        if (c1 == 0) return
        c2 = c1 + index(line(c1 + 1:), ',')
        if (c2 == c1) return
        c3 = c2 + index(line(c2 + 1:), ',')
        if (c3 == c2) return
        c4 = c3 + index(line(c3 + 1:), ',')

        dt_str = line(1:c1 - 1)
        read (line(c1 + 1:c2 - 1), *, iostat=stat) T_v; if (stat /= 0) return
        read (line(c2 + 1:c3 - 1), *, iostat=stat) q_v; if (stat /= 0) return
        if (c4 /= c3) then
            ! 5-column format: valid_time,T,q,U,precip
            read (line(c3 + 1:c4 - 1), *, iostat=stat) U_v; if (stat /= 0) return
            read (line(c4 + 1:), *, iostat=stat) pr_v
        else
            ! 4-column fallback: valid_time,T,q,U
            read (line(c3 + 1:), *, iostat=stat) U_v
        end if
    end subroutine parse_csv_line_atm

end module physics_governing_atmosphere_observation
