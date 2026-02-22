!> Module for calculating solar position, sunrise, and sunset.
!>
!> Calculations are based on NOAA approximation algorithms.
module core_types_solar
    use, intrinsic :: iso_fortran_env, only: real64, int32
    use core_types_datetime, only: type_datetime
    implicit none
    private

    public :: type_solar_system

    !> Pi
    real(real64), parameter :: PI = 3.1415926535897932d0
    !> Conversion factor from degrees to radians
    real(real64), parameter :: DEG_TO_RAD = PI / 180.0d0
    !> Conversion factor from radians to degrees
    real(real64), parameter :: RAD_TO_DEG = 180.0d0 / PI

    !> A class to manage location information for solar calculations.
    type :: type_solar_system
        private
        !> Latitude (degrees)
        real(real64) :: latitude = 0.0d0
        !> Longitude (degrees)
        real(real64) :: longitude = 0.0d0
        !> Altitude (meters)
        real(real64) :: altitude_m = 0.0d0
        !> Dip of horizon due to altitude (degrees)
        real(real64) :: dip_deg = 0.0d0
    contains
        ! --- Public Interfaces ---
        !> Initializes the solar system object.
        procedure, public, pass(self) :: initialize => initialize_solar_system

        ! --- Position Calculations (Generic) ---
        procedure, private, pass(self) :: get_pos_from_dt
        procedure, private, pass(self) :: get_pos_from_vals
        !> Calculates solar position (elevation and azimuth).
        generic, public :: get_position => get_pos_from_dt, get_pos_from_vals

        ! --- Day Events Calculations (Generic) ---
        procedure, private, pass(self) :: get_events_from_dt
        procedure, private, pass(self) :: get_events_from_vals
        !> Calculates sunrise, sunset, and solar noon events.
        generic, public :: get_day_events => get_events_from_dt, get_events_from_vals

        ! --- Internal Kernels (Private & NoPass) ---
        procedure, private, nopass :: calc_julian_day
        procedure, private, nopass :: calc_sun_coords
        procedure, private, nopass :: calc_gmst
    end type type_solar_system

contains

    !> Initializes the solar system with location data.
    subroutine initialize_solar_system(self, latitude, longitude, altitude_m)
        implicit none
        !> The type_solar_system instance
        class(type_solar_system), intent(inout) :: self
        !> Latitude in degrees
        real(real64), intent(in) :: latitude
        !> Longitude in degrees
        real(real64), intent(in) :: longitude
        !> Altitude in meters
        real(real64), intent(in) :: altitude_m

        self%latitude = latitude
        self%longitude = longitude
        self%altitude_m = altitude_m
        ! Approximation for dip of horizon: 0.0347 * sqrt(h)
        self%dip_deg = 0.0347d0 * sqrt(max(0.0d0, altitude_m))
    end subroutine initialize_solar_system

    ! ------------------------------------------------------------------------
    ! [Generic: get_position] From Datetime Object
    ! ------------------------------------------------------------------------
    subroutine get_pos_from_dt(self, dt, elevation, azimuth)
        implicit none
        !> The type_solar_system instance
        class(type_solar_system), intent(in) :: self
        !> Datetime object
        class(type_datetime), intent(in) :: dt
        !> Solar elevation (degrees)
        real(real64), intent(inout) :: elevation
        !> Solar azimuth (degrees)
        real(real64), intent(inout) :: azimuth

        integer(int32) :: y, m, d, h, min, sec, tz_min
        real(real64) :: tz_hr, time_frac

        call dt%get(y, m, d, h, min, sec, tz_min)

        tz_hr = real(tz_min, real64) / 60.0d0

        ! Calculate day fraction in UTC (0.0 - 1.0)
        time_frac = (real(h, real64) + real(min, real64) / 60.0d0 + &
                     real(sec, real64) / 3600.0d0) - tz_hr
        time_frac = time_frac / 24.0d0

        call compute_pos_kernel(self, y, m, d, time_frac, elevation, azimuth)
    end subroutine get_pos_from_dt

    ! ------------------------------------------------------------------------
    ! [Generic: get_position] From Raw Values
    ! ------------------------------------------------------------------------
    subroutine get_pos_from_vals(self, year, month, day, hour, timezone, elevation, azimuth)
        implicit none
        !> The type_solar_system instance
        class(type_solar_system), intent(in) :: self
        !> Year
        integer(int32), intent(in) :: year
        !> Month
        integer(int32), intent(in) :: month
        !> Day
        integer(int32), intent(in) :: day
        !> Local time (e.g., 12.5 for 12:30)
        real(real64), intent(in) :: hour
        !> Timezone offset (e.g., 9.0 for JST)
        real(real64), intent(in) :: timezone
        !> Solar elevation (degrees)
        real(real64), intent(inout) :: elevation
        !> Solar azimuth (degrees)
        real(real64), intent(inout) :: azimuth

        real(real64) :: time_frac

        ! Calculate day fraction in UTC
        time_frac = (hour - timezone) / 24.0d0

        call compute_pos_kernel(self, year, month, day, time_frac, elevation, azimuth)
    end subroutine get_pos_from_vals

    ! ------------------------------------------------------------------------
    ! Common Kernel for Position Calculation
    ! ------------------------------------------------------------------------
    subroutine compute_pos_kernel(self, y, m, d, utc_frac, el, az)
        implicit none
        !> The type_solar_system instance
        class(type_solar_system), intent(in) :: self
        !> Year
        integer(int32), intent(in) :: y
        !> Month
        integer(int32), intent(in) :: m
        !> Day
        integer(int32), intent(in) :: d
        !> UTC day fraction
        real(real64), intent(in) :: utc_frac
        !> Solar elevation (degrees)
        real(real64), intent(inout) :: el
        !> Solar azimuth (degrees)
        real(real64), intent(inout) :: az

        real(real64) :: jd, t, dec, lha_rad, latitude_rad, dec_rad, sin_el, cos_az
        real(real64) :: gmst, ra, ecl_long, obliq

        ! 1. Julian Day (Calling nopass procedure)
        call self%calc_julian_day(y, m, d, jd)
        jd = jd + utc_frac

        ! 2. Julian Century
        t = (jd - 2451545.0d0) / 36525.0d0

        ! 3. Solar Coordinates
        call self%calc_sun_coords(t, ecl_long, obliq, ra, dec)

        ! 4. Hour Angle Calculation
        call self%calc_gmst(jd, gmst)

        ! LHA = GMST + Longitude - RA
        lha_rad = (gmst + self%longitude - ra)
        lha_rad = mod(lha_rad, 360.0d0)
        if (lha_rad < 0.0d0) lha_rad = lha_rad + 360.0d0
        lha_rad = lha_rad * DEG_TO_RAD

        latitude_rad = self%latitude * DEG_TO_RAD
        dec_rad = dec * DEG_TO_RAD

        ! 5. Elevation (Altitude)
        sin_el = sin(latitude_rad) * sin(dec_rad) + cos(latitude_rad) * cos(dec_rad) * cos(lha_rad)
        el = asin(sin_el) * RAD_TO_DEG

        ! 6. Azimuth
        ! cos(az) = (sin(dec) - sin(el)sin(lat)) / (cos(el)cos(lat))
        cos_az = (sin(dec_rad) - sin(latitude_rad) * sin_el) / (cos(el * DEG_TO_RAD) * cos(latitude_rad))
        if (cos_az > 1.0d0) cos_az = 1.0d0
        if (cos_az < -1.0d0) cos_az = -1.0d0

        az = acos(cos_az) * RAD_TO_DEG
        ! If sin(LHA) > 0, azimuth is to the West (afternoon) -> 360 - az
        if (sin(lha_rad) > 0.0d0) az = 360.0d0 - az
    end subroutine compute_pos_kernel

    ! ------------------------------------------------------------------------
    ! [Generic: get_day_events] From Datetime Object
    ! ------------------------------------------------------------------------
    subroutine get_events_from_dt(self, dt, t_rise, t_set, t_noon, alt_noon, status)
        implicit none
        !> The type_solar_system instance
        class(type_solar_system), intent(in) :: self
        !> Datetime object
        class(type_datetime), intent(in) :: dt
        !> Sunrise time (hour)
        real(real64), intent(inout) :: t_rise
        !> Sunset time (hour)
        real(real64), intent(inout) :: t_set
        !> Solar noon time (hour)
        real(real64), intent(inout) :: t_noon
        !> Solar altitude at noon (degrees)
        real(real64), intent(inout) :: alt_noon
        !> Status (0: Normal, 1: Midnight Sun, -1: Polar Night)
        integer(int32), intent(inout) :: status

        integer(int32) :: y, m, d, h, min, sec, tz_min
        real(real64) :: tz_hr

        ! Extract components from datetime
        call dt%get(y, m, d, h, min, sec, tz_min)
        tz_hr = real(tz_min, real64) / 60.0d0

        ! Delegate to values implementation
        call self%get_day_events(y, m, d, tz_hr, t_rise, t_set, t_noon, alt_noon, status)
    end subroutine get_events_from_dt

    ! ------------------------------------------------------------------------
    ! [Generic: get_day_events] From Raw Values
    ! ------------------------------------------------------------------------
    subroutine get_events_from_vals(self, year, month, day, timezone, t_rise, t_set, t_noon, alt_noon, status)
        implicit none
        !> The type_solar_system instance
        class(type_solar_system), intent(in) :: self
        !> Year
        integer(int32), intent(in) :: year
        !> Month
        integer(int32), intent(in) :: month
        !> Day
        integer(int32), intent(in) :: day
        !> Timezone offset (e.g., 9.0)
        real(real64), intent(in) :: timezone
        !> Sunrise time (hour)
        real(real64), intent(inout) :: t_rise
        !> Sunset time (hour)
        real(real64), intent(inout) :: t_set
        !> Solar noon time (hour)
        real(real64), intent(inout) :: t_noon
        !> Solar altitude at noon (degrees)
        real(real64), intent(inout) :: alt_noon
        !> Status (0: Normal, 1: Midnight Sun, -1: Polar Night)
        integer(int32), intent(inout) :: status

        real(real64) :: jd, t, ecl, obl, ra, dec, gmst
        real(real64) :: eq_time, sun_noon_utc, ha_deg, cos_ha
        real(real64) :: zenith_rad, y_val, mean_l, mean_a
        real(real64) :: latitude_rad, dec_rad, sin_alt_noon

        status = 0
        t_rise = 0.0d0
        t_set = 0.0d0
        t_noon = 0.0d0
        alt_noon = 0.0d0

        ! Calculate based on Solar Noon
        call self%calc_julian_day(year, month, day, jd)
        t = (jd - 2451545.0d0) / 36525.0d0

        call self%calc_sun_coords(t, ecl, obl, ra, dec)

        ! Equation of Time
        mean_l = mod(280.46646d0 + 36000.76983d0 * t, 360.0d0) * DEG_TO_RAD
        mean_a = mod(357.52911d0 + 35999.05029d0 * t, 360.0d0) * DEG_TO_RAD
        y_val = tan(obl * DEG_TO_RAD / 2.0d0)**2

        eq_time = 4.0d0 * RAD_TO_DEG * ( &
                  y_val * sin(2.0d0 * mean_l) - &
                  2.0d0 * 0.01671d0 * sin(mean_a) + &
                  4.0d0 * 0.01671d0 * y_val * sin(mean_a) * cos(2.0d0 * mean_l) - &
                  0.5d0 * y_val**2 * sin(4.0d0 * mean_l) - &
                  1.25d0 * 0.01671d0**2 * sin(2.0d0 * mean_a))

        ! --- Solar Noon (UTC) ---
        sun_noon_utc = (720.0d0 - 4.0d0 * self%longitude - eq_time) / 60.0d0
        t_noon = mod(sun_noon_utc + timezone, 24.0d0)
        if (t_noon < 0.0d0) t_noon = t_noon + 24.0d0

        ! --- Solar Altitude at Noon ---
        ! Hour Angle is 0 at solar noon
        latitude_rad = self%latitude * DEG_TO_RAD
        dec_rad = dec * DEG_TO_RAD
        ! sin(h) = sin(phi)sin(delta) + cos(phi)cos(delta)*cos(0)
        sin_alt_noon = sin(latitude_rad) * sin(dec_rad) + cos(latitude_rad) * cos(dec_rad)
        alt_noon = asin(sin_alt_noon) * RAD_TO_DEG

        ! --- Sunrise / Sunset ---
        ! Zenith distance = 90 deg + radius + refraction + dip
        zenith_rad = (90.8333d0 + self%dip_deg) * DEG_TO_RAD

        cos_ha = (cos(zenith_rad) - sin(latitude_rad) * sin(dec_rad)) / &
                 (cos(latitude_rad) * cos(dec_rad))

        if (cos_ha > 1.0d0) then
            status = -1 ! Polar Night
            return
        else if (cos_ha < -1.0d0) then
            status = 1 ! Midnight Sun (White Night)
            return
        end if

        ha_deg = acos(cos_ha) * RAD_TO_DEG
        t_rise = sun_noon_utc - ha_deg / 15.0d0 + timezone
        t_set = sun_noon_utc + ha_deg / 15.0d0 + timezone

        t_rise = mod(t_rise, 24.0d0)
        if (t_rise < 0.0d0) t_rise = t_rise + 24.0d0
        t_set = mod(t_set, 24.0d0)
        if (t_set < 0.0d0) t_set = t_set + 24.0d0

    end subroutine get_events_from_vals

    ! ========================================================================
    ! Internal Calculation Kernels (NOPASS - Pure Calculations)
    ! ========================================================================

    !> Calculates the Julian Day.
    subroutine calc_julian_day(y_in, m_in, d_in, jd)
        implicit none
        !> Year
        integer(int32), intent(in) :: y_in
        !> Month
        integer(int32), intent(in) :: m_in
        !> Day
        integer(int32), intent(in) :: d_in
        !> Julian Day
        real(real64), intent(inout) :: jd

        integer(int32) :: y, m, a, b
        y = y_in; m = m_in
        if (m <= 2) then
            y = y - 1; m = m + 12
        end if
        a = floor(real(y, real64) / 100.0d0)
        b = 2 - a + floor(real(a, real64) / 4.0d0)
        jd = floor(365.25d0 * (y + 4716)) + floor(30.6001d0 * (m + 1)) + d_in + b - 1524.5d0
    end subroutine calc_julian_day

    !> Calculates solar coordinates (Ecliptic Longitude, Obliquity, RA, Dec).
    subroutine calc_sun_coords(t, ecl_long, obliq, ra, dec)
        implicit none
        !> Julian Century
        real(real64), intent(in) :: t
        !> Ecliptic longitude
        real(real64), intent(inout) :: ecl_long
        !> Obliquity
        real(real64), intent(inout) :: obliq
        !> Right Ascension
        real(real64), intent(inout) :: ra
        !> Declination
        real(real64), intent(inout) :: dec

        real(real64) :: mean_long, mean_anom

        mean_long = mod(280.46646d0 + 36000.76983d0 * t, 360.0d0)
        if (mean_long < 0.0d0) mean_long = mean_long + 360.0d0
        mean_anom = mod(357.52911d0 + 35999.05029d0 * t, 360.0d0)
        mean_anom = mean_anom * DEG_TO_RAD

        ecl_long = mean_long + 1.914602d0 * sin(mean_anom) + &
                   0.019993d0 * sin(2.0d0 * mean_anom)
        obliq = 23.439291d0 - 0.0130041d0 * t

        ra = atan2(cos(obliq * DEG_TO_RAD) * sin(ecl_long * DEG_TO_RAD), cos(ecl_long * DEG_TO_RAD)) * RAD_TO_DEG
        if (ra < 0.0d0) ra = ra + 360.0d0
        dec = asin(sin(obliq * DEG_TO_RAD) * sin(ecl_long * DEG_TO_RAD)) * RAD_TO_DEG
    end subroutine calc_sun_coords

    !> Calculates Greenwich Mean Sidereal Time (GMST).
    subroutine calc_gmst(jd, gmst)
        implicit none
        !> Julian Day
        real(real64), intent(in) :: jd
        !> GMST
        real(real64), intent(inout) :: gmst

        gmst = 280.46061837d0 + 360.98564736629d0 * (jd - 2451545.0d0)
        gmst = mod(gmst, 360.0d0)
        if (gmst < 0.0d0) gmst = gmst + 360.0d0
    end subroutine calc_gmst

end module core_types_solar
