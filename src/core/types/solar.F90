module core_types_solar
    use iso_fortran_env, only: real64, int32
    use core_types_datetime, only: type_datetime
    implicit none
    private

    public :: type_solar_system

    ! 定数定義
    real(real64), parameter :: PI = 3.1415926535897932d0
    real(real64), parameter :: DEG_TO_RAD = PI / 180.0d0
    real(real64), parameter :: RAD_TO_DEG = 180.0d0 / PI

    ! ========================================================================
    ! 太陽計算クラス
    ! ========================================================================
    type :: type_solar_system
        private
        real(real64) :: lantitude ! 緯度 (度)
        real(real64) :: longitude ! 経度 (度)
        real(real64) :: altitude_m ! 標高 (m)
        real(real64) :: dip_deg ! 標高による伏角 (度)
    contains
        ! --- 公開メソッド (Public Interface) ---
        procedure, pass(self), public :: initialize => initialize_solar_system

        ! 位置計算 (Generic)
        procedure, pass(self), private :: get_pos_from_dt
        procedure, pass(self), private :: get_pos_from_vals
        generic, public :: get_position => get_pos_from_dt, get_pos_from_vals

        ! 日の出・日没・南中計算 (Generic)
        procedure, pass(self), private :: get_events_from_dt
        procedure, pass(self), private :: get_events_from_vals
        generic, public :: get_day_events => get_events_from_dt, get_events_from_vals

        ! --- 内部計算用メソッド (Private & NoPass) ---
        procedure, nopass, private :: calc_julian_day
        procedure, nopass, private :: calc_sun_coords
        procedure, nopass, private :: calc_gmst
    end type type_solar_system

contains

    ! ------------------------------------------------------------------------
    ! 初期化メソッド
    ! ------------------------------------------------------------------------
    subroutine initialize_solar_system(self, lantitude, longitude, altitude_m)
        implicit none
        class(type_solar_system), intent(inout) :: self
        real(real64), intent(in) :: lantitude
        real(real64), intent(in) :: longitude
        real(real64), intent(in) :: altitude_m

        self%lantitude = lantitude
        self%longitude = longitude
        self%altitude_m = altitude_m
        self%dip_deg = 0.0347d0 * sqrt(max(0.0d0, altitude_m))
    end subroutine initialize_solar_system

    ! ------------------------------------------------------------------------
    ! [Generic: get_position] Datetime
    ! ------------------------------------------------------------------------
    subroutine get_pos_from_dt(self, dt, elevation, azimuth)
        class(type_solar_system), intent(in) :: self
        class(type_datetime), intent(in) :: dt
        real(real64), intent(inout) :: elevation, azimuth

        integer(int32) :: y, m, d, h, min, sec, tz_min
        real(real64) :: tz_hr, time_frac

        call dt%get(y, m, d, h, min, sec, tz_min)
        tz_hr = real(tz_min, real64) / 60.0d0
        time_frac = (real(h, real64) + real(min, real64) / 60.0d0 + &
                     real(sec, real64) / 3600.0d0) - tz_hr
        time_frac = time_frac / 24.0d0

        call compute_pos_kernel(self, y, m, d, time_frac, elevation, azimuth)
    end subroutine get_pos_from_dt

    ! ------------------------------------------------------------------------
    ! [Generic: get_position] Values
    ! ------------------------------------------------------------------------
    subroutine get_pos_from_vals(self, year, month, day, hour, timezone, elevation, azimuth)
        class(type_solar_system), intent(in) :: self
        integer(int32), intent(in) :: year, month, day
        real(real64), intent(in) :: hour
        real(real64), intent(in) :: timezone
        real(real64), intent(inout) :: elevation, azimuth

        real(real64) :: time_frac
        time_frac = (hour - timezone) / 24.0d0
        call compute_pos_kernel(self, year, month, day, time_frac, elevation, azimuth)
    end subroutine get_pos_from_vals

    ! ------------------------------------------------------------------------
    ! 共通カーネル (位置計算)
    ! ------------------------------------------------------------------------
    subroutine compute_pos_kernel(self, y, m, d, utc_frac, el, az)
        class(type_solar_system), intent(in) :: self
        integer(int32), intent(in) :: y, m, d
        real(real64), intent(in) :: utc_frac
        real(real64), intent(inout) :: el, az

        real(real64) :: jd, t, dec, lha_rad, lantitude_rad, dec_rad, sin_el, cos_az
        real(real64) :: gmst, ra, ecl_longitudeg, obliq

        call self%calc_julian_day(y, m, d, jd)
        jd = jd + utc_frac
        t = (jd - 2451545.0d0) / 36525.0d0

        call self%calc_sun_coords(t, ecl_longitudeg, obliq, ra, dec)
        call self%calc_gmst(jd, gmst)

        lha_rad = (gmst + self%longitude - ra)
        lha_rad = mod(lha_rad, 360.0d0)
        if (lha_rad < 0.0d0) lha_rad = lha_rad + 360.0d0
        lha_rad = lha_rad * DEG_TO_RAD

        lantitude_rad = self%lantitude * DEG_TO_RAD
        dec_rad = dec * DEG_TO_RAD

        sin_el = sin(lantitude_rad) * sin(dec_rad) + cos(lantitude_rad) * cos(dec_rad) * cos(lha_rad)
        el = asin(sin_el) * RAD_TO_DEG

        cos_az = (sin(dec_rad) - sin(lantitude_rad) * sin_el) / (cos(el * DEG_TO_RAD) * cos(lantitude_rad))
        if (cos_az > 1.0d0) cos_az = 1.0d0
        if (cos_az < -1.0d0) cos_az = -1.0d0

        az = acos(cos_az) * RAD_TO_DEG
        if (sin(lha_rad) > 0.0d0) az = 360.0d0 - az
    end subroutine compute_pos_kernel

    ! ------------------------------------------------------------------------
    ! [Generic: get_day_events] Datetime引数版
    ! ------------------------------------------------------------------------
    subroutine get_events_from_dt(self, dt, t_rise, t_set, t_noon, alt_noon, status)
        class(type_solar_system), intent(in) :: self
        class(type_datetime), intent(in) :: dt
        real(real64), intent(inout) :: t_rise, t_set, t_noon, alt_noon
        integer(int32), intent(inout) :: status

        integer(int32) :: y, m, d, h, min, sec, tz_min
        real(real64) :: tz_hr

        ! datetimeから日付とタイムゾーンを取り出す
        call dt%get(y, m, d, h, min, sec, tz_min)
        tz_hr = real(tz_min, real64) / 60.0d0

        ! 数値版を呼び出す
        call self%get_day_events(y, m, d, tz_hr, t_rise, t_set, t_noon, alt_noon, status)
    end subroutine get_events_from_dt

    ! ------------------------------------------------------------------------
    ! [Generic: get_day_events] 数値引数版 (南中高度・時刻追加)
    ! ------------------------------------------------------------------------
    subroutine get_events_from_vals(self, year, month, day, timezone, t_rise, t_set, t_noon, alt_noon, status)
        class(type_solar_system), intent(in) :: self
        integer(int32), intent(in) :: year, month, day
        real(real64), intent(in) :: timezone
        real(real64), intent(inout) :: t_rise, t_set, t_noon, alt_noon
        integer(int32), intent(inout) :: status

        real(real64) :: jd, t, ecl, obl, ra, dec, gmst
        real(real64) :: eq_time, sun_noon_utc, ha_deg, cos_ha
        real(real64) :: zenith_rad, y_val, mean_l, mean_a
        real(real64) :: lantitude_rad, dec_rad, sin_alt_noon

        status = 0
        t_rise = 0.0d0
        t_set = 0.0d0
        t_noon = 0.0d0
        alt_noon = 0.0d0

        ! 正午基準で計算
        call self%calc_julian_day(year, month, day, jd)
        t = (jd - 2451545.0d0) / 36525.0d0

        call self%calc_sun_coords(t, ecl, obl, ra, dec)

        ! 均時差
        mean_l = mod(280.46646d0 + 36000.76983d0 * t, 360.0d0) * DEG_TO_RAD
        mean_a = mod(357.52911d0 + 35999.05029d0 * t, 360.0d0) * DEG_TO_RAD
        y_val = tan(obl * DEG_TO_RAD / 2.0d0)**2
        eq_time = 4.0d0 * RAD_TO_DEG * ( &
                  y_val * sin(2.0d0 * mean_l) - &
                  2.0d0 * 0.01671d0 * sin(mean_a) + &
                  4.0d0 * 0.01671d0 * y_val * sin(mean_a) * cos(2.0d0 * mean_l) - &
                  0.5d0 * y_val**2 * sin(4.0d0 * mean_l) - &
                  1.25d0 * 0.01671d0**2 * sin(2.0d0 * mean_a))

        ! --- 南中時刻 (Solar Noon) ---
        sun_noon_utc = (720.0d0 - 4.0d0 * self%longitude - eq_time) / 60.0d0
        t_noon = mod(sun_noon_utc + timezone, 24.0d0)
        if (t_noon < 0.0d0) t_noon = t_noon + 24.0d0

        ! --- 南中高度 (Solar Noon Altitude) ---
        ! 南中時は時角(Hour Angle) = 0
        lantitude_rad = self%lantitude * DEG_TO_RAD
        dec_rad = dec * DEG_TO_RAD
        ! sin(h) = sin(phi)sin(delta) + cos(phi)cos(delta)*cos(0)
        sin_alt_noon = sin(lantitude_rad) * sin(dec_rad) + cos(lantitude_rad) * cos(dec_rad)
        alt_noon = asin(sin_alt_noon) * RAD_TO_DEG

        ! --- 日の出・日没 ---
        zenith_rad = (90.8333d0 + self%dip_deg) * DEG_TO_RAD
        cos_ha = (cos(zenith_rad) - sin(lantitude_rad) * sin(dec_rad)) / &
                 (cos(lantitude_rad) * cos(dec_rad))

        if (cos_ha > 1.0d0) then
            status = -1 ! 極夜
            return
        else if (cos_ha < -1.0d0) then
            status = 1 ! 白夜
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
    ! 内部計算用 (NOPASS)
    ! ========================================================================

    subroutine calc_julian_day(y_in, m_in, d_in, jd)
        integer(int32), intent(in) :: y_in, m_in, d_in
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

    subroutine calc_sun_coords(t, ecl_longitudeg, obliq, ra, dec)
        real(real64), intent(in) :: t
        real(real64), intent(inout) :: ecl_longitudeg, obliq, ra, dec
        real(real64) :: mean_longitudeg, mean_anom

        mean_longitudeg = mod(280.46646d0 + 36000.76983d0 * t, 360.0d0)
        if (mean_longitudeg < 0.0d0) mean_longitudeg = mean_longitudeg + 360.0d0
        mean_anom = mod(357.52911d0 + 35999.05029d0 * t, 360.0d0)
        mean_anom = mean_anom * DEG_TO_RAD

        ecl_longitudeg = mean_longitudeg + 1.914602d0 * sin(mean_anom) + &
                         0.019993d0 * sin(2.0d0 * mean_anom)
        obliq = 23.439291d0 - 0.0130041d0 * t

        ra = atan2(cos(obliq * DEG_TO_RAD) * sin(ecl_longitudeg * DEG_TO_RAD), cos(ecl_longitudeg * DEG_TO_RAD)) * RAD_TO_DEG
        if (ra < 0.0d0) ra = ra + 360.0d0
        dec = asin(sin(obliq * DEG_TO_RAD) * sin(ecl_longitudeg * DEG_TO_RAD)) * RAD_TO_DEG
    end subroutine calc_sun_coords

    subroutine calc_gmst(jd, gmst)
        real(real64), intent(in) :: jd
        real(real64), intent(inout) :: gmst
        gmst = 280.46061837d0 + 360.98564736629d0 * (jd - 2451545.0d0)
        gmst = mod(gmst, 360.0d0)
        if (gmst < 0.0d0) gmst = gmst + 360.0d0
    end subroutine calc_gmst

end module core_types_solar
