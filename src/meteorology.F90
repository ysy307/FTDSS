module core_types_physics_meteorology
    use, intrinsic :: iso_fortran_env
    use :: stdlib_optval, only:optval
    use :: core_constants, only:TIME_UNITS
    use :: core_memory, only:allocate_array, deallocate_array
    use :: core_types_datetime, only:type_datetime

    implicit none
    private

    public :: type_meteorology_data
    public :: type_meteorology

    !---------------------------------------------------------------------------
    !> Array of Structures (AoS) style
    !> Suitable for file I/O, single point access, and raw data storage.
    !---------------------------------------------------------------------------
    type :: type_meteorology_data
        !> Date and time of the data record
        type(type_datetime) :: datetime

        !> Duration of the measurement accumulation/averaging in seconds
        real(real64) :: duration_seconds = 3600.0d0

        !> State Variables
        real(real64) :: temperature = 0.0d0
        real(real64) :: humidity = 0.0d0
        real(real64) :: wind_speed = 0.0d0
        real(real64) :: wind_direction = 0.0d0
        real(real64) :: pressure_station = 1013.25d0
        real(real64) :: pressure_sea_level = 1013.25d0
        real(real64) :: sunshine_hours = 0.0d0
        real(real64) :: snow_depth = 0.0d0

        !> Flux Variables (Converted to Rate/Intensity)
        real(real64) :: precipitation_rate_mmh = 0.0d0
        real(real64) :: solar_irradiance_wm2 = 0.0d0
        real(real64) :: longwave_radiation_wm2 = 0.0d0
        real(real64) :: snowfall_rate_cmh = 0.0d0

    contains
        procedure, public, pass(self) :: initialize => initialize_meteorology_data
        procedure, public, pass(self) :: reset => reset_meteorology_data
    end type type_meteorology_data

    !---------------------------------------------------------------------------
    !> Structure of Arrays (SoA) style
    !> Suitable for vectorized computation and efficient memory access.
    !---------------------------------------------------------------------------
    type :: type_meteorology
        integer(int32) :: n_steps = 0

        type(type_datetime), allocatable :: datetime(:)
        real(real64), allocatable :: duration_seconds(:)

        !> State Variables (Arrays)
        real(real64), allocatable :: temperature(:)
        real(real64), allocatable :: humidity(:)
        real(real64), allocatable :: wind_speed(:)
        real(real64), allocatable :: wind_direction(:)
        real(real64), allocatable :: pressure_station(:)
        real(real64), allocatable :: pressure_sea_level(:)
        real(real64), allocatable :: sunshine_hours(:)
        real(real64), allocatable :: snow_depth(:)

        !> Flux Variables (Arrays)
        real(real64), allocatable :: precipitation_rate_mmh(:)
        real(real64), allocatable :: solar_irradiance_wm2(:)
        real(real64), allocatable :: longwave_radiation_wm2(:)
        real(real64), allocatable :: snowfall_rate_cmh(:)

    contains
        procedure, public, pass(self) :: initialize => initialize_type_meteorology
        procedure, public, pass(self) :: destroy => destroy_type_meteorology
        procedure, public, pass(self) :: import_meteorology_data
    end type type_meteorology

contains

    !---------------------------------------------------------------------------
    ! AoS Implementation
    !---------------------------------------------------------------------------
    subroutine initialize_meteorology_data(self, datetime_str, temperature, &
                                           precipitation_amount, sunshine, &
                                           wind_speed, wind_direction, humidity, &
                                           radiation_mj, &
                                           pressure_station, pressure_sea_level, &
                                           snow_depth, snowfall_amount, &
                                           longwave_radiation, duration)
        implicit none
        class(type_meteorology_data), intent(inout) :: self
        character(*), intent(in) :: datetime_str

        real(real64), intent(in) :: temperature, wind_speed, wind_direction, humidity, sunshine
        real(real64), intent(in) :: precipitation_amount, radiation_mj
        real(real64), intent(in), optional :: pressure_station, pressure_sea_level
        real(real64), intent(in), optional :: snow_depth, snowfall_amount
        real(real64), intent(in), optional :: longwave_radiation, duration

        real(real64) :: dt_sec
        real(real64) :: precip_mm, snow_cm
        real(real64) :: ONE_HOUR_SEC

        ONE_HOUR_SEC = TIME_UNITS%HOURS%value

        dt_sec = optval(duration, ONE_HOUR_SEC)
        if (dt_sec < epsilon(0.0d0)) dt_sec = ONE_HOUR_SEC
        self%duration_seconds = dt_sec

        call self%datetime%set(datetime_str)

        self%temperature = temperature
        self%wind_speed = wind_speed
        self%wind_direction = wind_direction
        self%humidity = humidity
        self%sunshine_hours = sunshine

        self%pressure_station = optval(pressure_station, 1013.25d0)
        self%pressure_sea_level = optval(pressure_sea_level, 1013.25d0)
        self%snow_depth = optval(snow_depth, 0.0d0)

        ! Convert accumulated values to rates
        precip_mm = precipitation_amount
        self%precipitation_rate_mmh = precip_mm * (ONE_HOUR_SEC / dt_sec)

        self%solar_irradiance_wm2 = (radiation_mj * 1.0d6) / dt_sec

        snow_cm = optval(snowfall_amount, 0.0d0)
        self%snowfall_rate_cmh = snow_cm * (ONE_HOUR_SEC / dt_sec)

        self%longwave_radiation_wm2 = optval(longwave_radiation, 0.0d0)

    end subroutine initialize_meteorology_data

    subroutine reset_meteorology_data(self)
        implicit none
        class(type_meteorology_data), intent(inout) :: self

        self%duration_seconds = TIME_UNITS%HOURS%value
        self%temperature = 0.0d0
        self%humidity = 0.0d0
        self%wind_speed = 0.0d0
        self%wind_direction = 0.0d0
        self%pressure_station = 1013.25d0
        self%pressure_sea_level = 1013.25d0
        self%sunshine_hours = 0.0d0
        self%snow_depth = 0.0d0
        self%precipitation_rate_mmh = 0.0d0
        self%solar_irradiance_wm2 = 0.0d0
        self%longwave_radiation_wm2 = 0.0d0
        self%snowfall_rate_cmh = 0.0d0
    end subroutine reset_meteorology_data

    !---------------------------------------------------------------------------
    ! SoA Implementation
    !---------------------------------------------------------------------------
    subroutine initialize_type_meteorology(self, num_steps, data)
        implicit none
        class(type_meteorology), intent(inout) :: self
        integer(int32), intent(in) :: num_steps
        type(type_meteorology_data), intent(in), optional :: data(:)

        if (self%n_steps > 0) call self%destroy()

        self%n_steps = num_steps

        if (.not. allocated(self%datetime)) allocate (self%datetime(num_steps))

        call allocate_array(self%duration_seconds, num_steps)
        call allocate_array(self%temperature, num_steps)
        call allocate_array(self%humidity, num_steps)
        call allocate_array(self%wind_speed, num_steps)
        call allocate_array(self%wind_direction, num_steps)
        call allocate_array(self%pressure_station, num_steps)
        call allocate_array(self%pressure_sea_level, num_steps)
        call allocate_array(self%sunshine_hours, num_steps)
        call allocate_array(self%snow_depth, num_steps)
        call allocate_array(self%precipitation_rate_mmh, num_steps)
        call allocate_array(self%solar_irradiance_wm2, num_steps)
        call allocate_array(self%longwave_radiation_wm2, num_steps)
        call allocate_array(self%snowfall_rate_cmh, num_steps)

        if (present(data)) then
            call self%import_meteorology_data(data)
        end if
    end subroutine initialize_type_meteorology

    subroutine destroy_type_meteorology(self)
        implicit none
        class(type_meteorology), intent(inout) :: self

        if (allocated(self%datetime)) deallocate (self%datetime)
        call deallocate_array(self%duration_seconds)
        call deallocate_array(self%temperature)
        call deallocate_array(self%humidity)
        call deallocate_array(self%wind_speed)
        call deallocate_array(self%wind_direction)
        call deallocate_array(self%pressure_station)
        call deallocate_array(self%pressure_sea_level)
        call deallocate_array(self%sunshine_hours)
        call deallocate_array(self%snow_depth)
        call deallocate_array(self%precipitation_rate_mmh)
        call deallocate_array(self%solar_irradiance_wm2)
        call deallocate_array(self%longwave_radiation_wm2)
        call deallocate_array(self%snowfall_rate_cmh)

        self%n_steps = 0
    end subroutine destroy_type_meteorology

    !> Import meteorology data from an AoS array (source) into this SoA (destination)
    subroutine import_meteorology_data(self, data)
        implicit none
        class(type_meteorology), intent(inout) :: self
        type(type_meteorology_data), intent(in) :: data(:)
        integer(int32) :: i, n

        n = size(data)

        ! Allocation is strictly forbidden here.
        ! Ensure the buffer is already allocated with correct size.
        if (self%n_steps /= n) then
            error stop "Error: Data size mismatch in import_data. Initialize with correct size first."
        end if

        !$omp parallel do private(i)
        do i = 1, n
            self%datetime(i) = data(i)%datetime
            self%duration_seconds(i) = data(i)%duration_seconds

            self%temperature(i) = data(i)%temperature
            self%humidity(i) = data(i)%humidity
            self%wind_speed(i) = data(i)%wind_speed
            self%wind_direction(i) = data(i)%wind_direction
            self%pressure_station(i) = data(i)%pressure_station
            self%pressure_sea_level(i) = data(i)%pressure_sea_level
            self%sunshine_hours(i) = data(i)%sunshine_hours
            self%snow_depth(i) = data(i)%snow_depth

            self%precipitation_rate_mmh(i) = data(i)%precipitation_rate_mmh
            self%solar_irradiance_wm2(i) = data(i)%solar_irradiance_wm2
            self%longwave_radiation_wm2(i) = data(i)%longwave_radiation_wm2
            self%snowfall_rate_cmh(i) = data(i)%snowfall_rate_cmh
        end do
        !$omp end parallel do

    end subroutine import_meteorology_data

end module core_types_physics_meteorology