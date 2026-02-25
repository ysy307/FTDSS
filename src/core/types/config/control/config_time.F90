module core_types_config_control_time
    use, intrinsic :: iso_fortran_env
    use :: core_constants, only:type_constant_value
    use :: core_types_config_base, only:abst_config
    implicit none
    private

    public :: type_config_time
    public :: type_config_time_ats

    type, extends(abst_config) :: type_config_time
        integer(int32) :: target_bdf_order = 1
        real(real64) :: initial_step = 0.0d0
        real(real64) :: start_time = 0.0d0
        real(real64) :: end_time = 0.0d0
        type(type_constant_value) :: time_stepping_unit = type_constant_value("", "", -1, "", 0)
        type(type_constant_value) :: simulation_period_unit = type_constant_value("", "", -1, "", 0)
    contains
        procedure, public, pass(self) :: copy => copy_config_time
        procedure, public, pass(self) :: reset => reset_config_time
    end type type_config_time

    type, extends(abst_config) :: type_config_time_ats
        logical :: active = .false.

        !> [Config] Thresholds for iteration counts
        integer(int32) :: iter_min = 4 ! If iter < min, increase dt
        integer(int32) :: iter_max = 8 ! If iter > max, decrease dt

        !> [Config] Scaling factors
        real(real64) :: scale_up = 1.2d0 ! Success (easy)
        real(real64) :: scale_down = 0.8d0 ! Success (hard)
        real(real64) :: scale_retry = 0.5d0 ! Failure (diverged)

        real(real64) :: safety_factor = 0.9d0
        real(real64) :: max_growth_rate = 2.0d0

        !> [Config] Absolute limits (Copies from time control for easy access) in Seconds
        real(real64) :: dt_min = 1.0d-5
        real(real64) :: dt_max = 1.0d+2
    contains
        procedure, public, pass(self) :: copy => copy_config_time_ats
        procedure, public, pass(self) :: reset => reset_config_time_ats
    end type type_config_time_ats

contains

    subroutine copy_config_time(self, source)
        implicit none
        class(type_config_time), intent(inout) :: self
        class(abst_config), intent(in) :: source

        call self%reset()

        select type (source)
        type is (type_config_time)

            call self%set(self%target_bdf_order, source%target_bdf_order)
            call self%set(self%initial_step, source%initial_step)
            call self%set(self%start_time, source%start_time)
            call self%set(self%end_time, source%end_time)
            call self%set(self%time_stepping_unit, source%time_stepping_unit)
            call self%set(self%simulation_period_unit, source%simulation_period_unit)
        class default
            call self%reset()
        end select
    end subroutine copy_config_time

    subroutine reset_config_time(self)
        implicit none
        class(type_config_time), intent(inout) :: self

        self%target_bdf_order = 1
        self%initial_step = 0.0d0
        self%start_time = 0.0d0
        self%end_time = 0.0d0
        self%time_stepping_unit = type_constant_value("", "", -1, "", 0)
        self%simulation_period_unit = type_constant_value("", "", -1, "", 0)
    end subroutine reset_config_time

    subroutine copy_config_time_ats(self, source)
        implicit none
        class(type_config_time_ats), intent(inout) :: self
        class(abst_config), intent(in) :: source

        call self%reset()

        select type (source)
        type is (type_config_time_ats)
            call self%set(self%active, source%active)
            call self%set(self%iter_min, source%iter_min)
            call self%set(self%iter_max, source%iter_max)
            call self%set(self%scale_up, source%scale_up)
            call self%set(self%scale_down, source%scale_down)
            call self%set(self%scale_retry, source%scale_retry)
            call self%set(self%safety_factor, source%safety_factor)
            call self%set(self%max_growth_rate, source%max_growth_rate)
            call self%set(self%dt_min, source%dt_min)
            call self%set(self%dt_max, source%dt_max)
        class default
            call self%reset()
        end select
    end subroutine copy_config_time_ats

    subroutine reset_config_time_ats(self)
        implicit none
        class(type_config_time_ats), intent(inout) :: self

        self%active = .false.
        self%iter_min = 4
        self%iter_max = 8
        self%scale_up = 1.2d0
        self%scale_down = 0.8d0
        self%scale_retry = 0.5d0
        self%safety_factor = 0.9d0
        self%max_growth_rate = 2.0d0
        self%dt_min = 1.0d-5
        self%dt_max = 1.0d+2
    end subroutine reset_config_time_ats

end module core_types_config_control_time
