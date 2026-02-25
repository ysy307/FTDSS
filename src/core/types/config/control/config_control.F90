module core_types_config_control
    use :: core_types_config_control_acceleration, only: &
        type_config_acceleration
    use :: core_types_config_control_output_manager, only: &
        type_config_output_manager
    use :: core_types_config_control_time, only: &
        type_config_time, &
        type_config_time_ats
    implicit none
    private

    public :: type_config_acceleration
    public :: type_config_output_manager

    public :: type_config_time
    public :: type_config_time_ats

end module core_types_config_control
