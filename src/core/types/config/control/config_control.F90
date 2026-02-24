module core_types_config_control
    use :: core_types_config_control_acceleration, only: &
        type_config_acceleration
    use :: core_types_config_control_output_manager, only: &
        type_config_output_manager
    implicit none
    private

    public :: type_config_acceleration
    public :: type_config_output_manager

end module core_types_config_control
