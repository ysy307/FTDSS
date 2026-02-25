module core_types_config_control
    use :: core_types_config_control_acceleration, only: &
        type_config_acceleration
    use :: core_types_config_control_output_manager, only: &
        type_config_output_manager
    use :: core_types_config_control_time, only: &
        type_config_time, &
        type_config_time_ats
    use :: core_types_config_control_iteration, only: &
        type_config_iteration,&
        type_config_iteration_nonlinear,&
        type_config_iteration_criterion
    use :: core_types_config_control_parallel, only: &
        type_config_parallel_openmp
    implicit none
    private

    public :: type_config_acceleration
    public :: type_config_output_manager

    public :: type_config_time
    public :: type_config_time_ats

    public :: type_config_iteration
    public :: type_config_iteration_nonlinear
    public :: type_config_iteration_criterion

    public :: type_config_parallel_openmp

end module core_types_config_control
