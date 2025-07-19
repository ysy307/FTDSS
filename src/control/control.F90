module module_control
    use :: control_time, only:type_time
    use :: control_iteration, only:type_iteration
    use :: control_openmp
    implicit none
    private

    public :: type_time
    public :: type_iteration

    public :: initialize_openmp

end module module_control

