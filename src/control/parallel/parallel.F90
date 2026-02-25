module control_parallel
    use :: control_parallel_openmp, only: &
        initialize_openmp
    implicit none
    private

    public :: initialize_openmp

end module control_parallel
