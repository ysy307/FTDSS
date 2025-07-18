module control_iteration
    use, intrinsic :: iso_fortran_env, only: int32
    implicit none
    private

    public :: type_iteration

    type :: type_iteration
        integer(int32) :: iter
        integer(int32) :: max_iter

        logical :: isConverged
        integer(int32) :: step
    end type type_iteration

end module control_iteration
