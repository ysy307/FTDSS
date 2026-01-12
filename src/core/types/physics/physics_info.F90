module core_types_physics_info
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: core_types_coordinate, only:type_coordinate_dp
    use :: core_deallocate, only:deallocate_array
    implicit none
    private

    public :: type_physics_info

    type :: type_physics_info
        integer(int32) :: num_phases = 0
        real(real64) :: solid = 0.0d0
        real(real64) :: water = 0.0d0
        real(real64) :: ice = 0.0d0
        real(real64) :: vapor = 0.0d0
        real(real64), allocatable :: dispersivity(:)
        real(real64), allocatable :: params(:)
    contains
        procedure, pass(self), public :: reset => reset_physics_info
    end type type_physics_info

contains
    subroutine reset_physics_info(self)
        implicit none
        class(type_physics_info), intent(inout) :: self

        self%num_phases = 0
        self%solid = 0.0d0
        self%water = 0.0d0
        self%ice = 0.0d0
        self%vapor = 0.0d0

        call deallocate_array(self%dispersivity)
        call deallocate_array(self%params)

    end subroutine reset_physics_info

end module core_types_physics_info
