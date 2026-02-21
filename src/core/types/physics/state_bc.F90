module core_types_physics_state_bc
    use, intrinsic :: iso_fortran_env
    use :: core_memory, only:allocate_array, deallocate_array
    use :: core_constants, only:type_constant_id
    implicit none
    private

    public :: type_state_bc

    type :: type_state_bc
        integer(int32) :: boundary_id = -1
        type(type_constant_id) :: physics_type
        type(type_constant_id) :: bc_kind

        real(real64), allocatable :: time_points(:)
        real(real64), allocatable :: values(:, :) ! (成分, 時間)

        integer(int32) :: num_time_points = 0
        integer(int32) :: num_variables = 0
    contains
        procedure, public, pass(self) :: set => set_state_bc
        procedure, public, pass(self) :: copy => copy_state_bc
        procedure, public, pass(self) :: reset => reset_state_bc

    end type type_state_bc

contains
    subroutine set_state_bc(self, boundary_id, physics_type, bc_kind, time_points, values)
        implicit none
        class(type_state_bc), intent(inout) :: self
        integer(int32), intent(in) :: boundary_id
        type(type_constant_id), intent(in) :: physics_type
        type(type_constant_id), intent(in) :: bc_kind
        real(real64), intent(in) :: time_points(:)
        real(real64), intent(in) :: values(:, :)

        self%boundary_id = boundary_id
        self%physics_type = physics_type
        self%bc_kind = bc_kind

        self%num_time_points = size(time_points)
        self%num_variables = size(values, 1)
        if (allocated(self%time_points)) deallocate (self%time_points)
        allocate (self%time_points(self%num_time_points))
        self%time_points = time_points
        if (allocated(self%values)) deallocate (self%values)
        allocate (self%values(self%num_variables, self%num_time_points))
        self%values = values
    end subroutine set_state_bc

    subroutine copy_state_bc(self, source)
        implicit none
        class(type_state_bc), intent(inout) :: self
        class(type_state_bc), intent(in) :: source

        self%boundary_id = source%boundary_id
        self%physics_type = source%physics_type
        self%bc_kind = source%bc_kind

        self%num_time_points = source%num_time_points
        self%num_variables = source%num_variables

        if (allocated(source%time_points)) then
            allocate (self%time_points(size(source%time_points)))
            self%time_points = source%time_points
        else
            if (allocated(self%time_points)) deallocate (self%time_points)
        end if

        if (allocated(source%values)) then
            allocate (self%values(size(source%values, 1), size(source%values, 2)))
            self%values = source%values
        else
            if (allocated(self%values)) deallocate (self%values)
        end if
    end subroutine copy_state_bc

    subroutine reset_state_bc(self)
        implicit none
        class(type_state_bc), intent(inout) :: self

        self%boundary_id = -1

        if (allocated(self%time_points)) deallocate (self%time_points)
        if (allocated(self%values)) deallocate (self%values)

        self%num_time_points = 0
        self%num_variables = 0
    end subroutine reset_state_bc

end module core_types_physics_state_bc
