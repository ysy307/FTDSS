module core_types_config_bc
    use, intrinsic :: iso_fortran_env
    use :: core_memory, only:allocate_array, deallocate_array
    use :: core_constants, only:type_constant_id
    use :: core_types_config_base, only:abst_config
    implicit none
    private

    public :: type_config_bc

    type, extends(abst_config) :: type_config_bc
        !> 対象とする境界のID
        integer(int32) :: boundary_id = -1
        !> 対象とする現象の種類
        !> 熱移動，水分移動など
        type(type_constant_id) :: physics_type = type_constant_id("none", "none", -1)
        !> 境界条件の種類
        !> ディリクレ，ノイマンなど
        type(type_constant_id) :: bc_kind = type_constant_id("none", "none", -1)

        real(real64), allocatable :: time_points(:)
        real(real64), allocatable :: values(:, :) ! (成分, 時間)

        integer(int32) :: num_time_points = 0
        integer(int32) :: num_variables = 0
    contains

        procedure, public, pass(self) :: copy => copy_config_bc
        procedure, public, pass(self) :: reset => reset_config_bc

    end type type_config_bc

contains

    subroutine copy_config_bc(self, source)
        implicit none
        class(type_config_bc), intent(inout) :: self
        class(abst_config), intent(in) :: source

        select type (source)
        type is (type_config_bc)

            call self%set(self%boundary_id, source%boundary_id)
            call self%set(self%physics_type, source%physics_type)
            call self%set(self%bc_kind, source%bc_kind)
            

            call self%set(self%num_time_points, source%num_time_points)
            call self%set(self%num_variables, source%num_variables)

            call self%set(self%time_points, source%time_points)
            call self%set(self%values, source%values)
        class default
            call self%reset()
        end select
    end subroutine copy_config_bc

    subroutine reset_config_bc(self)
        implicit none
        class(type_config_bc), intent(inout) :: self

        self%boundary_id = -1

        if (allocated(self%time_points)) deallocate (self%time_points)
        if (allocated(self%values)) deallocate (self%values)

        self%num_time_points = 0
        self%num_variables = 0
    end subroutine reset_config_bc

end module core_types_config_bc
