module types_config_control_acceleration
    use, intrinsic :: iso_fortran_env
    use :: core_constants, only:type_constant_id
    use :: core_types_config_base, only:abst_config
    implicit none
    private

    public :: type_config_acceleration

    type, extends(abst_config) :: type_config_acceleration
        !> Acceleration method type
        type(type_constant_id) :: method = type_constant_id("", "", -1)
        real(real64) :: max_relaxation = 1.0d0
        real(real64) :: min_relaxation = 0.05d0
        integer(int32) :: num_dofs = -1
    contains
        procedure, public, pass(self) :: copy => copy_config_acceleration
        procedure, public, pass(self) :: reset => reset_config_acceleration
    end type type_config_acceleration

contains

    subroutine copy_config_acceleration(self, source)
        implicit none
        class(type_config_acceleration), intent(inout) :: self
        class(abst_config), intent(in) :: source

        select type (source)
        type is (type_config_acceleration)
            call self%set(self%method, source%method)
            call self%set(self%max_relaxation, source%max_relaxation)
            call self%set(self%min_relaxation, source%min_relaxation)
            call self%set(self%num_dofs, source%num_dofs)
        class default
            call self%reset()
        end select
    end subroutine copy_config_acceleration

    subroutine reset_config_acceleration(self)
        implicit none
        class(type_config_acceleration), intent(inout) :: self

        self%method = type_constant_id("", "", -1)
        self%max_relaxation = 1.0d0
        self%min_relaxation = 0.05d0
        self%num_dofs = -1
    end subroutine reset_config_acceleration

end module types_config_control_acceleration
