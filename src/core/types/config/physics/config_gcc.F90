module types_config_physics_gcc
    use, intrinsic :: iso_fortran_env
    use :: core_constants, only:type_constant_id, ERROR_CODES
    use :: types_config_base, only:abst_config
    use :: types_config_physics_base, only:abst_config_physics_model
    implicit none
    private

    public :: type_config_gcc

    type, extends(abst_config_physics_model) :: type_config_gcc
        !> GCC model identification
        type(type_constant_id) :: gcc_model = type_constant_id("none", "none", -1)
    contains
        procedure, public, pass(self) :: copy => copy_config_gcc
        procedure, public, pass(self) :: reset => reset_config_gcc
    end type type_config_gcc

contains

    subroutine copy_config_gcc(self, source)
        implicit none
        class(type_config_gcc), intent(inout) :: self
        class(abst_config), intent(in) :: source

        call copy_config_physics_model(self, source)
        select type (source)
        type is (type_config_gcc)
            call self%set(self%gcc_model, source%gcc_model)
        end select
    end subroutine copy_config_gcc

    subroutine reset_config_gcc(self)
        implicit none
        class(type_config_gcc), intent(inout) :: self

        call reset_config_physics_model(self)
        self%gcc_model = type_constant_id("none", "none", -1)
    end subroutine reset_config_gcc

end module types_config_physics_gcc
