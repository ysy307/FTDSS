module types_config_control_output_manager
    use, intrinsic :: iso_fortran_env
    use :: core_constants, only:type_constant_id, type_constant_value
    use :: types_config_base, only:abst_config
    implicit none
    private

    public :: type_config_output_manager

    type, extends(abst_config) :: type_config_output_manager
        real(real64) :: interval_val
        type(type_constant_value) :: interval_unit
        type(type_constant_value) :: output_unit
        type(type_constant_id) :: file_format
    contains
        procedure, public, pass(self) :: copy => copy_config_output_manager
        procedure, public, pass(self) :: reset => reset_config_output_manager
    end type type_config_output_manager

contains

    subroutine copy_config_output_manager(self, source)
        implicit none
        class(type_config_output_manager), intent(inout) :: self
        class(abst_config), intent(in) :: source

        select type (source)
        type is (type_config_output_manager)
            call self%set(self%interval_val, source%interval_val)
            call self%set(self%interval_unit, source%interval_unit)
            call self%set(self%output_unit, source%output_unit)
            call self%set(self%file_format, source%file_format)
        class default
            call self%reset()
        end select
    end subroutine copy_config_output_manager

    subroutine reset_config_output_manager(self)
        implicit none
        class(type_config_output_manager), intent(inout) :: self

        self%interval_val = 0.0d0
        self%interval_unit = type_constant_value("", "", -1, "", 0)
        self%output_unit = type_constant_value("", "", -1, "", 0)
        self%file_format = type_constant_id("", "", -1)
    end subroutine reset_config_output_manager

end module types_config_control_output_manager
