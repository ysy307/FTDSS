module inout_input_translator
    use, intrinsic :: iso_fortran_env
    use :: module_core
    use :: inout_input, only:type_input
    implicit none
    private


    type :: type_input_translator
    contains
        procedure, private, pass(self) :: execute_condition_boundary
        generic :: execute => execute_condition_boundary
    end type type_input_translator

    type(type_input_translator), parameter, public :: input_translator = type_input_translator()

    interface
        module subroutine execute_condition_boundary(self, input, index, target_physics, config)
            implicit none
            class(type_input_translator), intent(in) :: self
            class(type_input), intent(in) :: input
            integer(int32), intent(in) :: index
            type(type_constant_id), intent(in) :: target_physics
            type(type_config_bc), intent(inout) :: config
        end subroutine execute_condition_boundary
    end interface

! contains

end module inout_input_translator
