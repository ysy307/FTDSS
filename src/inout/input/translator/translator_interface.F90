module inout_input_translator
    use, intrinsic :: iso_fortran_env
    use :: module_core
    use :: inout_input, only:type_input
    implicit none
    private

    type :: type_input_translator
    contains
        procedure, private, pass(self) :: execute_condition_boundary
        procedure, private, pass(self) :: execute_condition_initial
        procedure, private, pass(self) :: execute_condition_acceleration
        procedure, private, pass(self) :: execute_condition_time
        procedure, private, pass(self) :: execute_condition_time_ats
        procedure, private, pass(self) :: execute_basic_swcc
        procedure, private, pass(self) :: execute_basic_gcc
        procedure, private, pass(self) :: execute_output_field
        generic, public :: execute => execute_condition_boundary, &
            execute_condition_initial, &
            execute_condition_acceleration, &
            execute_condition_time, &
            execute_condition_time_ats, &
            execute_basic_swcc, &
            execute_basic_gcc, &
            execute_output_field
    end type type_input_translator

    type(type_input_translator), parameter, public :: input_translator = type_input_translator()

    interface
        module subroutine execute_condition_boundary(self, input, index, target_physics, config)
            implicit none
            class(type_input_translator), intent(in) :: self
            class(type_input), intent(in) :: input
            integer(int32), intent(in) :: index
            type(type_constant_id), intent(in) :: target_physics
            class(abst_config), intent(inout) :: config
        end subroutine execute_condition_boundary

        module subroutine execute_condition_initial(self, input, target_physics, config)
            implicit none
            class(type_input_translator), intent(in) :: self
            class(type_input), intent(in) :: input
            type(type_constant_id), intent(in) :: target_physics
            class(abst_config), intent(inout) :: config

        end subroutine execute_condition_initial

        module subroutine execute_condition_acceleration(self, input, config)
            implicit none
            class(type_input_translator), intent(in) :: self
            class(type_input), intent(in) :: input
            class(type_config_acceleration), intent(inout) :: config

        end subroutine execute_condition_acceleration

        module subroutine execute_condition_time(self, input, config)
            implicit none
            class(type_input_translator), intent(in) :: self
            class(type_input), intent(in) :: input
            class(type_config_time), intent(inout) :: config

        end subroutine execute_condition_time

        module subroutine execute_condition_time_ats(self, input, config)
            implicit none
            class(type_input_translator), intent(in) :: self
            class(type_input), intent(in) :: input
            class(type_config_time_ats), intent(inout) :: config

        end subroutine execute_condition_time_ats

        module subroutine execute_basic_swcc(self, input, material_id, config)
            implicit none
            class(type_input_translator), intent(in) :: self
            class(type_input), intent(in) :: input
            integer(int32), intent(in) :: material_id
            class(type_config_wrf), intent(inout) :: config
        end subroutine execute_basic_swcc

        module subroutine execute_basic_gcc(self, input, material_id, config)
            implicit none
            class(type_input_translator), intent(in) :: self
            class(type_input), intent(in) :: input
            integer(int32), intent(in) :: material_id
            class(type_config_gcc), intent(inout) :: config

        end subroutine execute_basic_gcc

        module subroutine execute_basic_iteration(self, input, config)
            implicit none
            class(type_input_translator), intent(in) :: self
            class(type_input), intent(in) :: input
            class(type_config_iteration), intent(inout) :: config

        end subroutine execute_basic_iteration

        module subroutine execute_output_field(self, input, config)
            implicit none
            class(type_input_translator), intent(in) :: self
            class(type_input), intent(in) :: input
            class(type_config_output_manager), intent(inout) :: config

        end subroutine execute_output_field
    end interface

! contains

end module inout_input_translator
