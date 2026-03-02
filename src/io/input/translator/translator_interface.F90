module io_input_translator
    use, intrinsic :: iso_fortran_env
    use :: module_core
    use :: io_input, only:type_input
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
        procedure, private, pass(self) :: execute_basic_iteration
        procedure, private, pass(self) :: execute_basic_parallel_openmp
        procedure, private, pass(self) :: execute_basic_control_manager
        procedure, private, pass(self) :: execute_geometry_domain_nodes
        procedure, private, pass(self) :: execute_geometry_domain_elements
        procedure, private, pass(self) :: execute_output_field
        generic, public :: execute => execute_condition_boundary, &
            execute_condition_initial, &
            execute_condition_acceleration, &
            execute_condition_time, &
            execute_condition_time_ats, &
            execute_basic_swcc, &
            execute_basic_gcc, &
            execute_basic_iteration, &
            execute_basic_parallel_openmp, &
            execute_basic_control_manager, &
            execute_geometry_domain_nodes, &
            execute_geometry_domain_elements, &
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

        module subroutine execute_basic_parallel_openmp(self, input, config)
            implicit none
            class(type_input_translator), intent(in) :: self
            class(type_input), intent(in) :: input
            class(type_config_parallel_openmp), intent(inout) :: config

        end subroutine execute_basic_parallel_openmp

        module subroutine execute_basic_control_manager(self, input, config)
            implicit none
            class(type_input_translator), intent(in) :: self
            class(type_input), intent(in) :: input
            class(type_config_control_manager), intent(inout) :: config

        end subroutine execute_basic_control_manager

        module subroutine execute_geometry_domain_nodes(self, input, config)
            implicit none
            class(type_input_translator), intent(in) :: self
            class(type_input), intent(in) :: input
            class(type_config_nodes), intent(inout) :: config

        end subroutine execute_geometry_domain_nodes

        module subroutine execute_geometry_domain_elements(self, input, config_elements, config_multicoloring)
            implicit none
            class(type_input_translator), intent(in) :: self
            class(type_input), intent(in) :: input
            class(type_config_elements), intent(inout) :: config_elements
            class(type_config_multicoloring), intent(inout) :: config_multicoloring

        end subroutine execute_geometry_domain_elements

        module subroutine execute_geometry_domain_boundaries(self, input, config_elements)
            implicit none
            class(type_input_translator), intent(in) :: self
            class(type_input), intent(in) :: input
            class(type_config_elements), intent(inout), allocatable :: config_elements(:)

        end subroutine execute_geometry_domain_boundaries

        module subroutine execute_output_field(self, input, config)
            implicit none
            class(type_input_translator), intent(in) :: self
            class(type_input), intent(in) :: input
            class(type_config_output_manager), intent(inout) :: config

        end subroutine execute_output_field
    end interface

! contains

end module io_input_translator
