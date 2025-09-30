module conditions_boundary
    use, intrinsic :: iso_fortran_env
    use :: stdlib_strings, only:to_string, strip
    use :: stdlib_logger
    use :: module_core
    use :: module_input
    use :: module_control
    implicit none
    private

    public :: abst_bc
    public :: type_bc_thermal_dirichlet
    public :: type_bc_thermal_adiabatic

    public :: construct_type_bc_thermal_dirichlet
    public :: construct_type_bc_thermal_adiabatic

    type, abstract :: abst_bc
        private
    end type abst_bc

    type, extends(abst_bc) :: type_bc_thermal_dirichlet
        real(real64), allocatable :: time_points(:)
        real(real64), allocatable :: values(:)
    end type type_bc_thermal_dirichlet

    interface
        module function construct_type_bc_thermal_dirichlet(cell_id, input, controls) result(structure)
            implicit none
            integer(int32), intent(in) :: cell_id
            type(type_input), intent(in) :: input
            type(type_controls), intent(in) :: controls
            class(abst_bc), allocatable :: structure
        end function construct_type_bc_thermal_dirichlet
    end interface

    type, extends(abst_bc) :: type_bc_thermal_adiabatic
    end type type_bc_thermal_adiabatic

    interface
        module function construct_type_bc_thermal_adiabatic(cell_id, input, controls) result(structure)
            implicit none
            integer(int32), intent(in) :: cell_id
            type(type_input), intent(in) :: input
            type(type_controls), intent(in) :: controls
            class(abst_bc), allocatable :: structure
        end function construct_type_bc_thermal_adiabatic
    end interface

end module conditions_boundary
