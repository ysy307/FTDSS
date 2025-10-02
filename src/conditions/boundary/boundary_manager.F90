module conditions_boundary_manager
    use :: iso_fortran_env
    use :: module_core
    use :: module_input
    use :: module_control
    use :: conditions_boundary
    implicit none
    private

    public :: create_boundary_conditions

contains

    function create_boundary_conditions(target_bc_id, cell_id, input, controls) result(structure)
        implicit none
        integer(int32), intent(in) :: target_bc_id
        integer(int32), intent(in) :: cell_id
        type(type_input), intent(in) :: input
        type(type_controls), intent(in) :: controls
        class(abst_bc), allocatable :: structure

        select case (target_bc_id)
        case (THERMAL_BC_ADIABATIC)
            structure = construct_type_bc_thermal_adiabatic(cell_id, input, controls)
        case (THERMAL_BC_DIRICHLET)
            structure = construct_type_bc_thermal_dirichlet(cell_id, input, controls)
        case default
            write (*, *) "Error: Unknown boundary condition type ID: ", target_bc_id
            stop
        end select

    end function create_boundary_conditions
end module conditions_boundary_manager
