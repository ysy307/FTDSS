module conditions_boundary_manager
    use, intrinsic :: iso_fortran_env, only: int32
    use :: stdlib_strings, only:to_string
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

            ! 将来的な拡張
            ! case (THERMAL_BC_NEUMANN)
            !     structure = construct_type_bc_thermal_neumann(cell_id, input, controls)

        case default
            call error_message(ERR_BC_UNKNOWN, &
                               c_opt="Unknown boundary condition type ID: "//trim(to_string(target_bc_id)))
        end select

        ! 共通プロパティの設定
        if (allocated(structure)) then
            structure%boundary_id = cell_id
        end if

    end function create_boundary_conditions

end module conditions_boundary_manager
