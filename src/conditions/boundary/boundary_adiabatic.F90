submodule(conditions_boundary) conditions_boundary_adiabatic
    implicit none
contains

    module function construct_type_bc_thermal_adiabatic(cell_id, input, controls) result(structure)
        implicit none
        integer(int32), intent(in) :: cell_id
        type(type_input), intent(in) :: input
        type(type_controls), intent(in) :: controls
        class(abst_bc), allocatable :: structure

        if (allocated(structure)) deallocate (structure)
        allocate (type_bc_thermal_adiabatic :: structure)

    end function construct_type_bc_thermal_adiabatic

end submodule conditions_boundary_adiabatic
