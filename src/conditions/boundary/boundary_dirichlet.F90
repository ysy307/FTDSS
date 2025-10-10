submodule(conditions_boundary) conditions_boundary_dirichlet
    implicit none
contains

    module function construct_type_bc_thermal_dirichlet(cell_id, input, controls) result(structure)
        implicit none
        integer(int32), intent(in) :: cell_id
        type(type_input), intent(in) :: input
        type(type_controls), intent(in) :: controls
        class(abst_bc), allocatable :: structure

        integer(int32) :: i
        real(real64) :: time_conv

        if (allocated(structure)) deallocate (structure)
        allocate (type_bc_thermal_dirichlet :: structure)

        select type (this => structure)
        type is (type_bc_thermal_dirichlet)

            call deallocate_array(this%time_points)
            allocate (this%time_points, source=input%conditions%time_control%boundary_time_points)
            time_conv = controls%time%convert_time_unit(input%conditions%time_control%simulation_period%unit, &
                                                        TIME_UNIT_SECONDS)
            this%time_points = this%time_points * time_conv

            call deallocate_array(this%values)
            do i = 1, input%conditions%num_boundaries
                if (input%conditions%boundary_conditions(i)%id == cell_id) then
                    allocate (this%values, source=input%conditions%boundary_conditions(i)%physics(PHYSICS_TYPE_THERMAL)%values)
                    exit
                end if
            end do

        end select
    end function construct_type_bc_thermal_dirichlet

end submodule conditions_boundary_dirichlet
