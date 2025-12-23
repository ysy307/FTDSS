submodule(conditions_boundary) conditions_boundary_dirichlet
    use :: stdlib_strings, only:to_string
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
        logical :: found

        if (allocated(structure)) deallocate (structure)
        allocate (type_bc_thermal_dirichlet :: structure)

        select type (this => structure)
        type is (type_bc_thermal_dirichlet)

            ! --- 時間配列の割り当て ---
            if (allocated(this%time_points)) deallocate (this%time_points)
            if (allocated(input%conditions%time_control%boundary_time_points)) then
                allocate (this%time_points, source=input%conditions%time_control%boundary_time_points)
                time_conv = controls%time%convert_time_unit( &
                            input%conditions%time_control%simulation_period%unit, &
                            TIME_UNIT_SECONDS)
                this%time_points = this%time_points * time_conv
            else
                allocate (this%time_points(1))
                this%time_points(1) = 0.0d0
            end if

            ! --- 値配列の割り当て ---
            found = .false.
            if (allocated(this%values)) deallocate (this%values)

            do i = 1, input%conditions%num_boundaries
                if (input%conditions%boundary_conditions(i)%id == cell_id) then
                    if (allocated(input%conditions%boundary_conditions(i)%physics(PHYSICS_TYPE_THERMAL)%values)) then
                        allocate (this%values, &
                                  source=input%conditions%boundary_conditions(i)%physics(PHYSICS_TYPE_THERMAL)%values)
                        found = .true.
                    end if
                    exit
                end if
            end do

            if (.not. found) then
                call error_message(ERR_BC_INIT, &
                                   c_opt="Dirichlet data not found for BC ID: "//trim(to_string(cell_id)))
            end if

            if (size(this%time_points) /= size(this%values)) then
                call error_message(ERR_BC_INIT, &
                                   c_opt="Time/Value size mismatch in Dirichlet BC ID: "//trim(to_string(cell_id)))
            end if

        end select
    end function construct_type_bc_thermal_dirichlet

end submodule conditions_boundary_dirichlet
