submodule(io_input_conditions) input_conditions_base
    implicit none
contains
    module subroutine initialize_type_conditions(self)
        implicit none
        class(type_conditions), intent(inout), target :: self
        type(json_file) :: json

        integer(int32) :: i

        call json%initialize()
        call json%load(filename=self%file_name)
        call json%print_error_message(output_unit)

        call self%read_time_controls(json)
        call self%read_boundary_conditions(json)
        call self%read_initial_conditions(json)

        call json%destroy()
        call json%print_error_message(output_unit)

        self%time_control%parent => self
        do i = 1, self%num_boundaries
            self%boundary_conditions(i)%parent => self
        end do
        self%initial_conditions%parent => self

    end subroutine initialize_type_conditions

    module subroutine display_conditions(self)
        implicit none
        class(type_conditions), intent(in) :: self

        integer(int32) :: ierr, myrank
        integer(int32) :: i

        call MPI_Comm_rank(MPI_COMM_WORLD, myrank, ierr)
        if (myrank == 0) then
            write (*, '(A)') "=== Time Controls ==="
            call self%time_control%display()
            write (*, '(A)') "=== Boundary Conditions ==="
            do i = 1, self%num_boundaries
                call self%boundary_conditions(i)%display()
            end do
            write (*, '(A)') "=== Initial Conditions ==="
            call self%initial_conditions%display()
        end if
    end subroutine display_conditions

end submodule input_conditions_base
