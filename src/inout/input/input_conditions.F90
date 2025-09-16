! submodule(inout_input_conditions) inout_input_conditions_time_controls
!     implicit none
!
!
!     !!------------------------------------------------------------------------------------------------------------------------------
!     ! JSON key names for initial conditions
!     !!------------------------------------------------------------------------------------------------------------------------------
!     character(*), parameter :: initial_conditions = "initial_conditions"
!     character(*), parameter :: value = "value"
!     character(*), parameter :: valid_initial_condition_types(3) = ["uniform", "laplace", "file"]
!     character(*), parameter :: field_name = "field_name"
!     !!------------------------------------------------------------------------------------------------------------------------------

!     subroutine read_conditions_boundary_conditions(self, json)
!         implicit none
!         class(type_input) :: self
!         type(json_file), intent(inout) :: json !! JSON parser

!         character(:), allocatable :: key
!         character(:), allocatable :: key_base
!         logical :: found
!         integer(int32) :: i

!         call json%info(boundary_conditions, found=found, n_children=self%conditions%num_boundaries)
!         call json%print_error_message(output_unit)
!         if (.not. found .or. self%conditions%num_boundaries <= 0) then
!             call json%destroy()
!             call error_message(904, c_opt=boundary_conditions)
!         end if

!         if (allocated(self%conditions%boundary_conditions)) deallocate (self%conditions%boundary_conditions)
!         allocate (self%conditions%boundary_conditions(self%conditions%num_boundaries))

!         do i = 1, self%conditions%num_boundaries
!             key_base = boundary_conditions//"("//to_string(i)//")"
!             key = join([key_base, id])
!             call json%get(key, self%conditions%boundary_conditions(i)%id, found=found)
!             if (.not. found) then
!                 call json%destroy()
!                 call error_message(904, c_opt=key)
!             end if

!             if (self%basic%analysis_controls%calculate_thermal) then
!                 key = join([key_base, thermal])
!                 call read_conditions_boundary_conditions_thermal(self%conditions%boundary_conditions(i)%thermal, json, key, &
!                                                                  size(self%conditions%time_control%boundary_time_points(:)))
!             end if

!             if (self%basic%analysis_controls%calculate_hydraulic) then
!                 key = join([key_base, hydraulic])
!                 call read_conditions_boundary_conditions_hydraulic(self%conditions%boundary_conditions(i)%hydraulic, json, key, &
!                                                                    size(self%conditions%time_control%boundary_time_points(:)))
!             end if

!         end do

!     end subroutine read_conditions_boundary_conditions

!     subroutine read_conditions_boundary_conditions_thermal(boundary, json, key_base, num_time_points)
!         implicit none
!         class(type_boundary_local), intent(inout) :: boundary
!         type(json_file), intent(inout) :: json !! JSON parser
!         character(*), intent(in) :: key_base !! Base key for the boundary condition
!         integer(int32), intent(in), optional :: num_time_points !! Number of time points for the boundary condition

!         character(:), allocatable :: key
!         logical :: found

!         select type (bc => boundary)
!         class is (type_boundary_local)
!             ! Do nothing, bc is already of type type_boundary_local
!         class is (type_boundary_local_initial)
!             key = join([key_base, id])
!             call json%get(key, bc%id, found=found)
!             if (.not. found) then
!                 call json%destroy()
!                 call error_message(904, c_opt=key)
!             end if
!         end select

!         key = join([key_base, type])
!         call json%get(key, boundary%type, found=found)
!         if (.not. found) then
!             call json%destroy()
!             call error_message(904, c_opt=key)
!         else if (.not. any(valid_thermal_boundary_types(:) == boundary%type)) then
!             call json%destroy()
!             call error_message(905, c_opt=key)
!         end if

!         select case (boundary%type)
!         case (valid_thermal_boundary_types(1))
!             key = join([key_base, is_uniform])
!             call json%get(key, boundary%is_uniform, found=found)
!             if (.not. found) then
!                 call json%destroy()
!                 call error_message(904, c_opt=key)
!             end if

!             if (boundary%is_uniform) then
!                 key = join([key_base, values])

!                 call json%get(key, boundary%values, found=found)
!                 if (.not. found) then
!                     call json%destroy()
!                     call error_message(904, c_opt=key)
!                 else if (present(num_time_points)) then
!                     if (size(boundary%values(:)) /= num_time_points) then
!                         call json%destroy()
!                         call error_message(905, c_opt=key)
!                     end if
!                 end if
!             end if
!         end select

!     end subroutine read_conditions_boundary_conditions_thermal

!     subroutine read_conditions_boundary_conditions_hydraulic(boundary, json, key_base, num_time_points)
!         implicit none
!         class(type_boundary_local), intent(inout) :: boundary
!         type(json_file), intent(inout) :: json !! JSON parser
!         character(*), intent(in) :: key_base !! Base key for the boundary condition
!         integer(int32), intent(in), optional :: num_time_points !! Number of time points for the boundary condition

!         character(:), allocatable :: key
!         logical :: found

!         select type (bc => boundary)
!         class is (type_boundary_local)
!             ! Do nothing, bc is already of type type_boundary_local
!         class is (type_boundary_local_initial)
!             key = join([key_base, id])
!             call json%get(key, bc%id, found=found)
!             if (.not. found) then
!                 call json%destroy()
!                 call error_message(904, c_opt=key)
!             end if
!         end select

!         key = join([key_base, type])
!         call json%get(key, boundary%type, found=found)
!         if (.not. found) then
!             call json%destroy()
!             call error_message(904, c_opt=key)
!         else if (.not. any(valid_hydraulic_boundary_types(:) == boundary%type)) then
!             call json%destroy()
!             call error_message(905, c_opt=key)
!         end if

!         select case (boundary%type)
!         case (valid_hydraulic_boundary_types(1))
!             key = join([key_base, is_uniform])
!             call json%get(key, boundary%is_uniform, found=found)
!             if (.not. found) then
!                 call json%destroy()
!                 call error_message(904, c_opt=key)
!             end if

!             if (boundary%is_uniform) then
!                 key = join([key_base, values])
!                 call json%get(key, boundary%values, found=found)
!                 if (.not. found) then
!                     call json%destroy()
!                     call error_message(904, c_opt=key)
!                 else if (present(num_time_points)) then
!                     if (size(boundary%values(:)) /= num_time_points) then
!                         call json%destroy()
!                         call error_message(905, c_opt=key)
!                     end if
!                 end if
!             end if
!         end select

!     end subroutine read_conditions_boundary_conditions_hydraulic

!     subroutine read_conditions_initial_conditions(self, json)
!         implicit none
!         class(type_input) :: self
!         type(json_file), intent(inout) :: json !! JSON parser

!         character(:), allocatable :: key

!         if (self%basic%analysis_controls%calculate_thermal) then
!             key = join([initial_conditions, thermal])
!             call read_conditions_initial_conditions_thermal(self%conditions%initial_conditions%thermal, json, key, &
!                                                             self%conditions%num_boundaries)
!         end if

!         if (self%basic%analysis_controls%calculate_hydraulic) then
!             key = join([initial_conditions, hydraulic])
!             call read_conditions_initial_conditions_hydraulic(self%conditions%initial_conditions%hydraulic, json, key, &
!                                                               self%conditions%num_boundaries)
!         end if

!         key = join([initial_conditions, porosity])
!         call read_conditions_initial_conditions_porosity(self%conditions%initial_conditions%porosity, json, key, &
!                                                          self%conditions%num_boundaries)

!     end subroutine read_conditions_initial_conditions

!     subroutine read_conditions_initial_conditions_thermal(initial_condition, json, key_base, num_boundaries)
!         implicit none
!         type(type_initial_local), intent(inout) :: initial_condition
!         type(json_file), intent(inout) :: json !! JSON parser
!         character(*), intent(in) :: key_base !! Base key for the initial condition
!         integer(int32), intent(in), optional :: num_boundaries !! Number of boundaries for the initial condition

!         character(:), allocatable :: key
!         logical :: found
!         integer(int32) :: i

!         key = join([key_base, type])
!         call json%get(key, initial_condition%type, found=found)
!         if (.not. found) then
!             call json%destroy()
!             call error_message(904, c_opt=key)
!         end if

!         if (.not. any(valid_initial_condition_types(:) == initial_condition%type)) then
!             call json%destroy()
!             call error_message(905, c_opt=key)
!         end if

!         select case (initial_condition%type)
!         case (valid_initial_condition_types(1)) ! uniform
!             key = join([key_base, value])
!             call json%get(key, initial_condition%value, found=found)
!             if (.not. found) then
!                 call json%destroy()
!                 call error_message(904, c_opt=key)
!             end if
!         case (valid_initial_condition_types(2)) ! laplace
!             if (allocated(initial_condition%boundary)) deallocate (initial_condition%boundary)
!             allocate (initial_condition%boundary(num_boundaries))

!             do i = 1, num_boundaries
!                 key = join([key_base, boundary_conditions//"("//to_string(i)//")"])
!                 call read_conditions_boundary_conditions_thermal(initial_condition%boundary(i), json, key)
!             end do
!         case (valid_initial_condition_types(3)) ! file
!             key = join([key_base, field_name])
!             call json%get(key, initial_condition%field_name, found=found)
!             if (.not. found) then
!                 call json%destroy()
!                 call error_message(904, c_opt=key)
!             end if
!         end select

!     end subroutine read_conditions_initial_conditions_thermal

!     subroutine read_conditions_initial_conditions_porosity(initial_condition, json, key_base, num_boundaries)
!         implicit none
!         type(type_initial_local), intent(inout) :: initial_condition
!         type(json_file), intent(inout) :: json !! JSON parser
!         character(*), intent(in) :: key_base !! Base key for the initial condition
!         integer(int32), intent(in), optional :: num_boundaries !! Number of boundaries for the initial condition

!         character(:), allocatable :: key
!         logical :: found
!         integer(int32) :: i

!         key = join([key_base, type])
!         call json%get(key, initial_condition%type, found=found)
!         if (.not. found) then
!             call json%destroy()
!             call error_message(904, c_opt=key)
!         end if

!         if (.not. any(valid_initial_condition_types(:) == initial_condition%type)) then
!             call json%destroy()
!             call error_message(905, c_opt=key)
!         end if

!         select case (initial_condition%type)
!         case (valid_initial_condition_types(1)) ! uniform
!             key = join([key_base, value])
!             call json%get(key, initial_condition%value, found=found)
!             if (.not. found) then
!                 call json%destroy()
!                 call error_message(904, c_opt=key)
!             end if
!         case (valid_initial_condition_types(2)) ! laplace

!         case (valid_initial_condition_types(3)) ! file
!             key = join([key_base, field_name])
!             call json%get(key, initial_condition%field_name, found=found)
!             if (.not. found) then
!                 call json%destroy()
!                 call error_message(904, c_opt=key)
!             end if
!         end select

!     end subroutine read_conditions_initial_conditions_porosity

!     subroutine read_conditions_initial_conditions_hydraulic(initial_condition, json, key_base, num_boundaries)
!         implicit none
!         type(type_initial_local), intent(inout) :: initial_condition
!         type(json_file), intent(inout) :: json !! JSON parser
!         character(*), intent(in) :: key_base !! Base key for the initial condition
!         integer(int32), intent(in), optional :: num_boundaries !! Number of boundaries for the initial condition

!         character(:), allocatable :: key
!         logical :: found
!         integer(int32) :: i

!         key = join([key_base, type])
!         call json%get(key, initial_condition%type, found=found)
!         if (.not. found) then
!             call json%destroy()
!             call error_message(904, c_opt=key)
!         end if

!         if (.not. any(valid_initial_condition_types(:) == initial_condition%type)) then
!             call json%destroy()
!             call error_message(905, c_opt=key)
!         end if

!         select case (initial_condition%type)
!         case (valid_initial_condition_types(1)) ! uniform
!             key = join([key_base, value])
!             call json%get(key, initial_condition%value, found=found)
!             if (.not. found) then
!                 call json%destroy()
!                 call error_message(904, c_opt=key)
!             end if
!         case (valid_initial_condition_types(2)) ! laplace
!             if (allocated(initial_condition%boundary)) deallocate (initial_condition%boundary)
!             allocate (initial_condition%boundary(num_boundaries))

!             do i = 1, num_boundaries
!                 key = join([key_base, boundary_conditions//"("//to_string(i)//")"])
!                 call read_conditions_boundary_conditions_hydraulic(initial_condition%boundary(i), json, key)
!             end do
!         case (valid_initial_condition_types(3)) ! file
!             key = join([key_base, field_name])
!             call json%get(key, initial_condition%field_name, found=found)
!             if (.not. found) then
!                 call json%destroy()
!                 call error_message(904, c_opt=key)
!             end if
!         end select

!     end subroutine read_conditions_initial_conditions_hydraulic

! end submodule inout_input_conditions
