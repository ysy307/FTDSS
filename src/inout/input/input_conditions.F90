submodule(inout_input) inout_input_conditions
    implicit none
    !-------------------------------------------------------------------------------
    ! JSON key names for time control
    !-------------------------------------------------------------------------------
    character(*), parameter :: time_control = "time_control"
    character(*), parameter :: simulation_period = "simulation_period"
    character(*), parameter :: unit = "unit"
    character(*), parameter :: time_units(5) = ["second", "minute", "hour", "day", "year"]
    character(*), parameter :: start = "start"
    character(*), parameter :: end = "end"
    character(*), parameter :: time_stepping = "time_stepping"
    character(*), parameter :: initial_step = "initial_step"
    character(*), parameter :: min_step = "min_step"
    character(*), parameter :: max_step = "max_step"
    character(*), parameter :: boundary_condition_time_points = "boundary_condition_time_points"

contains
    module subroutine inout_read_conditions(self)
        !> Load the boundary/initial conditions from the JSON file
        implicit none
        class(type_input) :: self

        type(json_file) :: json
        character(:), allocatable :: key
        integer(int32) :: iRegion

        call json%initialize()
        call json%load(filename=self%conditions_file_name)
        call json%print_error_message(output_unit)

        call read_conditions_time_control(self, json)

        ! call inout_read_conditions_BC(self, json)
        ! call inout_read_conditions_IC(self, json)

        call json%destroy()
        call json%print_error_message(output_unit)

    end subroutine inout_read_conditions

    subroutine read_conditions_time_control(self, json)
        !> Load the time control parameters from the JSON file
        implicit none
        class(type_input) :: self
        type(json_file), intent(inout) :: json !! JSON parser

        character(:), allocatable :: key
        logical :: found

        call read_conditions_time_control_simulation_period(self, json)
        call read_conditions_time_control_time_stepping(self, json)
        call read_conditions_time_control_boundary_condition_time_points(self, json)

    end subroutine read_conditions_time_control

    subroutine read_conditions_time_control_simulation_period(self, json)
        !> Load the time control parameters from the JSON file
        implicit none
        class(type_input) :: self
        type(json_file), intent(inout) :: json !! JSON parser

        character(:), allocatable :: key
        logical :: found

        key = join([time_control, simulation_period, unit])
        call json%get(key, self%conditions%time_control%simulation_period%unit, found=found)
        if (.not. found) then
            call json%destroy()
            call error_message(904, c_opt=key)
        end if
        if (.not. any(time_units(:) == self%conditions%time_control%simulation_period%unit)) then
            call json%destroy()
            call error_message(905, c_opt=key)
        end if

        key = join([time_control, simulation_period, start])
        call json%get(key, self%conditions%time_control%simulation_period%start, found=found)
        if (.not. found) then
            call json%destroy()
            call error_message(904, c_opt=key)
        end if

        key = join([time_control, simulation_period, end])
        call json%get(key, self%conditions%time_control%simulation_period%end, found=found)
        if (.not. found) then
            call json%destroy()
            call error_message(904, c_opt=key)
        end if

        if (self%conditions%time_control%simulation_period%start >= self%conditions%time_control%simulation_period%end) then
            call json%destroy()
            call error_message(905, c_opt=key)
        end if

    end subroutine read_conditions_time_control_simulation_period

    subroutine read_conditions_time_control_time_stepping(self, json)
        !> Load the time stepping parameters from the JSON file
        implicit none
        class(type_input) :: self
        type(json_file), intent(inout) :: json !! JSON parser

        character(:), allocatable :: key
        logical :: found

        key = join([time_control, time_stepping, unit])
        call json%get(key, self%conditions%time_control%time_stepping%unit, found=found)
        if (.not. found) then
            call json%destroy()
            call error_message(904, c_opt=key)
        end if
        if (.not. any(time_units(:) == self%conditions%time_control%time_stepping%unit)) then
            call json%destroy()
            call error_message(905, c_opt=key)
        end if

        key = join([time_control, time_stepping, initial_step])
        call json%get(key, self%conditions%time_control%time_stepping%initial_step, found=found)
        if (.not. found) then
            call json%destroy()
            call error_message(904, c_opt=key)
        end if
        if (self%conditions%time_control%time_stepping%initial_step <= 0.0) then
            call json%destroy()
            call error_message(905, c_opt=key)
        end if

        key = join([time_control, time_stepping, min_step])
        call json%get(key, self%conditions%time_control%time_stepping%min_step, found=found)
        if (.not. found) then
            call json%destroy()
            call error_message(904, c_opt=key)
        end if
        if (self%conditions%time_control%time_stepping%min_step <= 0.0) then
            call json%destroy()
            call error_message(905, c_opt=key)
        end if

        key = join([time_control, time_stepping, max_step])
        call json%get(key, self%conditions%time_control%time_stepping%max_step, found=found)
        if (.not. found) then
            call json%destroy()
            call error_message(904, c_opt=key)
        end if
        if (self%conditions%time_control%time_stepping%max_step <= 0.0) then
            call json%destroy()
            call error_message(905, c_opt=key)
        end if

        if (self%conditions%time_control%time_stepping%min_step > self%conditions%time_control%time_stepping%max_step) then
            call json%destroy()
            call error_message(905, c_opt=key)
        end if

        if (self%conditions%time_control%time_stepping%initial_step < self%conditions%time_control%time_stepping%min_step .or. &
            self%conditions%time_control%time_stepping%initial_step > self%conditions%time_control%time_stepping%max_step) then
            call json%destroy()
            call error_message(905, c_opt=key)
        end if

    end subroutine read_conditions_time_control_time_stepping

    subroutine read_conditions_time_control_boundary_condition_time_points(self, json)
        !> Load the boundary condition time points from the JSON file
        implicit none
        class(type_input) :: self
        type(json_file), intent(inout) :: json !! JSON parser

        character(:), allocatable :: key
        logical :: found
        integer(int32) :: i
        real(real64), parameter :: machine_epsilon = 1.0d-9

        key = join([time_control, boundary_condition_time_points])
        call json%get(key, self%conditions%time_control%boundary_time_points, found=found)
        if (.not. found) then
            call json%destroy()
            call error_message(904, c_opt=key)
        end if

        ! --- Check if the time points array is sorted ---
        if (size(self%conditions%time_control%boundary_time_points) > 1) then
            do i = 1, size(self%conditions%time_control%boundary_time_points) - 1
                if (self%conditions%time_control%boundary_time_points(i) >= &
                    self%conditions%time_control%boundary_time_points(i + 1)) then
                    ! The array is not sorted in strictly ascending order.
                    call json%destroy()
                    call error_message(905, c_opt=key)
                end if
            end do
        end if

        if (size(self%conditions%time_control%boundary_time_points) < 2) then
            ! At least two time points are required.
            call json%destroy()
            call error_message(905, c_opt=key)
        end if

        ! --- Check if the time points are within the simulation period ---
        if (size(self%conditions%time_control%boundary_time_points) > 0) then
            ! 開始時刻のチェック
            if (abs(self%conditions%time_control%boundary_time_points(1) - &
                    self%conditions%time_control%simulation_period%start) > machine_epsilon) then
                call json%destroy()
                call error_message(905, c_opt=key)
            end if

            ! 終了時刻のチェック
            if (abs(self%conditions%time_control%boundary_time_points(size(self%conditions%time_control%boundary_time_points)) - &
                    self%conditions%time_control%simulation_period%end) > machine_epsilon) then
                call json%destroy()
                call error_message(905, c_opt=key)
            end if
        end if

    end subroutine read_conditions_time_control_boundary_condition_time_points

    ! subroutine inout_input_conditions_JSON_BC(self, json)
    !     !> Load the boundary conditions from the JSON file
    !     implicit none
    !     class(type_input) :: self
    !     type(json_file), intent(inout) :: json !! JSON parser

    !     character(:), allocatable :: key
    !     character(2) :: cBCGroup
    !     integer(int32) :: iBC
    !     integer(int32) :: minium, maximum

    !     integer(int32) :: numGroup
    !     integer(int32) :: iGroup

    !     ! key = Connect_dot(BCName, GroupName)
    !     ! call json%get(key, self%conditions%Groups)
    !     ! call json%print_error_message(output_unit)

    !     ! numGroup = size(self%conditions%Groups)
    !     ! minium = minval(self%conditions%Groups)
    !     ! maximum = maxval(self%conditions%Groups)
    !     ! allocate (self%conditions%Heat(minium:maximum))
    !     ! allocate (self%conditions%Water(minium:maximum))

    !     ! key = Connect_dot(BCName, TimeName)
    !     ! call json%get(key, self%conditions%Time)
    !     ! call json%print_error_message(output_unit)

    !     ! do iBC = 1, numGroup
    !     !     iGroup = self%conditions%Groups(iBC)
    !     !     write (cBCGroup, '(i0)') iGroup
    !     !     key = Connect_dot(BCName, cBCGroup, ThermalName, TypeName)
    !     !     call json%get(key, self%conditions%Heat(iGroup)%type)
    !     !     call json%print_error_message(output_unit)

    !     !     select case (self%conditions%Heat(iGroup)%type)
    !     !     case (DirichletName, HeatTransferName)
    !     !         key = Connect_dot(BCName, cBCGroup, ThermalName, UniformName)
    !     !         call json%get(key, self%conditions%Heat(iGroup)%isUniform)
    !     !         call json%print_error_message(output_unit)

    !     !         key = Connect_dot(BCName, cBCGroup, ThermalName, ValueName)
    !     !         call json%get(key, self%conditions%Heat(iGroup)%value)
    !     !         call json%print_error_message(output_unit)
    !     !     end select

    !     ! key = inout_input_Connect_dot(BCName, cBCGroup, HydsraulicName, TypeName)
    !     ! call json%get(key, self%conditions%BC_Hydraulic(iBC)%type)
    !     ! call json%print_error_message(output_unit)

    !     ! select case (self%conditions%BC_Hydraulic(iBC)%type)
    !     ! case (DirichletName, HeatTransferName)
    !     !     key = inout_input_Connect_dot(BCName, cBCGroup, HydraulicName, ValueName)
    !     !     call json%get(key, self%conditions%BC_Hydraulic(iBC)%value)
    !     !     call json%print_error_message(output_unit)
    !     ! case default
    !     !     self%conditions%BC_Hydraulic(iBC)%value = NaNValue
    !     ! end select
    !     ! end do

    ! end subroutine inout_input_conditions_JSON_BC

    ! subroutine inout_input_conditions_JSON_IC(self, json)
    !     !> Load the initialy conditions from the JSON file
    !     implicit none
    !     class(type_input) :: self
    !     type(json_file), intent(inout) :: json !! JSON parser

    !     character(:), allocatable :: key
    !     character(:), allocatable :: tmp

    !     character(2) :: cICGroup
    !     integer(int32) :: i, count
    !     logical(4) :: isFind

    !     ! key = Connect_Dot(ICName, ThermalName, TypeName)
    !     ! call json%get(key, self%IC%Heat%type)
    !     ! call json%print_error_message(output_unit)

    !     ! select case (self%IC%Heat%type)
    !     ! case (ConstantName)
    !     !     key = Connect_Dot(ICName, ThermalName, ValueName)
    !     !     call json%get(key, self%IC%Heat%value)
    !     !     call json%print_error_message(output_unit)
    !     ! case (LaplaceName)
    !     !     stop 'Laplace type is not supported yet, sorry'

    !     ! end select

    !     ! key = Connect_Dot(ICName, HydraulicName, TypeName)
    !     ! call json%get(key, self%conditions%IC_Hydraulic%type)
    !     ! call json%print_error_message(output_unit)

    !     ! select case (self%conditions%IC_Hydraulic%type)
    !     ! case (ConstantName)
    !     !     key = Connect_Dot(ICName, HydraulicName, ValueName)
    !     !     call json%get(key, self%conditions%IC_Hydraulic%value)
    !     !     call json%print_error_message(output_unit)
    !     ! case (LaplaceName)
    !     !     count = 0
    !     !     do i = 1, size(self%conditions%BCGroup)
    !     !         write (cICGroup, '(i0)') self%conditions%BCGroup(i)
    !     !         key = Connect_Dot(ICName, HydraulicName, ValueName, cICGroup, TypeName)
    !     !         call json%get(key, tmp, found=isFind)
    !     !         if (isFind) count = count + 1
    !     !     end do
    !     !     allocate (self%conditions%IC_Hydraulic%IC_BC(count))
    !     !     count = 0
    !     !     do i = 1, size(self%conditions%BCGroup)
    !     !         write (cICGroup, '(i0)') self%conditions%BCGroup(i)
    !     !         key = Connect_Dot(ICName, HydraulicName, ValueName, cICGroup, TypeName)
    !     !         call json%get(key, tmp, found=isFind)

    !     !         if (.not. isFind) cycle
    !     !         count = count + 1

    !     !         key = Connect_Dot(ICName, HydraulicName, ValueName, cICGroup, TypeName)
    !     !         call json%get(key, self%conditions%IC_Hydraulic%IC_BC(count)%type)
    !     !         call json%print_error_message(output_unit)

    !     !         key = Connect_Dot(ICName, HydraulicName, ValueName, cICGroup, ValueName)
    !     !         call json%get(key, self%conditions%IC_Hydraulic%IC_BC(count)%value)
    !     !         call json%print_error_message(output_unit)
    !     !     end do
    !     ! end select

    ! end subroutine inout_input_conditions_JSON_IC

end submodule inout_input_conditions
