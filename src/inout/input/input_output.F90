submodule(inout_input) inout_input_output_settings
    implicit none
    !!------------------------------------------------------------------------------------------------------------------------------
    !! JSON key names for field output
    !!------------------------------------------------------------------------------------------------------------------------------
    character(*), parameter :: field_output = "field_output"
    character(*), parameter :: coloring = "coloring"
    character(*), parameter :: file_format = "file_format"
    character(*), parameter :: valid_field_file_formats(3) = ["none", "vtk", "vtu"]
    character(*), parameter :: unit = "unit"
    character(*), parameter :: valid_units(5) = ["second", "minute", "hour", "day", "year"]
    character(*), parameter :: value = "value"
    character(*), parameter :: variables = "variables"
    character(*), parameter :: valid_variables(7) = ["temperature", "ice_saturation", "thermal_conductivity", &
                                                     "volumetric_heat_capacity", "pressure", "water_flux", "hydraulic_conductivity"]
    !!------------------------------------------------------------------------------------------------------------------------------
    !! JSON key names for history output
    !!------------------------------------------------------------------------------------------------------------------------------
    character(*), parameter :: history_output = "history_output"
    character(*), parameter :: valid_history_file_formats(3) = ["none", "dat", "csv"]
    character(*), parameter :: observation_type = "observation_type"
    character(*), parameter :: valid_observation_types(2) = ["node_ids", "coordinates"]
    character(*), parameter :: output_interval = "output_interval"
    !!------------------------------------------------------------------------------------------------------------------------------
    !! JSON key names for standard output
    !!------------------------------------------------------------------------------------------------------------------------------
    character(*), parameter :: standard_output = "standard_output"
    character(*), parameter :: print_progress = "print_progress"
    character(*), parameter :: print_interval = "print_interval"
    !!------------------------------------------------------------------------------------------------------------------------------

contains

    module subroutine inout_read_output_settings(self)
        implicit none
        class(type_input), intent(inout) :: self
        type(json_file) :: json

        call json%initialize()

        call json%load(filename=self%output_file_name)
        call json%print_error_message(output_unit)

        call read_output_settings_fields(self, json)

        call json%destroy()
        call json%print_error_message(output_unit)

    end subroutine inout_read_output_settings

    subroutine read_output_settings_fields(self, json)
        implicit none
        class(type_input), intent(inout) :: self
        type(json_file), intent(inout) :: json

        character(:), allocatable :: key
        logical :: found
        integer(int32) :: i

        character(len=64), allocatable :: tmp_variable_names(:)
        character(:), allocatable :: tmp_valid_names(:)
        logical, allocatable :: mask(:)

        key = join([field_output, file_format])
        call json%get(key, self%output_settings%field_output%file_format, found)
        call json%print_error_message(output_unit)
        if (.not. found) then
            self%output_settings%field_output%file_format = "none"
        else if (.not. any(valid_field_file_formats(:) == self%output_settings%field_output%file_format)) then
            call json%destroy()
            call error_message(905, c_opt=key)
        end if

        select case (self%output_settings%field_output%file_format)
        case (valid_field_file_formats(2), valid_field_file_formats(3))

            key = join([field_output, coloring])
            call json%get(key, self%output_settings%field_output%coloring, found)
            call json%print_error_message(output_unit)
            if (.not. found) then
                self%output_settings%field_output%coloring = .false.
            else
                if (self%basic%solver_settings%coloring == "none") then
                    self%output_settings%field_output%coloring = .false.
                end if
            end if

            key = join([field_output, output_interval, unit])
            call json%get(key, self%output_settings%field_output%output_interval_unit, found)
            call json%print_error_message(output_unit)
            if (.not. found) then
                call json%destroy()
                call error_message(904, c_opt=key)
            else if (.not. any(valid_units(:) == self%output_settings%field_output%output_interval_unit)) then
                call json%destroy()
                call error_message(905, c_opt=key)
            end if

            key = join([field_output, output_interval, value])
            call json%get(key, self%output_settings%field_output%output_interval_step, found)
            call json%print_error_message(output_unit)
            if (.not. found) then
                call json%destroy()
                call error_message(904, c_opt=key)
            else if (self%output_settings%field_output%output_interval_step <= 0) then
                call json%destroy()
                call error_message(905, c_opt=key)
            end if

            key = join([field_output, variables])
            call json%get(key, tmp_variable_names, found)
            call json%print_error_message(output_unit)
            if (.not. found) then
                call json%destroy()
                call error_message(904, c_opt=key)
            else if (size(tmp_variable_names) == 0) then
                call json%destroy()
                call error_message(905, c_opt=key)
            else
                call allocate_array(mask, size(tmp_variable_names))
                mask(:) = .false.
                do i = 1, size(tmp_variable_names)
                    mask(i) = any(valid_variables(:) == trim(adjustl(tmp_variable_names(i))))
                end do
                tmp_valid_names = pack(tmp_variable_names, mask)
                if (size(tmp_valid_names) == 0) then
                    call json%destroy()
                    call error_message(905, c_opt=key)
                else
                    if (allocated(self%output_settings%field_output%variable_names)) deallocate (self%output_settings%field_output%variable_names)
                    allocate (self%output_settings%field_output%variable_names, source=tmp_valid_names)
                end if

                call deallocate_array(mask)
                if (allocated(tmp_variable_names)) deallocate (tmp_variable_names)
                if (allocated(tmp_valid_names)) deallocate (tmp_valid_names)

            end if

        end select

        !! debug output
        print *, "Field output settings:"
        print *, "  File format: ", self%output_settings%field_output%file_format
        print *, "  Coloring: ", self%output_settings%field_output%coloring
        print *, "  Output interval unit: ", self%output_settings%field_output%output_interval_unit
        print *, "  Output interval step: ", self%output_settings%field_output%output_interval_step
        print *, "  Variable names: "
        if (allocated(self%output_settings%field_output%variable_names)) then
            do i = 1, size(self%output_settings%field_output%variable_names)
                print *, "    ", self%output_settings%field_output%variable_names(i)
            end do
        else
            print *, "    None"
        end if
        stop

    end subroutine read_output_settings_fields

end submodule inout_input_output_settings
