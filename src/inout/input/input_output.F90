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
        call read_output_settings_history(self, json)
        call read_output_settings_standard(self, json)

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
                call convert_chracter_array(json, tmp_variable_names, self%output_settings%field_output%variable_names)
            end if

        end select

    end subroutine read_output_settings_fields

    subroutine read_output_settings_history(self, json)
        implicit none
        class(type_input), intent(inout) :: self
        type(json_file), intent(inout) :: json

        character(:), allocatable :: key
        character(:), allocatable :: key_base
        logical :: found
        integer(int32) :: i

        character(len=64), allocatable :: tmp_variable_names(:)

        key = join([history_output, file_format])
        call json%get(key, self%output_settings%history_output%file_format, found)
        call json%print_error_message(output_unit)
        if (.not. found) then
            self%output_settings%history_output%file_format = "none"
        else if (.not. any(valid_history_file_formats(:) == self%output_settings%history_output%file_format)) then
            call json%destroy()
            call error_message(905, c_opt=key)
        end if

        select case (self%output_settings%history_output%file_format)
        case (valid_history_file_formats(2), valid_history_file_formats(3))
            key = join([history_output, observation_type])
            call json%get(key, self%output_settings%history_output%observation_type, found)
            call json%print_error_message(output_unit)
            if (.not. found) then
                call json%destroy()
                call error_message(904, c_opt=key)
            else if (.not. any(valid_observation_types(:) == self%output_settings%history_output%observation_type)) then
                call json%destroy()
                call error_message(905, c_opt=key)
            end if

            key = join([history_output, output_interval, unit])
            call json%get(key, self%output_settings%history_output%output_interval_unit, found)
            call json%print_error_message(output_unit)
            if (.not. found) then
                call json%destroy()
                call error_message(904, c_opt=key)
            else if (.not. any(valid_units(:) == self%output_settings%history_output%output_interval_unit)) then
                call json%destroy()
                call error_message(905, c_opt=key)
            end if

            key = join([history_output, output_interval, value])
            call json%get(key, self%output_settings%history_output%output_interval_step, found)
            call json%print_error_message(output_unit)
            if (.not. found) then
                call json%destroy()
                call error_message(904, c_opt=key)
            else if (self%output_settings%history_output%output_interval_step <= 0) then
                call json%destroy()
                call error_message(905, c_opt=key)
            end if

            key = join([history_output, variables])
            call json%get(key, tmp_variable_names, found)
            call json%print_error_message(output_unit)
            if (.not. found) then
                call json%destroy()
                call error_message(904, c_opt=key)
            else if (size(tmp_variable_names) == 0) then
                call json%destroy()
                call error_message(905, c_opt=key)
            else
                call convert_chracter_array(json, tmp_variable_names, self%output_settings%history_output%variable_names)
            end if

            select case (self%output_settings%history_output%observation_type)
            case (valid_observation_types(1)) ! node_ids
                key = join([history_output, valid_observation_types(1)])
                call json%get(key, self%output_settings%history_output%node_ids, found)
                call json%print_error_message(output_unit)
                if (.not. found) then
                    call json%destroy()
                    call error_message(904, c_opt=key)
                else if (size(self%output_settings%history_output%node_ids) == 0) then
                    call json%destroy()
                    call error_message(905, c_opt=key)
                end if

                self%output_settings%history_output%num_observations = size(self%output_settings%history_output%node_ids)

            case (valid_observation_types(2)) ! coordinates
                key = join([history_output, valid_observation_types(2)])
                call json%info(key, found=found, n_children=self%output_settings%history_output%num_observations)
                call json%print_error_message(output_unit)
                if (.not. found) then
                    call json%destroy()
                    call error_message(904, c_opt=key)
                else if (self%output_settings%history_output%num_observations <= 0) then
                    call json%destroy()
                    call error_message(905, c_opt=key)
                end if
                if (allocated(self%output_settings%history_output%coordinates)) then
                    deallocate (self%output_settings%history_output%coordinates)
                end if
                allocate (self%output_settings%history_output%coordinates(self%output_settings%history_output%num_observations))

                do i = 1, self%output_settings%history_output%num_observations
                    key_base = join([history_output, valid_observation_types(2)//"("//to_string(i)//")"])
                    key = join([key_base, "x"])
                    call json%get(key, self%output_settings%history_output%coordinates(i)%x, found)
                    call json%print_error_message(output_unit)
                    if (.not. found) then
                        call json%destroy()
                        call error_message(904, c_opt=key)
                    end if
                    key = join([key_base, "y"])
                    call json%get(key, self%output_settings%history_output%coordinates(i)%y, found)
                    call json%print_error_message(output_unit)
                    if (.not. found) then
                        call json%destroy()
                        call error_message(904, c_opt=key)
                    end if
                    key = join([key_base, "z"])
                    call json%get(key, self%output_settings%history_output%coordinates(i)%z, found)
                    call json%print_error_message(output_unit)
                    if (.not. found) then
                        call json%destroy()
                        call error_message(904, c_opt=key)
                    end if
                end do
            end select
        end select

    end subroutine read_output_settings_history

    subroutine read_output_settings_standard(self, json)
        implicit none
        class(type_input), intent(inout) :: self
        type(json_file), intent(inout) :: json

        character(:), allocatable :: key
        logical :: found

        key = join([standard_output, print_progress])
        call json%get(key, self%output_settings%standard_output%print_progress, found)
        call json%print_error_message(output_unit)
        if (.not. found) then
            self%output_settings%standard_output%print_progress = .false.
        end if

        key = join([standard_output, print_interval, unit])
        call json%get(key, self%output_settings%standard_output%print_progress_unit, found)
        call json%print_error_message(output_unit)
        if (.not. found) then
            call json%destroy()
            call error_message(904, c_opt=key)
        else if (.not. any(valid_units(:) == self%output_settings%standard_output%print_progress_unit)) then
            call json%destroy()
            call error_message(905, c_opt=key)
        end if

        key = join([standard_output, print_interval, value])
        call json%get(key, self%output_settings%standard_output%print_progress_interval, found)
        call json%print_error_message(output_unit)
        if (.not. found) then
            call json%destroy()
            call error_message(904, c_opt=key)
        else if (self%output_settings%standard_output%print_progress_interval <= 0) then
            call json%destroy()
            call error_message(905, c_opt=key)
        end if

    end subroutine read_output_settings_standard

    subroutine convert_chracter_array(json, array, new_array)
        implicit none
        class(json_file), intent(inout) :: json
        character(*), allocatable, intent(inout) :: array(:)
        character(:), allocatable, intent(inout) :: new_array(:)

        integer(int32) :: i
        character(:), allocatable :: tmp_valid_names(:)
        logical, allocatable :: mask(:)

        call allocate_array(mask, size(array))
        mask(:) = .false.
        do i = 1, size(array)
            mask(i) = any(valid_variables(:) == trim(adjustl(array(i))))
        end do
        tmp_valid_names = pack(array, mask)
        if (size(tmp_valid_names) == 0) then
            call json%destroy()
            call error_message(905, c_opt="output variables")
        else
            if (allocated(new_array)) deallocate (new_array)
            allocate (new_array, source=tmp_valid_names)
        end if

        call deallocate_array(mask)
        if (allocated(array)) deallocate (array)
        if (allocated(tmp_valid_names)) deallocate (tmp_valid_names)

    end subroutine convert_chracter_array

end submodule inout_input_output_settings
