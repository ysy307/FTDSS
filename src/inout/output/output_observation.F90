submodule(input_output) input_output_obaservation
    implicit none
contains
    module subroutine initialize_type_output_observation(self, input, coordinate, domain, dir_output, variable_name)
        implicit none
        class(type_output_observation), intent(inout) :: self
        type(type_input), intent(in) :: input
        type(type_dp_3d), intent(inout), pointer :: coordinate
        type(type_domain), intent(inout) :: domain
        character(*), intent(in) :: dir_output
        character(*), intent(in) :: variable_name

        integer(int32) :: iObs, iElem, num_elements
        integer(int32) :: local_id, local_type, ierr
        real(real64) :: tmp_xi, tmp_eta
        type(type_dp_vector_3d) :: cartesian, normalized

        integer(int32) :: num_target_variables

        logical :: inside

        self%type = input%output_settings%history_output%observation_type
        if (self%type == "none") then
            self%do_output = .false.
            return
        else
            self%do_output = .true.
        end if

        self%num_observations = input%output_settings%history_output%num_observations
        select case (self%type)
        case ("node_ids")
            if (allocated(self%node_ids)) deallocate (self%node_ids)
            allocate (self%node_ids, source=input%output_settings%history_output%node_ids)
        case ("coordinates")
            call self%coordinate%initialize(self%num_observations)
            do iObs = 1, self%num_observations
                self%coordinate%x(iObs) = input%output_settings%history_output%coordinates(iObs)%x
                self%coordinate%y(iObs) = input%output_settings%history_output%coordinates(iObs)%y
                self%coordinate%z(iObs) = input%output_settings%history_output%coordinates(iObs)%z
            end do
            allocate (self%elements(self%num_observations))
            allocate (self%coordinate_normalized(self%num_observations))

            select case (input%basic%simulation_settings%calculate_type)

            case (1)
                do iObs = 1, self%num_observations
                    num_elements = domain%get_num_elements()
                    do iElem = 1, num_elements
                        call cartesian%set([self%coordinate%x(iObs), self%coordinate%y(iObs), 0.0d0])
                        call domain%Elements(iElem)%e%is_inside(cartesian, &
                                                                normalized, &
                                                                inside)
                        if (inside) then
                            local_id = domain%Elements(iElem)%e%get_id()
                            self%elements(iObs)%e = create_element(local_id, &
                                                                   coordinate, &
                                                                   input)
                            self%coordinate_normalized = normalized
                            exit
                        end if
                        if (iElem == num_elements) then
                            print *, "Error: Observation point ", iObs, " is not inside any element."
                            stop
                        end if
                    end do
                end do
            case (2)
                do iObs = 1, self%num_observations
                    num_elements = domain%get_num_elements()
                    do iElem = 1, num_elements
                        call cartesian%set([self%coordinate%x(iObs), self%coordinate%z(iObs), 0.0d0])
                        call domain%Elements(iElem)%e%is_inside(cartesian, &
                                                                normalized, &
                                                                inside)
                        if (inside) then
                            local_id = domain%Elements(iElem)%e%get_id()
                            self%elements(iObs)%e = create_element(local_id, &
                                                                   coordinate, &
                                                                   input)
                            self%coordinate_normalized = normalized
                            exit
                        end if
                    end do
                    if (iElem == num_elements) then
                        print *, "Error: Observation point ", iObs, " is not inside any element."
                        stop
                    end if
                end do
            end select
        end select

        if (associated(self%get_values)) nullify (self%get_values)

        select case (trim(adjustl(variable_name)))
        case ("temperature")
            self%name = trim(adjustl(variable_name))
            self%unit = "°C"
            self%file_name = trim(adjustl(dir_output))//"obsf_T."//trim(adjustl(input%output_settings%history_output%file_format))
            self%num_unit = 99999999
            select case (self%type)
            case ("node_ids")
                self%get_values => get_observations_temperature
            case ("coordinates")
                self%get_values => interpolate_observations_temperature
            end select
            self%get_values => interpolate_observations_temperature
        case ("ice_saturation")
            self%name = trim(adjustl(variable_name))
            self%unit = "-"
            self%file_name = trim(adjustl(dir_output))//"obsf_Si."//trim(adjustl(input%output_settings%history_output%file_format))
            self%num_unit = 99999999
            select case (self%type)
            case ("node_ids")
                self%get_values => get_observations_si
            case ("coordinates")
                self%get_values => interpolate_observations_si
            end select
        case ("thermal_conductivity")
            self%name = trim(adjustl(variable_name))
            self%unit = "W/m/K"
            self%file_name = trim(adjustl(dir_output))//"obsf_TC."//trim(adjustl(input%output_settings%history_output%file_format))
            self%num_unit = 99999999
            select case (self%type)
            case ("node_ids")
                self%get_values => get_observations_thc
            case ("coordinates")
                self%get_values => interpolate_observations_thc
            end select
        case ("volumetric_heat_capacity")
            self%name = trim(adjustl(variable_name))
            self%unit = "J/m^3/K"
            self%file_name = trim(adjustl(dir_output))//"obsf_C."//trim(adjustl(input%output_settings%history_output%file_format))
            self%num_unit = 99999999
            select case (self%type)
            case ("node_ids")
                self%get_values => get_observations_vhc
            case ("coordinates")
                self%get_values => interpolate_observations_vhc
            end select
        case ("pressure")
            self%name = trim(adjustl(variable_name))
            self%unit = "m"
            self%file_name = trim(adjustl(dir_output))//"obsf_P."//trim(adjustl(input%output_settings%history_output%file_format))
            self%num_unit = 99999999
            select case (self%type)
            case ("node_ids")
                self%get_values => get_observations_pw
            case ("coordinates")
                self%get_values => interpolate_observations_pw
            end select
        case ("water_flux")
            self%name = trim(adjustl(variable_name))
            self%unit = "m/s"
            self%file_name = trim(adjustl(dir_output))//"obsf_Flux."//trim(adjustl(input%output_settings%history_output%file_format))
            self%num_unit = 99999999

            self%num_observations = self%num_observations * 3
        case ("hydraulic_conductivity")
            self%name = trim(adjustl(variable_name))
            self%unit = "m/s"
            self%file_name = trim(adjustl(dir_output))//"obsf_K."//trim(adjustl(input%output_settings%history_output%file_format))
            self%num_unit = 99999999
        end select

        if (associated(self%write_line)) nullify (self%write_line)
        if (associated(self%write_header)) nullify (self%write_header)
        select case (trim(adjustl(input%output_settings%history_output%file_format)))
        case ("dat")
            self%write_header => write_obeservation_header_dat
            self%write_line => output_observation_line_dat
        case ("csv")
            self%write_header => write_obeservation_header_csv
            self%write_line => output_observation_line_csv
        end select

    end subroutine initialize_type_output_observation

    !----------------------------------------------------------------------!
    ! Write_Observation_Header:
    !----------------------------------------------------------------------!
    ! This subroutine writes the header section of an observation output
    ! file, including metadata such as observation point IDs, coordinates,
    ! and variable units.
    !
    ! Arguments:
    !   self       : Object of type_output class containing observation data.
    !   data_label : String label describing the type of observation data.
    !   var_unit   : String representing the unit of the observed variable.
    !   num_unit   : Integer I/O variable to hold the unit number for the file.
    !   filename   : Name of the output file to write the header into.
    !
    ! Subroutine Details:
    !   - Opens the output file with status 'replace' and assigns a new unit.
    !   - Writes general information including the data label and time unit.
    !   - Depending on the ObservationType, outputs either:
    !       (1) Node IDs for observation points, or
    !       (2) Spatial coordinates and associated element IDs.
    !   - Writes column headers for time and observation values.
    !   - Uses the `to_string` procedure from stdlib_strings to build output format.
    !
    !----------------------------------------------------------------------!
    subroutine write_obeservation_header_dat(self, time_unit)
        implicit none
        class(type_output_observation), intent(inout) :: self
        character(*), intent(in) :: time_unit

        integer(int32) :: iObs, num_observations, idim
        integer(int32) :: local_id

        if (.not. self%do_output) return

        num_observations = self%num_observations

        open (newunit=self%num_unit, file=trim(adjustl(self%file_name)), status='replace', action='write')

        write (self%num_unit, '(a)') "# "//trim(self%name)//" time variation"
        write (self%num_unit, '(a)') "#"

        select case (self%type)
        case ("node_ids")
            write (self%num_unit, '(a)') "# Observation Node ID"
            do iObs = 1, num_observations
                write (self%num_unit, '(a,i0,a,x,i0)') "# Node ID ", iObs, ":", self%node_ids(iObs)
            end do
        case ("coordinates")
            write (self%num_unit, '(a)') "# Observation Coordinate (x,y,z)"
            do iObs = 1, num_observations
                local_id = self%elements(iObs)%e%get_id()
                write (self%num_unit, '(a,x,i0,a,3(x,es18.11,a),a,i0)') &
                    "#    Point", iObs, ": (", &
                    self%coordinate%x(iObs), ",", &
                    self%coordinate%y(iObs), ",", &
                    self%coordinate%z(iObs), ")", &
                    " => Element ID: ", &
                    local_id
            end do
        end select

        write (self%num_unit, '(a)') "#"
        write (self%num_unit, '(a)') "# Output Unit: Time ["//trim(adjustl(time_unit))//"], " & !&
                                        //trim(self%name)//" ["//trim(self%unit)//"]" !&
        write (self%num_unit, '(a)') "#"
        select case (self%name)
        case ("water_flux")
            write (self%num_unit, '(a,'//to_string(num_observations)//'(2x,a))') &
                "Time", (("Obs"//to_string(iObs)//"_x", "Obs"//to_string(iObs)//"_y", "Obs"//to_string(iObs)//"_z"), &
                         iObs=1, num_observations / 3)
        case default
            write (self%num_unit, '(a,'//to_string(num_observations)//'(2x,a))') &
                "Time", ("Obs"//to_string(iObs), iObs=1, num_observations)
        end select

    end subroutine write_obeservation_header_dat

    subroutine write_obeservation_header_csv(self, time_unit)
        implicit none
        class(type_output_observation), intent(inout) :: self
        character(*), intent(in) :: time_unit

        integer(int32) :: iObs, num_observations, idim
        integer(int32) :: local_id

        if (.not. self%do_output) return

        num_observations = self%num_observations

        open (newunit=self%num_unit, file=trim(adjustl(self%file_name)), status='replace', action='write')

        write (self%num_unit, '(a)') "# "//trim(self%name)//" time variation"
        write (self%num_unit, '(a)') "#"

        select case (self%type)
        case ("node_ids")
            write (self%num_unit, '(a)') "# Observation Node ID"
            do iObs = 1, num_observations
                write (self%num_unit, '(a,i0,a,x,i0)') "# Node ID ", iObs, ":", self%node_ids(iObs)
            end do
        case ("coordinates")
            write (self%num_unit, '(a)') "# Observation Coordinate (x,y,z)"
            do iObs = 1, num_observations
                local_id = self%elements(iObs)%e%get_id()
                write (self%num_unit, '(a,x,i0,a,3(x,es18.11,a),a,i0)') &
                    "#    Point", iObs, ": (", &
                    self%coordinate%x(iObs), ",", &
                    self%coordinate%y(iObs), ",", &
                    self%coordinate%z(iObs), ")", &
                    " => Element ID: ", &
                    local_id
            end do
        end select

        write (self%num_unit, '(a)') "#"
        write (self%num_unit, '(a)') "# Output Unit: Time ["//trim(adjustl(time_unit))//"], " & !&
                                        //trim(self%name)//" ["//trim(self%unit)//"]" !&
        write (self%num_unit, '(a)') "#"
        select case (self%name)
        case ("water_flux")
            write (self%num_unit, '(a,'//to_string(num_observations)//'(",",a))') &
                "Time", (("Obs"//to_string(iObs)//"_x", "Obs"//to_string(iObs)//"_y", "Obs"//to_string(iObs)//"_z"), &
                         iObs=1, num_observations / 3)
        case default
            write (self%num_unit, '(a,'//to_string(num_observations)//'(",",a))') &
                "Time", ("Obs"//to_string(iObs), iObs=1, num_observations)
        end select

    end subroutine write_obeservation_header_csv

    !----------------------------------------------------------------------!
    ! interpolate_observations:
    !----------------------------------------------------------------------!
    ! This subroutine computes interpolated values at observation points
    ! using nodal values from the finite element mesh and shape functions.
    !
    ! Arguments:
    !   self         : Object of type_output class containing observation data.
    !   nodal_values : Array of real values at nodes (e.g., temperature, pressure).
    !   obs_values   : Array to store interpolated values at observation points.
    !                  This is modified in-place (intent inout).
    !
    ! Subroutine Details:
    !   - Initializes all entries in `obs_values` to zero.
    !   - For each observation point, retrieves the associated element and
    !     its node connectivity and shape functions.
    !   - Performs standard FEM interpolation using:
    !       interpolated value = sum( nodal_value * shape_function )
    !   - Shape function values are evaluated at the observation's local
    !     coordinates (xi, eta).
    !
    !----------------------------------------------------------------------!
    subroutine interpolate_observations_temperature(self, obs_values, domain, properties, &
                                                    nodal_temperature, nodal_porosity, nodal_pw)
        implicit none
        class(type_output_observation), intent(inout) :: self
        real(real64), intent(out) :: obs_values(:)
        type(type_domain), intent(inout), optional :: domain
        type(type_properties_manager), intent(inout), optional :: properties
        real(real64), intent(in), optional :: nodal_temperature(:)
        real(real64), intent(in), optional :: nodal_porosity(:)
        real(real64), intent(in), optional :: nodal_pw(:)

        integer(int32) :: iObs
        real(real64), allocatable :: original_temperature(:)
        integer(int32) :: istat

        ! Initialize to zero
        obs_values(:) = 0.0d0
        if (.not. present(nodal_temperature)) return

        if (.not. domain%reordering%get_algorithm_name() == "none") then
            allocate (original_temperature, mold=nodal_temperature)
            call domain%reordering%to_original_value(nodal_temperature, original_temperature)
        else
            allocate (original_temperature, source=nodal_temperature)
        end if

        do iObs = 1, self%num_observations
            obs_values(iObs) = self%elements(iObs)%e%lerp(self%coordinate_normalized(iObs), original_temperature(:))
        end do

        deallocate (original_temperature)
    end subroutine interpolate_observations_temperature

    subroutine get_observations_temperature(self, obs_values, domain, properties, &
                                            nodal_temperature, nodal_porosity, nodal_pw)
        implicit none
        class(type_output_observation), intent(inout) :: self
        real(real64), intent(out) :: obs_values(:)
        type(type_domain), intent(inout), optional :: domain
        type(type_properties_manager), intent(inout), optional :: properties
        real(real64), intent(in), optional :: nodal_temperature(:)
        real(real64), intent(in), optional :: nodal_porosity(:)
        real(real64), intent(in), optional :: nodal_pw(:)

        integer(int32) :: iObs
        real(real64), allocatable :: original_temperature(:)
        integer(int32) :: istat

        ! Initialize to zero
        obs_values(:) = 0.0d0
        if (.not. present(nodal_temperature)) return

        if (.not. domain%reordering%get_algorithm_name() == "none") then
            allocate (original_temperature, mold=nodal_temperature)
            call domain%reordering%to_original_value(nodal_temperature, original_temperature)
        else
            allocate (original_temperature, source=nodal_temperature)
        end if

        do iObs = 1, self%num_observations
            obs_values(iObs) = original_temperature(self%node_ids(iObs))
        end do

        deallocate (original_temperature)
    end subroutine get_observations_temperature

    subroutine interpolate_observations_si(self, obs_values, domain, properties, &
                                           nodal_temperature, nodal_porosity, nodal_pw)
        implicit none
        class(type_output_observation), intent(inout) :: self
        real(real64), intent(out) :: obs_values(:)
        type(type_domain), intent(inout), optional :: domain
        type(type_properties_manager), intent(inout), optional :: properties
        real(real64), intent(in), optional :: nodal_temperature(:)
        real(real64), intent(in), optional :: nodal_porosity(:)
        real(real64), intent(in), optional :: nodal_pw(:)

        type(type_state) :: state
        integer(int32) :: iObs, group_id

        real(real64), allocatable :: original_temperature(:)
        real(real64), allocatable :: original_porosity(:)
        integer(int32) :: istat

        ! Initialize to zero
        obs_values(:) = 0.0d0
        if (.not. present(nodal_temperature)) return
        if (.not. present(nodal_porosity)) return
        if (.not. present(nodal_pw)) state%pressure = 101325.0d0
        if (.not. present(properties)) return

        if (.not. domain%reordering%get_algorithm_name() == "none") then
            allocate (original_temperature, mold=nodal_temperature)
            allocate (original_porosity, mold=nodal_porosity)
            call domain%reordering%to_original_value(nodal_temperature, original_temperature)
            call domain%reordering%to_original_value(nodal_porosity, original_porosity)
        else
            allocate (original_temperature, source=nodal_temperature)
            allocate (original_porosity, source=nodal_porosity)
        end if

        do iObs = 1, self%num_observations
            state%temperature = self%elements(iObs)%e%lerp(self%coordinate_normalized(iObs), original_temperature(:))
            state%porosity = self%elements(iObs)%e%lerp(self%coordinate_normalized(iObs), original_porosity(:))
            group_id = self%elements(iObs)%e%get_group()
            state%water_content = properties%calc_qw(group_id, state)
            if (state%water_content > state%porosity) state%water_content = state%porosity
            if (state%water_content < 0.0d0) state%water_content = 0.0d0
            obs_values(iObs) = (state%porosity - state%water_content) / state%porosity
        end do

        deallocate (original_temperature)
        deallocate (original_porosity)
    end subroutine interpolate_observations_si

    subroutine get_observations_si(self, obs_values, domain, properties, &
                                   nodal_temperature, nodal_porosity, nodal_pw)
        implicit none
        class(type_output_observation), intent(inout) :: self
        real(real64), intent(out) :: obs_values(:)
        type(type_domain), intent(inout), optional :: domain
        type(type_properties_manager), intent(inout), optional :: properties
        real(real64), intent(in), optional :: nodal_temperature(:)
        real(real64), intent(in), optional :: nodal_porosity(:)
        real(real64), intent(in), optional :: nodal_pw(:)

        type(type_state) :: state
        integer(int32) :: iObs, group_id

        real(real64), allocatable :: original_temperature(:)
        real(real64), allocatable :: original_porosity(:)
        integer(int32) :: istat

        ! Initialize to zero
        obs_values(:) = 0.0d0
        if (.not. present(nodal_temperature)) return
        if (.not. present(nodal_porosity)) return
        if (.not. present(nodal_pw)) state%pressure = 101325.0d0
        if (.not. present(properties)) return

        if (.not. domain%reordering%get_algorithm_name() == "none") then
            allocate (original_temperature, mold=nodal_temperature)
            allocate (original_porosity, mold=nodal_porosity)
            call domain%reordering%to_original_value(nodal_temperature, original_temperature)
            call domain%reordering%to_original_value(nodal_porosity, original_porosity)
        else
            allocate (original_temperature, source=nodal_temperature)
            allocate (original_porosity, source=nodal_porosity)
        end if

        do iObs = 1, self%num_observations
            state%temperature = original_temperature(self%node_ids(iObs))
            state%porosity = nodal_porosity(self%node_ids(iObs))
            group_id = self%elements(iObs)%e%get_group()
            state%water_content = properties%calc_qw(group_id, state)
            if (state%water_content > state%porosity) state%water_content = state%porosity
            if (state%water_content < 0.0d0) state%water_content = 0.0d0
            obs_values(iObs) = (state%porosity - state%water_content) / state%porosity
        end do

        deallocate (original_temperature)
        deallocate (original_porosity)
    end subroutine get_observations_si

    subroutine interpolate_observations_thc(self, obs_values, domain, properties, &
                                            nodal_temperature, nodal_porosity, nodal_pw)
        implicit none
        class(type_output_observation), intent(inout) :: self
        real(real64), intent(out) :: obs_values(:)
        type(type_domain), intent(inout), optional :: domain
        type(type_properties_manager), intent(inout), optional :: properties
        real(real64), intent(in), optional :: nodal_temperature(:)
        real(real64), intent(in), optional :: nodal_porosity(:)
        real(real64), intent(in), optional :: nodal_pw(:)

        type(type_state) :: state
        integer(int32) :: iObs, group_id
        real(real64), allocatable :: original_temperature(:)
        real(real64), allocatable :: original_porosity(:)
        integer(int32) :: istat

        ! Initialize to zero
        obs_values(:) = 0.0d0
        if (.not. present(nodal_temperature)) return
        if (.not. present(nodal_porosity)) return
        if (.not. present(properties)) return
        if (.not. present(nodal_pw)) then
            state%pressure = 101325.0d0

            if (.not. domain%reordering%get_algorithm_name() == "none") then
                allocate (original_temperature, mold=nodal_temperature)
                allocate (original_porosity, mold=nodal_porosity)
                call domain%reordering%to_original_value(nodal_temperature, original_temperature)
                call domain%reordering%to_original_value(nodal_porosity, original_porosity)
            else
                allocate (original_temperature, source=nodal_temperature)
                allocate (original_porosity, source=nodal_porosity)
            end if

            do iObs = 1, self%num_observations
                state%temperature = self%elements(iObs)%e%lerp(self%coordinate_normalized(iObs), original_temperature(:))
                state%porosity = self%elements(iObs)%e%lerp(self%coordinate_normalized(iObs), original_porosity(:))
                group_id = self%elements(iObs)%e%get_group()
                state%water_content = properties%calc_qw(group_id, state)
                if (state%water_content > state%porosity) state%water_content = state%porosity
                if (state%water_content < 0.0d0) state%water_content = 0.0d0
                obs_values(iObs) = properties%calc_thc(group_id, state)
            end do

            deallocate (original_temperature)
            deallocate (original_porosity)
        end if

    end subroutine interpolate_observations_thc

    subroutine get_observations_thc(self, obs_values, domain, properties, &
                                    nodal_temperature, nodal_porosity, nodal_pw)
        implicit none
        class(type_output_observation), intent(inout) :: self
        real(real64), intent(out) :: obs_values(:)
        type(type_domain), intent(inout), optional :: domain
        type(type_properties_manager), intent(inout), optional :: properties
        real(real64), intent(in), optional :: nodal_temperature(:)
        real(real64), intent(in), optional :: nodal_porosity(:)
        real(real64), intent(in), optional :: nodal_pw(:)

        type(type_state) :: state
        integer(int32) :: iObs, group_id
        real(real64), allocatable :: original_temperature(:)
        real(real64), allocatable :: original_porosity(:)
        integer(int32) :: istat

        ! Initialize to zero
        obs_values(:) = 0.0d0
        if (.not. present(nodal_temperature)) return
        if (.not. present(nodal_porosity)) return
        if (.not. present(properties)) return
        if (.not. present(nodal_pw)) state%pressure = 101325.0d0

        if (.not. domain%reordering%get_algorithm_name() == "none") then
            allocate (original_temperature, mold=nodal_temperature)
            allocate (original_porosity, mold=nodal_porosity)
            call domain%reordering%to_original_value(nodal_temperature, original_temperature)
            call domain%reordering%to_original_value(nodal_porosity, original_porosity)
        else
            allocate (original_temperature, source=nodal_temperature)
            allocate (original_porosity, source=nodal_porosity)
        end if

        do iObs = 1, self%num_observations
            state%temperature = original_temperature(self%node_ids(iObs))
            state%porosity = nodal_porosity(self%node_ids(iObs))
            group_id = self%elements(iObs)%e%get_group()
            state%water_content = properties%calc_qw(group_id, state)
            if (state%water_content > state%porosity) state%water_content = state%porosity
            if (state%water_content < 0.0d0) state%water_content = 0.0d0
            obs_values(iObs) = properties%calc_thc(group_id, state)
        end do

        deallocate (original_temperature)
        deallocate (original_porosity)

    end subroutine get_observations_thc

    subroutine interpolate_observations_vhc(self, obs_values, domain, properties, &
                                            nodal_temperature, nodal_porosity, nodal_pw)
        implicit none
        class(type_output_observation), intent(inout) :: self
        real(real64), intent(out) :: obs_values(:)
        type(type_domain), intent(inout), optional :: domain
        type(type_properties_manager), intent(inout), optional :: properties
        real(real64), intent(in), optional :: nodal_temperature(:)
        real(real64), intent(in), optional :: nodal_porosity(:)
        real(real64), intent(in), optional :: nodal_pw(:)

        type(type_state) :: state
        integer(int32) :: iObs, group_id

        real(real64), allocatable :: original_temperature(:)
        real(real64), allocatable :: original_porosity(:)
        integer(int32) :: istat

        ! Initialize to zero
        obs_values(:) = 0.0d0
        if (.not. present(nodal_temperature)) return
        if (.not. present(nodal_porosity)) return
        if (.not. present(properties)) return
        if (.not. present(nodal_pw)) then
            state%pressure = 101325.0d0

            if (.not. domain%reordering%get_algorithm_name() == "none") then
                allocate (original_temperature, mold=nodal_temperature)
                allocate (original_porosity, mold=nodal_porosity)
                call domain%reordering%to_original_value(nodal_temperature, original_temperature)
                call domain%reordering%to_original_value(nodal_porosity, original_porosity)
            else
                allocate (original_temperature, source=nodal_temperature)
                allocate (original_porosity, source=nodal_porosity)
            end if

            do iObs = 1, self%num_observations
                state%temperature = self%elements(iObs)%e%lerp(self%coordinate_normalized(iObs), original_temperature(:))
                state%porosity = self%elements(iObs)%e%lerp(self%coordinate_normalized(iObs), original_porosity(:))
                group_id = self%elements(iObs)%e%get_group()
                state%water_content = properties%calc_qw(group_id, state)
                if (state%water_content > state%porosity) state%water_content = state%porosity
                if (state%water_content < 0.0d0) state%water_content = 0.0d0
                obs_values(iObs) = properties%calc_vhc(group_id, state)
            end do

            deallocate (original_temperature)
            deallocate (original_porosity)
        end if
    end subroutine interpolate_observations_vhc

    subroutine get_observations_vhc(self, obs_values, domain, properties, &
                                    nodal_temperature, nodal_porosity, nodal_pw)
        implicit none
        class(type_output_observation), intent(inout) :: self
        real(real64), intent(out) :: obs_values(:)
        type(type_domain), intent(inout), optional :: domain
        type(type_properties_manager), intent(inout), optional :: properties
        real(real64), intent(in), optional :: nodal_temperature(:)
        real(real64), intent(in), optional :: nodal_porosity(:)
        real(real64), intent(in), optional :: nodal_pw(:)

        type(type_state) :: state
        integer(int32) :: iObs, group_id
        real(real64), allocatable :: original_temperature(:)
        real(real64), allocatable :: original_porosity(:)
        integer(int32) :: istat

        ! Initialize to zero
        obs_values(:) = 0.0d0
        if (.not. present(nodal_temperature)) return
        if (.not. present(nodal_porosity)) return
        if (.not. present(properties)) return
        if (.not. present(nodal_pw)) state%pressure = 101325.0d0

        if (.not. domain%reordering%get_algorithm_name() == "none") then
            allocate (original_temperature, mold=nodal_temperature)
            allocate (original_porosity, mold=nodal_porosity)
            call domain%reordering%to_original_value(nodal_temperature, original_temperature)
            call domain%reordering%to_original_value(nodal_porosity, original_porosity)
        else
            allocate (original_temperature, source=nodal_temperature)
            allocate (original_porosity, source=nodal_porosity)
        end if

        do iObs = 1, self%num_observations
            state%temperature = original_temperature(self%node_ids(iObs))
            state%porosity = nodal_porosity(self%node_ids(iObs))
            group_id = self%elements(iObs)%e%get_group()
            state%water_content = properties%calc_qw(group_id, state)
            if (state%water_content > state%porosity) state%water_content = state%porosity
            if (state%water_content < 0.0d0) state%water_content = 0.0d0
            obs_values(iObs) = properties%calc_vhc(group_id, state)
        end do

        deallocate (original_temperature)
        deallocate (original_porosity)
    end subroutine get_observations_vhc

    subroutine interpolate_observations_pw(self, obs_values, domain, properties, &
                                           nodal_temperature, nodal_porosity, nodal_pw)
        implicit none
        class(type_output_observation), intent(inout) :: self
        real(real64), intent(out) :: obs_values(:)
        type(type_domain), intent(inout), optional :: domain
        type(type_properties_manager), intent(inout), optional :: properties
        real(real64), intent(in), optional :: nodal_temperature(:)
        real(real64), intent(in), optional :: nodal_porosity(:)
        real(real64), intent(in), optional :: nodal_pw(:)

        type(type_state) :: state
        integer(int32) :: iObs, group_id
        real(real64), allocatable :: original_pressure(:)
        integer(int32) :: istat

        ! Initialize to zero
        obs_values(:) = 0.0d0
        if (.not. present(nodal_pw)) return

        if (.not. domain%reordering%get_algorithm_name() == "none") then
            allocate (original_pressure, mold=nodal_pw)
            call domain%reordering%to_original_value(nodal_pw, original_pressure)
        else
            allocate (original_pressure, source=nodal_pw)
        end if

        do iObs = 1, self%num_observations
            obs_values(iObs) = self%elements(iObs)%e%lerp(self%coordinate_normalized(iObs), original_pressure(:))
        end do

        deallocate (original_pressure)
    end subroutine interpolate_observations_pw

    subroutine get_observations_pw(self, obs_values, domain, properties, &
                                   nodal_temperature, nodal_porosity, nodal_pw)
        implicit none
        class(type_output_observation), intent(inout) :: self
        real(real64), intent(out) :: obs_values(:)
        type(type_domain), intent(inout), optional :: domain
        type(type_properties_manager), intent(inout), optional :: properties
        real(real64), intent(in), optional :: nodal_temperature(:)
        real(real64), intent(in), optional :: nodal_porosity(:)
        real(real64), intent(in), optional :: nodal_pw(:)

        integer(int32) :: iObs
        real(real64), allocatable :: original_pressure(:)
        integer(int32) :: istat

        ! Initialize to zero
        obs_values(:) = 0.0d0
        if (.not. present(nodal_pw)) return

        if (.not. domain%reordering%get_algorithm_name() == "none") then
            allocate (original_pressure, mold=nodal_pw)
            call domain%reordering%to_original_value(nodal_pw, original_pressure)
        else
            allocate (original_pressure, source=nodal_pw)
        end if

        do iObs = 1, self%num_observations
            obs_values(iObs) = original_pressure(self%node_ids(iObs))
        end do

        deallocate (original_pressure)
    end subroutine get_observations_pw

    !----------------------------------------------------------------------!
    ! type_output_observation_line_dat:
    !----------------------------------------------------------------------!
    ! This subroutine writes a single line of observation data to an
    ! output unit (file). The line includes the current time and a list of
    ! interpolated values at observation points.
    !
    ! Arguments:
    !   unit   : Integer specifying the output file unit number.
    !   time   : Real value representing the simulation time or timestamp.
    !   values : Array of real values corresponding to observation points.
    !
    ! Subroutine Details:
    !   - Uses scientific notation (es22.15) for high-precision output.
    !   - Dynamically adjusts the format string based on the number of values.
    !   - Outputs in the form:
    !       <time>  <value1>  <value2>  ... <valueN>
    !
!----------------------------------------------------------------------!
    subroutine output_observation_line_dat(self, unit, time, values)
        implicit none
        class(type_output_observation), intent(in) :: self
        integer(int32), intent(in) :: unit
        real(real64), intent(in) :: time
        real(real64), intent(in) :: values(:)

        write (unit, '(*(es22.15,:,2x))') time, values(1:self%num_observations)

    end subroutine output_observation_line_dat

    subroutine output_observation_line_csv(self, unit, time, values)
        implicit none
        class(type_output_observation), intent(in) :: self
        integer(int32), intent(in) :: unit
        real(real64), intent(in) :: time
        real(real64), intent(in) :: values(:)

        write (unit, '(*(es22.15,:,","))') time, values(1:self%num_observations)

    end subroutine output_observation_line_csv

end submodule input_output_obaservation
