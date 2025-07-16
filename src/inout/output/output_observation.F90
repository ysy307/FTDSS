submodule(input_output) input_output_obaservation
    implicit none
contains

    module subroutine initialize_type_oservations(self, dir_output, variable_name, variable_unit, file_name)
        implicit none
        class(type_oservations), intent(inout) :: self
        character(*), intent(in) :: dir_output
        character(*), intent(in) :: variable_name
        character(*), intent(in) :: variable_unit
        character(*), intent(in) :: file_name

        self%name = trim(adjustl(variable_name))
        self%unit = trim(adjustl(variable_unit))
        self%file_name = trim(adjustl(dir_output))//trim(adjustl(file_name))
        self%num_unit = 99999999

    end subroutine initialize_type_oservations

    module subroutine initialize_type_output_observation(self, input, coordinate, domain)
        implicit none
        class(type_output_observation), intent(inout) :: self
        type(type_input), intent(in) :: input
        type(type_dp_3d), intent(inout), pointer :: coordinate
        type(type_domain), intent(inout) :: domain

        integer(int32) :: iObs, iElem, num_elements
        integer(int32) :: local_id, local_type, ierr
        real(real64) :: tmp_xi, tmp_eta

        logical :: inside

        self%type = input%output_settings%history_output%observation_type
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
            allocate (self%xi(self%num_observations))
            allocate (self%eta(self%num_observations))

            select case (input%basic%simulation_settings%calculate_type)

            case (1)
                do iObs = 1, self%num_observations
                    num_elements = domain%get_num_elements()
                    do iElem = 1, num_elements
                        call domain%Elements(iElem)%e%is_inside(self%coordinate%x(iObs), &
                                                                self%coordinate%y(iObs), &
                                                                tmp_xi, &
                                                                tmp_eta, &
                                                                inside)
                        if (inside) then
                            local_id = domain%Elements(iElem)%e%get_id()
                            call create_elements(new_element=self%elements(iObs)%e, &
                                                 id=local_id, &
                                                 global_coordinate=Coordinate, &
                                                 cell_info=input%geometry%vtk%cells(local_id), &
                                                 integration=input%basic%geometry_settings, &
                                                 ierr=ierr)
                            self%xi(iObs) = tmp_xi
                            self%eta(iObs) = tmp_eta
                            exit
                        end if
                    end do
                end do
            case (2)
                do iObs = 1, self%num_observations
                    num_elements = domain%get_num_elements()
                    do iElem = 1, num_elements
                        call domain%Elements(iElem)%e%is_inside(self%coordinate%x(iObs), &
                                                                self%coordinate%z(iObs), &
                                                                tmp_xi, &
                                                                tmp_eta, &
                                                                inside)
                        if (inside) then
                            local_id = domain%Elements(iElem)%e%get_id()
                            call create_elements(new_element=self%elements(iObs)%e, &
                                                 id=local_id, &
                                                 global_coordinate=Coordinate, &
                                                 cell_info=input%geometry%vtk%cells(local_id), &
                                                 integration=input%basic%geometry_settings, &
                                                 ierr=ierr)
                            self%xi(iObs) = tmp_xi
                            self%eta(iObs) = tmp_eta
                            exit
                        end if
                    end do
                end do
            end select
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
    !   self       : Object of Type_Output class containing observation data.
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
    module subroutine type_output_observation_write_header(self, variable, time_unit)
        implicit none
        class(type_output_observation), intent(inout) :: self
        class(type_oservations), intent(inout) :: variable
        character(*), intent(in) :: time_unit

        integer(int32) :: iObs, num_observations
        integer(int32) :: local_id

        num_observations = self%num_observations

        open (newunit=variable%num_unit, file=trim(adjustl(variable%file_name)), status='replace', action='write')

        write (variable%num_unit, '(a)') "# "//trim(variable%name)//" time variation"
        write (variable%num_unit, '(a)') "#"

        select case (self%type)
        case ("node_ids")
            write (variable%num_unit, '(a)') "# Observation Node ID"
            do iObs = 1, num_observations
                write (variable%num_unit, '(a,i0,a,x,i0)') "# Node ID ", iObs, ":", self%node_ids(iObs)
            end do
        case ("coordinates")
            write (variable%num_unit, '(a)') "# Observation Coordinate (x,y,z)"
            do iObs = 1, num_observations
                local_id = self%elements(iObs)%e%get_id()
                write (variable%num_unit, '(a,x,i0,a,3(x,es18.11,a),a,i0)') &
                    "#    Point", iObs, ": (", &
                    self%coordinate%x(iObs), ",", &
                    self%coordinate%y(iObs), ",", &
                    self%coordinate%z(iObs), ")", &
                    " => Element ID: ", &
                    local_id
            end do
        end select

        write (variable%num_unit, '(a)') "#"
        write (variable%num_unit, '(a)') "# Output Unit: Time ["//trim(adjustl(time_unit))//"], " & !&
                                        //trim(variable%name)//" ["//trim(variable%unit)//"]" !&
        write (variable%num_unit, '(a)') "#"
        write (variable%num_unit, '(a,'//to_string(num_observations)//'(2x,a))') &
            "Time", ("Obs"//to_string(iObs), iObs=1, num_observations)

    end subroutine type_output_observation_write_header

    !----------------------------------------------------------------------!
    ! interpolate_observations:
    !----------------------------------------------------------------------!
    ! This subroutine computes interpolated values at observation points
    ! using nodal values from the finite element mesh and shape functions.
    !
    ! Arguments:
    !   self         : Object of Type_Output class containing observation data.
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
    module subroutine interpolate_observations_temperature(obs_values, observation_data, nodal_temperature, &
                                                           nodal_porosity, nodal_pw, properties, domain)
        implicit none
        real(real64), intent(out) :: obs_values(:)
        type(type_output_observation), intent(in) :: observation_data
        real(real64), intent(in), optional :: nodal_temperature(:)
        real(real64), intent(in), optional :: nodal_porosity(:)
        real(real64), intent(in), optional :: nodal_pw(:)
        type(type_proereties_manager), intent(inout), optional :: properties
        type(type_domain), intent(inout), optional :: domain

        integer(int32) :: iObs
        real(real64), allocatable :: Original_Temperature(:)
        integer(int32) :: istat

        ! Initialize to zero
        obs_values(:) = 0.0d0
        if (.not. present(nodal_temperature)) return

        allocate (Original_Temperature, mold=nodal_temperature)
        call domain%reordering%to_original_value(nodal_temperature, Original_Temperature)

        do iObs = 1, observation_data%num_observations
            obs_values(iObs) = observation_data%elements(iObs)%e%interpolate( &
                               observation_data%xi(iObs), observation_data%eta(iObs), Original_Temperature(:))
        end do

        deallocate (Original_Temperature)
    end subroutine interpolate_observations_temperature

    module subroutine interpolate_observations_si(obs_values, observation_data, nodal_temperature, &
                                                  nodal_porosity, nodal_pw, properties, domain)
        implicit none
        real(real64), intent(out) :: obs_values(:)
        type(type_output_observation), intent(in) :: observation_data
        real(real64), intent(in), optional :: nodal_temperature(:)
        real(real64), intent(in), optional :: nodal_porosity(:)
        real(real64), intent(in), optional :: nodal_pw(:)
        type(type_proereties_manager), intent(inout), optional :: properties
        type(type_domain), intent(inout), optional :: domain

        type(type_gauss_point_state) :: state
        integer(int32) :: iObs, group_id

        real(real64), allocatable :: Original_Temperature(:)
        real(real64), allocatable :: Original_Porosity(:)
        integer(int32) :: istat

        ! Initialize to zero
        obs_values(:) = 0.0d0
        if (.not. present(nodal_temperature)) return
        if (.not. present(nodal_porosity)) return
        if (.not. present(nodal_pw)) state%pressure = 101325.0d0
        if (.not. present(properties)) return

        allocate (Original_Temperature, mold=nodal_temperature)
        allocate (Original_Porosity, mold=nodal_porosity)

        ! Reorder nodal values to original order
        call domain%reordering%to_original_value(nodal_temperature, Original_Temperature)
        call domain%reordering%to_original_value(nodal_porosity, Original_Porosity)

        do iObs = 1, observation_data%num_observations
            state%temperature = observation_data%elements(iObs)%e%Interpolate( &
                                observation_data%xi(iObs), observation_data%eta(iObs), Original_Temperature(:))
            state%porosity = observation_data%elements(iObs)%e%Interpolate( &
                             observation_data%xi(iObs), observation_data%eta(iObs), Original_Porosity(:))
            group_id = observation_data%elements(iObs)%e%get_group()
            state%water_content = properties%get_qw(state, group_id)
            obs_values(iObs) = (state%porosity - state%water_content) / state%porosity
        end do

        deallocate (Original_Temperature)
        deallocate (Original_Porosity)
    end subroutine interpolate_observations_si

    module subroutine interpolate_observations_thc(obs_values, observation_data, nodal_temperature, &
                                                   nodal_porosity, nodal_pw, properties, domain)
        implicit none
        real(real64), intent(out) :: obs_values(:)
        type(type_output_observation), intent(in) :: observation_data
        real(real64), intent(in), optional :: nodal_temperature(:)
        real(real64), intent(in), optional :: nodal_porosity(:)
        real(real64), intent(in), optional :: nodal_pw(:)
        type(type_proereties_manager), intent(inout), optional :: properties
        type(type_domain), intent(inout), optional :: domain

        type(type_gauss_point_state) :: state
        integer(int32) :: iObs, group_id
        real(real64), allocatable :: Original_Temperature(:)
        real(real64), allocatable :: Original_Porosity(:)
        integer(int32) :: istat

        ! Initialize to zero
        obs_values(:) = 0.0d0
        if (.not. present(nodal_temperature)) return
        if (.not. present(nodal_porosity)) return
        if (.not. present(properties)) return
        if (.not. present(nodal_pw)) then
            state%pressure = 101325.0d0

            allocate (Original_Temperature, mold=nodal_temperature)
            allocate (Original_Porosity, mold=nodal_porosity)
            call domain%reordering%to_original_value(nodal_temperature, Original_Temperature)
            call domain%reordering%to_original_value(nodal_porosity, Original_Porosity)

            do iObs = 1, observation_data%num_observations
                state%temperature = observation_data%elements(iObs)%e%Interpolate( &
                                    observation_data%xi(iObs), observation_data%eta(iObs), Original_Temperature(:))
                state%porosity = observation_data%elements(iObs)%e%Interpolate( &
                                 observation_data%xi(iObs), observation_data%eta(iObs), Original_Porosity(:))
                group_id = observation_data%elements(iObs)%e%get_group()
                state%water_content = properties%get_qw(state, group_id)
                obs_values(iObs) = properties%get_thc(state, group_id)
            end do

            deallocate (Original_Temperature)
            deallocate (Original_Porosity)
        end if

    end subroutine interpolate_observations_thc

    module subroutine interpolate_observations_vhc(obs_values, observation_data, nodal_temperature, &
                                                   nodal_porosity, nodal_pw, properties, domain)
        implicit none
        real(real64), intent(out) :: obs_values(:)
        type(type_output_observation), intent(in) :: observation_data
        real(real64), intent(in), optional :: nodal_temperature(:)
        real(real64), intent(in), optional :: nodal_porosity(:)
        real(real64), intent(in), optional :: nodal_pw(:)
        type(type_proereties_manager), intent(inout), optional :: properties
        type(type_domain), intent(inout), optional :: domain

        type(type_gauss_point_state) :: state
        integer(int32) :: iObs, group_id

        real(real64), allocatable :: Original_Temperature(:)
        real(real64), allocatable :: Original_Porosity(:)
        integer(int32) :: istat

        ! Initialize to zero
        obs_values(:) = 0.0d0
        if (.not. present(nodal_temperature)) return
        if (.not. present(nodal_porosity)) return
        if (.not. present(properties)) return
        if (.not. present(nodal_pw)) then
            state%pressure = 101325.0d0

            allocate (Original_Temperature, mold=nodal_temperature)
            allocate (Original_Porosity, mold=nodal_porosity)
            call domain%reordering%to_original_value(nodal_temperature, Original_Temperature)
            call domain%reordering%to_original_value(nodal_porosity, Original_Porosity)

            do iObs = 1, observation_data%num_observations
                state%temperature = observation_data%elements(iObs)%e%Interpolate( &
                                    observation_data%xi(iObs), observation_data%eta(iObs), Original_temperature(:))
                state%porosity = observation_data%elements(iObs)%e%Interpolate( &
                                 observation_data%xi(iObs), observation_data%eta(iObs), Original_porosity(:))
                group_id = observation_data%elements(iObs)%e%get_group()
                state%water_content = properties%get_qw(state, group_id)
                obs_values(iObs) = properties%get_vhc(state, group_id)
            end do

            deallocate (Original_Temperature)
            deallocate (Original_Porosity)
        end if
    end subroutine interpolate_observations_vhc

    module subroutine interpolate_observations_pw(obs_values, observation_data, nodal_temperature, &
                                                  nodal_porosity, nodal_pw, properties, domain)
        implicit none
        real(real64), intent(out) :: obs_values(:)
        type(type_output_observation), intent(in) :: observation_data
        real(real64), intent(in), optional :: nodal_temperature(:)
        real(real64), intent(in), optional :: nodal_porosity(:)
        real(real64), intent(in), optional :: nodal_pw(:)
        type(type_proereties_manager), intent(inout), optional :: properties
        type(type_domain), intent(inout), optional :: domain
        ! Note: nodal_pw is optional, if not present, pressure is assumed to be 101325.0d0

        type(type_gauss_point_state) :: state
        integer(int32) :: iObs, group_id
        real(real64), allocatable :: Original_Pressure(:)
        integer(int32) :: istat

        ! Initialize to zero
        obs_values(:) = 0.0d0
        if (.not. present(nodal_pw)) return

        allocate (Original_Pressure, mold=nodal_pw)
        call domain%reordering%to_original_value(nodal_pw, Original_Pressure)

        do iObs = 1, observation_data%num_observations
            obs_values(iObs) = observation_data%elements(iObs)%e%Interpolate( &
                               observation_data%xi(iObs), observation_data%eta(iObs), Original_Pressure(:))
        end do

        deallocate (Original_Pressure)
    end subroutine interpolate_observations_pw

    !----------------------------------------------------------------------!
    ! type_output_observation_Line:
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
    subroutine type_output_observation_Line(unit, time, values)
        use stdlib_strings, only: to_string
        implicit none
        integer(int32), intent(in) :: unit
        real(real64), intent(in) :: time
        real(real64), intent(in) :: values(:)

        write (unit, '(es22.15,'//to_string(size(values))//'(2x,es22.15))') time, values

    end subroutine type_output_observation_Line

    !----------------------------------------------------------------------!
    ! Output_Process_Observation:
    !----------------------------------------------------------------------!
    ! This subroutine handles the processing and output of observation
    ! data at a given time step. It supports both nodal and interpolated
    ! observation types and multiple physical variables.
    !
    ! Arguments:
    !   self    : Object of Type_Output class that manages output settings.
    !   time    : Current simulation time.
    !   Temp    : (Optional) Temperature field (nodal values).
    !   Si      : (Optional) Ice content or saturation index field.
    !   TC      : (Optional) Thermal conductivity field.
    !   C       : (Optional) Volumetric heat capacity field.
    !   Pres    : (Optional) Pressure field.
    !   wFlux   : (Optional) Water flux field.
    !   K       : (Optional) Hydraulic conductivity field.
    !   Thermal : (Optional) Object containing thermal models for ice computation.
    !   phi     : (Optional) Porosity or a related physical property used in ice models.
    !
    ! Subroutine Details:
    !   - For each enabled observation type and available input, performs:
    !       * Initialization of observation header (if needed)
    !       * Selection between direct node ID or interpolated values
    !       * Optional post-processing (e.g., ice content calculations via Thermal model)
    !   - Writes the results to the corresponding output files with time-stamped lines.
    !   - Supports extensibility by checking optional arguments and types (e.g., GCC, EXP models).
    !
    !----------------------------------------------------------------------!
    module subroutine Output_Process_Observation(self, time, Temp, Si, TC, C, Pres, wFlux, K, Thermal, phi, Propeties, domain)
        implicit none
        class(Type_Output) :: self
        real(real64), intent(in) :: time
        real(real64), intent(in), optional :: Temp(:)
        real(real64), intent(in), optional :: Si(:)
        real(real64), intent(in), optional :: TC(:)
        real(real64), intent(in), optional :: C(:)
        real(real64), intent(in), optional :: Pres(:)
        real(real64), intent(in), optional :: wFlux(:)
        real(real64), intent(in), optional :: K(:)
        class(abst_thermal), intent(inout), optional :: Thermal
        real(real64), intent(in), optional :: phi(:)
        type(type_proereties_manager), intent(inout), optional :: Propeties
        type(type_domain), intent(inout), optional :: domain

        real(real64) :: obsValues(self%Observation%num_observations)
        real(real64) :: tmpValues(self%Observation%num_observations)
        real(real64) :: obsValues2d(2 * self%Observation%num_observations)

        integer(int32) :: iObs

        !! Temperature
        do iObs = 1, size(self%Observation%Variables)
            if (self%Observation%Variables(iObs)%doOutput) then
                select case (self%Observation%ObservationType)
                case (1)
                    call type_output_observation_Line(self%Observation%Variables(iObs)%num_unit, time, Temp(self%Observation%ObsNodeID(:)))
                case (2)
                    call self%Observation%Variables(iObs)%get_values(obs_values=obsValues, &
                                                                     observation_data=self%Observation, &
                                                                     nodal_temperature=Temp, &
                                                                     nodal_porosity=phi, &
                                                                     properties=Propeties, &
                                                                     domain=domain)

                    call type_output_observation_Line(self%Observation%Variables(iObs)%num_unit, time, obsValues)
                end select
            end if
        end do

    end subroutine Output_Process_Observation

end submodule input_output_obaservation
