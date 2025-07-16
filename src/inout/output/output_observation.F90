submodule(input_output) input_output_obaservation
    implicit none
contains

    module subroutine ObservationVariable_Initialize(self, dir_Output, VariableName, FileName, VariableUnitName, doOutput)
        implicit none
        class(ObservationVariable_t), intent(inout) :: self
        character(*), intent(in) :: dir_Output
        character(*), intent(in) :: VariableName
        character(*), intent(in) :: FileName
        character(*), intent(in) :: VariableUnitName
        logical(4), intent(in) :: doOutput

        self%VariableName = trim(adjustl(VariableName))
        self%FileName = trim(adjustl(dir_Output))//trim(adjustl(FileName))
        self%num_unit = 99999999
        self%VariableUnitName = trim(adjustl(VariableUnitName))
        self%doOutput = doOutput

    end subroutine ObservationVariable_Initialize

    module subroutine Output_Observation_Initialize(self, Input, Coordinate, Domain)
        implicit none
        class(Output_Observation), intent(inout) :: self
        type(Type_Input), intent(in) :: Input
        type(type_dp_3d), intent(inout), pointer :: Coordinate
        type(type_domain), intent(inout), optional :: Domain

        integer(int32) :: iObs, iElem, nElements
        integer(int32) :: local_id, local_type, ierr
        real(real64) :: tmp_xi, tmp_eta

        logical :: inside

        self%ObservationType = Input%OutputSettings%ObservationType
        self%NumObservation = Input%OutputSettings%NumObservation
        select case (self%ObservationType)
        case (1)
            allocate (self%ObsNodeID, source=Input%OutputSettings%ObsID)
        case (2)
            call self%Cood_Obs%initialize(self%NumObservation)
            self%Cood_Obs = Input%OutputSettings%Cood_Obs
            allocate (self%Element(self%NumObservation))
            allocate (self%obs_xi(self%NumObservation))
            allocate (self%obs_eta(self%NumObservation))
            if (.not. present(Domain)) then
                stop "need Elements"
            else
                if (Input%Basic%DimensionType == 1) then
                    do iObs = 1, self%NumObservation
                        nElements = Domain%get_num_elements()
                        do iElem = 1, nElements
                            call Domain%Elements(iElem)%e%is_inside(self%Cood_Obs%x(iObs), &
                                                                    self%Cood_Obs%y(iObs), &
                                                                    tmp_xi, &
                                                                    tmp_eta, &
                                                                    inside)
                            if (inside) then
                                local_id = Domain%Elements(iElem)%e%get_id()
                                call create_element(new_element=self%Element(iObs)%e, &
                                                    id=local_id, &
                                                    global_coordinate=Coordinate, &
                                                    cell_info=Input%vtk%cells(local_id), &
                                                    ierr=ierr)
                                self%obs_xi(iObs) = tmp_xi
                                self%obs_eta(iObs) = tmp_eta
                                exit
                            end if
                        end do
                    end do
                else if (Input%Basic%DimensionType == 2) then
                    do iObs = 1, self%NumObservation
                        nElements = Domain%get_num_elements()
                        do iElem = 1, nElements
                            call Domain%Elements(iElem)%e%is_inside(self%Cood_Obs%x(iObs), &
                                                                    self%Cood_Obs%z(iObs), &
                                                                    tmp_xi, &
                                                                    tmp_eta, &
                                                                    inside)
                            if (inside) then
                                local_id = Domain%Elements(iElem)%e%get_id()
                                call create_element(new_element=self%Element(iObs)%e, &
                                                    id=local_id, &
                                                    global_coordinate=Coordinate, &
                                                    cell_info=Input%vtk%cells(local_id), &
                                                    ierr=ierr)
                                self%obs_xi(iObs) = tmp_xi
                                self%obs_eta(iObs) = tmp_eta
                                exit
                            end if
                        end do
                    end do
                end if
            end if
        end select

    end subroutine Output_Observation_Initialize

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
    module subroutine Output_Observation_Write_Header(self, ObsVar, TimeUnit)
        use stdlib_strings, only: to_string
        implicit none
        class(Output_Observation), intent(inout) :: self
        class(ObservationVariable_t), intent(inout) :: ObsVar
        character(*), intent(in) :: TimeUnit

        integer(int32) :: iObs, nObs
        integer(int32) :: local_id

        if (.not. ObsVar%doOutput) return

        nObs = self%NumObservation

        open (newunit=ObsVar%num_unit, file=trim(adjustl(ObsVar%FileName)), status='replace', action='write')

        write (ObsVar%num_unit, '(a)') "# "//trim(ObsVar%VariableName)//" time variation"
        write (ObsVar%num_unit, '(a)') "#"

        select case (self%ObservationType)
        case (1)
            write (ObsVar%num_unit, '(a)') "# Observation Node ID"
            do iObs = 1, nObs
                write (ObsVar%num_unit, '(a,i0,a,x,i0)') "# Node ID ", iObs, ":", self%ObsNodeID(iObs)
            end do
        case (2)
            write (ObsVar%num_unit, '(a)') "# Observation Coordinate (x,y,z)"
            do iObs = 1, nObs
                local_id = self%Element(iObs)%e%get_id()
                write (ObsVar%num_unit, '(a,x,i0,a,3(x,es18.11,a),a,i0)') &
                    "#    Point", iObs, ": (", &
                    self%Cood_Obs%x(iObs), ",", &
                    self%Cood_Obs%y(iObs), ",", &
                    self%Cood_Obs%z(iObs), ")", &
                    " => Element ID: ", &
                    local_id
            end do
        end select

        write (ObsVar%num_unit, '(a)') "#"
        write (ObsVar%num_unit, '(a)') "# Output Unit: Time ["//trim(adjustl(TimeUnit))//"], " & !&
                                        //trim(ObsVar%VariableName)//" ["//trim(ObsVar%VariableUnitName)//"]" !&
        write (ObsVar%num_unit, '(a)') "#"
        write (ObsVar%num_unit, '(a,'//to_string(nObs)//'(2x,a))') "Time", ("Obs"//to_string(iObs), iObs=1, nObs)

    end subroutine Output_Observation_Write_Header

    !----------------------------------------------------------------------!
    ! Interpolate_ObsValues:
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
    module subroutine Interpolate_ObsValues_Temperature(obs_values, observation_data, nodal_temperature, &
                                                        nodal_porosity, nodal_Pw, Properties, Domain)
        implicit none
        real(real64), intent(out) :: obs_values(:)
        type(Output_Observation), intent(in) :: observation_data
        real(real64), intent(in), optional :: nodal_temperature(:)
        real(real64), intent(in), optional :: nodal_porosity(:)
        real(real64), intent(in), optional :: nodal_Pw(:)
        type(type_proereties_manager), intent(inout), optional :: Properties
        type(type_domain), intent(inout), optional :: Domain

        integer(int32) :: iObs
        real(real64), allocatable :: Original_Temperature(:)
        integer(int32) :: istat

        ! Initialize to zero
        obs_values(:) = 0.0d0
        if (.not. present(nodal_temperature)) return

        allocate (Original_Temperature, mold=nodal_temperature)
        call Domain%rcm%reorder_to_original(nodal_temperature, Original_Temperature)

        do iObs = 1, observation_data%NumObservation
            obs_values(iObs) = observation_data%Element(iObs)%e%Interpolate( &
                               observation_data%obs_xi(iObs), observation_data%obs_eta(iObs), Original_Temperature(:))
        end do

        deallocate (Original_Temperature)
    end subroutine Interpolate_ObsValues_Temperature

    module subroutine Interpolate_ObsValues_Si(obs_values, observation_data, nodal_temperature, &
                                               nodal_porosity, nodal_Pw, Properties, Domain)
        implicit none
        real(real64), intent(out) :: obs_values(:)
        type(Output_Observation), intent(in) :: observation_data
        real(real64), intent(in), optional :: nodal_temperature(:)
        real(real64), intent(in), optional :: nodal_porosity(:)
        real(real64), intent(in), optional :: nodal_Pw(:)
        type(type_proereties_manager), intent(inout), optional :: Properties
        type(type_domain), intent(inout), optional :: Domain

        type(type_gauss_point_state) :: state
        integer(int32) :: iObs, group_id

        real(real64), allocatable :: Original_Temperature(:)
        real(real64), allocatable :: Original_Porosity(:)
        integer(int32) :: istat

        ! Initialize to zero
        obs_values(:) = 0.0d0
        if (.not. present(nodal_temperature)) return
        if (.not. present(nodal_porosity)) return
        if (.not. present(nodal_Pw)) state%pressure = 101325.0d0
        if (.not. present(Properties)) return

        allocate (Original_Temperature, mold=nodal_temperature)
        allocate (Original_Porosity, mold=nodal_porosity)

        ! Reorder nodal values to original order
        call Domain%rcm%reorder_to_original(nodal_temperature, Original_Temperature)
        call Domain%rcm%reorder_to_original(nodal_porosity, Original_Porosity)

        do iObs = 1, observation_data%NumObservation
            state%temperature = observation_data%Element(iObs)%e%Interpolate( &
                                observation_data%obs_xi(iObs), observation_data%obs_eta(iObs), Original_Temperature(:))
            state%porosity = observation_data%Element(iObs)%e%Interpolate( &
                             observation_data%obs_xi(iObs), observation_data%obs_eta(iObs), Original_Porosity(:))
            group_id = observation_data%Element(iObs)%e%get_group()
            state%water_content = Properties%get_qw(state, group_id)
            obs_values(iObs) = (state%porosity - state%water_content) / state%porosity
        end do

        deallocate (Original_Temperature)
        deallocate (Original_Porosity)
    end subroutine Interpolate_ObsValues_Si

    module subroutine Interpolate_ObsValues_THC(obs_values, observation_data, nodal_temperature, &
                                                nodal_porosity, nodal_Pw, Properties, Domain)
        implicit none
        real(real64), intent(out) :: obs_values(:)
        type(Output_Observation), intent(in) :: observation_data
        real(real64), intent(in), optional :: nodal_temperature(:)
        real(real64), intent(in), optional :: nodal_porosity(:)
        real(real64), intent(in), optional :: nodal_Pw(:)
        type(type_proereties_manager), intent(inout), optional :: Properties
        type(type_domain), intent(inout), optional :: Domain

        type(type_gauss_point_state) :: state
        integer(int32) :: iObs, group_id
        real(real64), allocatable :: Original_Temperature(:)
        real(real64), allocatable :: Original_Porosity(:)
        integer(int32) :: istat

        ! Initialize to zero
        obs_values(:) = 0.0d0
        if (.not. present(nodal_temperature)) return
        if (.not. present(nodal_porosity)) return
        if (.not. present(Properties)) return
        if (.not. present(nodal_Pw)) then
            state%pressure = 101325.0d0

            allocate (Original_Temperature, mold=nodal_temperature)
            allocate (Original_Porosity, mold=nodal_porosity)
            call Domain%rcm%reorder_to_original(nodal_temperature, Original_Temperature)
            call Domain%rcm%reorder_to_original(nodal_porosity, Original_Porosity)

            do iObs = 1, observation_data%NumObservation
                state%temperature = observation_data%Element(iObs)%e%Interpolate( &
                                    observation_data%obs_xi(iObs), observation_data%obs_eta(iObs), Original_Temperature(:))
                state%porosity = observation_data%Element(iObs)%e%Interpolate( &
                                 observation_data%obs_xi(iObs), observation_data%obs_eta(iObs), Original_Porosity(:))
                group_id = observation_data%Element(iObs)%e%get_group()
                state%water_content = Properties%get_qw(state, group_id)
                obs_values(iObs) = Properties%get_thc(state, group_id)
            end do

            deallocate (Original_Temperature)
            deallocate (Original_Porosity)
        end if

    end subroutine Interpolate_ObsValues_THC

    module subroutine Interpolate_ObsValues_VHC(obs_values, observation_data, nodal_temperature, &
                                                nodal_porosity, nodal_Pw, Properties, Domain)
        implicit none
        real(real64), intent(out) :: obs_values(:)
        type(Output_Observation), intent(in) :: observation_data
        real(real64), intent(in), optional :: nodal_temperature(:)
        real(real64), intent(in), optional :: nodal_porosity(:)
        real(real64), intent(in), optional :: nodal_Pw(:)
        type(type_proereties_manager), intent(inout), optional :: Properties
        type(type_domain), intent(inout), optional :: Domain

        type(type_gauss_point_state) :: state
        integer(int32) :: iObs, group_id

        real(real64), allocatable :: Original_Temperature(:)
        real(real64), allocatable :: Original_Porosity(:)
        integer(int32) :: istat

        ! Initialize to zero
        obs_values(:) = 0.0d0
        if (.not. present(nodal_temperature)) return
        if (.not. present(nodal_porosity)) return
        if (.not. present(Properties)) return
        if (.not. present(nodal_Pw)) then
            state%pressure = 101325.0d0

            allocate (Original_Temperature, mold=nodal_temperature)
            allocate (Original_Porosity, mold=nodal_porosity)
            call Domain%rcm%reorder_to_original(nodal_temperature, Original_Temperature)
            call Domain%rcm%reorder_to_original(nodal_porosity, Original_Porosity)

            do iObs = 1, observation_data%NumObservation
                state%temperature = observation_data%Element(iObs)%e%Interpolate( &
                                    observation_data%obs_xi(iObs), observation_data%obs_eta(iObs), Original_temperature(:))
                state%porosity = observation_data%Element(iObs)%e%Interpolate( &
                                 observation_data%obs_xi(iObs), observation_data%obs_eta(iObs), Original_porosity(:))
                group_id = observation_data%Element(iObs)%e%get_group()
                state%water_content = Properties%get_qw(state, group_id)
                obs_values(iObs) = Properties%get_vhc(state, group_id)
            end do

            deallocate (Original_Temperature)
            deallocate (Original_Porosity)
        end if
    end subroutine Interpolate_ObsValues_VHC

    module subroutine Interpolate_ObsValues_Pw(obs_values, observation_data, nodal_temperature, &
                                               nodal_porosity, nodal_Pw, Properties, Domain)
        implicit none
        real(real64), intent(out) :: obs_values(:)
        type(Output_Observation), intent(in) :: observation_data
        real(real64), intent(in), optional :: nodal_temperature(:)
        real(real64), intent(in), optional :: nodal_porosity(:)
        real(real64), intent(in), optional :: nodal_Pw(:)
        type(type_proereties_manager), intent(inout), optional :: Properties
        type(type_domain), intent(inout), optional :: Domain
        ! Note: nodal_Pw is optional, if not present, pressure is assumed to be 101325.0d0

        type(type_gauss_point_state) :: state
        integer(int32) :: iObs, group_id
        real(real64), allocatable :: Original_Pressure(:)
        integer(int32) :: istat

        ! Initialize to zero
        obs_values(:) = 0.0d0
        if (.not. present(nodal_Pw)) return

        allocate (Original_Pressure, mold=nodal_Pw)
        call Domain%rcm%reorder_to_original(nodal_Pw, Original_Pressure)

        do iObs = 1, observation_data%NumObservation
            obs_values(iObs) = observation_data%Element(iObs)%e%Interpolate( &
                               observation_data%obs_xi(iObs), observation_data%obs_eta(iObs), Original_Pressure(:))
        end do

        deallocate (Original_Pressure)
    end subroutine Interpolate_ObsValues_Pw

    !----------------------------------------------------------------------!
    ! Output_Observation_Line:
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
    subroutine Output_Observation_Line(unit, time, values)
        use stdlib_strings, only: to_string
        implicit none
        integer(int32), intent(in) :: unit
        real(real64), intent(in) :: time
        real(real64), intent(in) :: values(:)

        write (unit, '(es22.15,'//to_string(size(values))//'(2x,es22.15))') time, values

    end subroutine Output_Observation_Line

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
    module subroutine Output_Process_Observation(self, time, Temp, Si, TC, C, Pres, wFlux, K, Thermal, phi, Propeties, Domain)
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
        class(Abstract_Thermal), intent(inout), optional :: Thermal
        real(real64), intent(in), optional :: phi(:)
        type(type_proereties_manager), intent(inout), optional :: Propeties
        type(type_domain), intent(inout), optional :: Domain

        real(real64) :: obsValues(self%Observation%NumObservation)
        real(real64) :: tmpValues(self%Observation%NumObservation)
        real(real64) :: obsValues2d(2 * self%Observation%NumObservation)

        integer(int32) :: iObs

        !! Temperature
        do iObs = 1, size(self%Observation%Variables)
            if (self%Observation%Variables(iObs)%doOutput) then
                select case (self%Observation%ObservationType)
                case (1)
                    call Output_Observation_Line(self%Observation%Variables(iObs)%num_unit, time, Temp(self%Observation%ObsNodeID(:)))
                case (2)
                    call self%Observation%Variables(iObs)%get_values(obs_values=obsValues, &
                                                                     observation_data=self%Observation, &
                                                                     nodal_temperature=Temp, &
                                                                     nodal_porosity=phi, &
                                                                     properties=Propeties, &
                                                                     Domain=Domain)

                    call Output_Observation_Line(self%Observation%Variables(iObs)%num_unit, time, obsValues)
                end select
            end if
        end do

    end subroutine Output_Process_Observation

end submodule input_output_obaservation
