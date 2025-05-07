submodule(Inout_Output) Inout_Output_Obaservation
    implicit none
contains
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
    module subroutine Write_Observation_Header(self, data_label, var_unit, num_unit, filename)
        use stdlib_strings, only: to_string
        implicit none
        class(Type_Output) :: self
        character(*), intent(in) :: data_label
        character(*), intent(in) :: var_unit
        character(*), intent(in) :: filename
        integer(int32), intent(inout) :: num_unit

        integer(int32) :: iObs, nObs

        nObs = self%Observation%NumObservation

        open (newunit=num_unit, file=trim(adjustl(filename)), status='replace', action='write')

        write (num_unit, '(a)') "# "//trim(data_label)//" time variation"
        write (num_unit, '(a)') "#"

        select case (self%Observation%ObservationType)
        case (1)
            write (num_unit, '(a)') "# Observation Node ID"
            do iObs = 1, nObs
                write (num_unit, '(a,i0,a,x,i0)') "# Node ID ", iObs, ":", self%Observation%ObsNodeID(iObs)
            end do
        case (2)
            write (num_unit, '(a)') "# Observation Coordinate (x,y,z)"
            ! print *, self%Observation%Element(iObs)%e%ElementID
            ! stop
            do iObs = 1, nObs
                write (num_unit, '(a,x,i0,a,3(x,es18.11,a),a,i0)') &
                    "#    Point", iObs, ": (", &
                    self%Observation%Cood_Obs%x(iObs), ",", &
                    self%Observation%Cood_Obs%y(iObs), ",", &
                    self%Observation%Cood_Obs%z(iObs), ")", &
                    " => Element ID: ", &
                    self%Observation%Element(iObs)%e%ElementID
            end do
        end select

        write (num_unit, '(a)') "#"
        write (num_unit, '(a)') "# Output Unit: Time ["//trim(adjustl(self%Output_TimeUnit))//"], "//trim(data_label)//" ["//trim(var_unit)//"]"
        write (num_unit, '(a)') "#"
        write (num_unit, '(a,'//to_string(nObs)//'(2x,a))') "Time", ("Obs"//to_string(iObs), iObs=1, nObs)
    end subroutine Write_Observation_Header

    !----------------------------------------------------------------------!
    ! Initialize_Observation_Header:
    !----------------------------------------------------------------------!
    ! This subroutine initializes the header section for a specific type of
    ! observation data by calling the appropriate Write_Observation_Header
    ! routine with relevant parameters.
    !
    ! Arguments:
    !   self      : Object of Type_Output class containing observation data.
    !   data_name : String indicating the type of observation to process.
    !               Supported values include:
    !                 - "Temperature"
    !                 - "Si" (Ice content)
    !                 - "TC" (Thermal conductivity)
    !                 - "C" (Volumetric Heat Capacity)
    !                 - "Pressure"
    !                 - "wFlux" (Water flux)
    !                 - "K" (Hydraulic conductivity)
    !
    ! Subroutine Details:
    !   - Based on the value of `data_name`, selects the corresponding
    !     observation property in the object.
    !   - Calls Write_Observation_Header with the appropriate label,
    !     unit, output file name, and unit number (if it is valid).
    !   - If an unknown observation type is passed, an error message is
    !     printed and the program is terminated.
    !
    !----------------------------------------------------------------------!
    module subroutine Initialize_Observation_Header(self, data_name)
        implicit none
        class(Type_Output) :: self
        character(*), intent(in) :: data_name

        select case (data_name)
        case ("Temperature")
            if (self%Observation%Temperature%numUnit >= 0) then
                call self%Write_Observation_Header(data_label="Temperature", &
                                                   var_unit=self%Observation%Temperature%VariableUnit, &
                                                   num_unit=self%Observation%Temperature%numUnit, &
                                                   filename=self%Observation%Temperature%Filename)
            end if
        case ("Si")
            if (self%Observation%Si%numUnit >= 0) then
                call self%Write_Observation_Header(data_label="Ice content", &
                                                   var_unit=self%Observation%Si%VariableUnit, &
                                                   num_unit=self%Observation%Si%numUnit, &
                                                   filename=self%Observation%Si%Filename)
            end if
        case ("TC")
            if (self%Observation%TC%numUnit >= 0) then
                call self%Write_Observation_Header(data_label="Thermal conductivity", &
                                                   var_unit=self%Observation%TC%VariableUnit, &
                                                   num_unit=self%Observation%TC%numUnit, &
                                                   filename=self%Observation%TC%Filename)
            end if
        case ("C")
            if (self%Observation%C%numUnit >= 0) then
                call self%Write_Observation_Header(data_label="Volumetric Heat Capacity", &
                                                   var_unit=self%Observation%C%VariableUnit, &
                                                   num_unit=self%Observation%C%numUnit, &
                                                   filename=self%Observation%C%Filename)
            end if
        case ("Pressure")
            if (self%Observation%Pressure%numUnit >= 0) then
                call self%Write_Observation_Header(data_label="Pressure", &
                                                   var_unit=self%Observation%Pressure%VariableUnit, &
                                                   num_unit=self%Observation%Pressure%numUnit, &
                                                   filename=self%Observation%Pressure%Filename)
            end if
        case ("wFlux")
            if (self%Observation%wFlux%numUnit >= 0) then
                call self%Write_Observation_Header(data_label="Water flux", &
                                                   var_unit=self%Observation%wFlux%VariableUnit, &
                                                   num_unit=self%Observation%wFlux%numUnit, &
                                                   filename=self%Observation%wFlux%Filename)
            end if
        case ("K")
            if (self%Observation%K%numUnit >= 0) then
                call self%Write_Observation_Header(data_label="Hydraulic conductivity", &
                                                   var_unit=self%Observation%K%VariableUnit, &
                                                   num_unit=self%Observation%K%numUnit, &
                                                   filename=self%Observation%K%Filename)
            end if
        case default
            write (*, *) "Error: Unknown observation type: ", trim(adjustl(data_name))
            stop
        end select

    end subroutine Initialize_Observation_Header

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
    module subroutine Interpolate_ObsValues(self, nodal_values, obs_values)
        implicit none
        class(Type_Output), intent(in) :: self
        real(real64), intent(in) :: nodal_values(:)
        real(real64), intent(inout) :: obs_values(:)

        integer(int32) :: iObs, iN, nNodes

        ! Initialize to zero
        obs_values = 0.0d0

        ! Perform interpolation
        do iObs = 1, self%Observation%NumObservation
            nNodes = self%Observation%Element(iObs)%e%getNumNodes()
            do iN = 1, nNodes
                obs_values(iObs) = obs_values(iObs) + &
                                   nodal_values(self%Observation%Element(iObs)%e%conn(iN)) * &
                                   self%Observation%Element(iObs)%e%psi(iN, &
                                                                        self%Observation%obs_xi(iObs), &
                                                                        self%Observation%obs_eta(iObs))
            end do
        end do
    end subroutine Interpolate_ObsValues

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
    module subroutine Output_Process_Observation(self, time, Temp, Si, TC, C, Pres, wFlux, K, Thermal, phi)
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

        real(real64) :: obsValues(self%Observation%NumObservation)
        real(real64) :: tmpValues(self%Observation%NumObservation)
        real(real64) :: obsValues2d(2 * self%Observation%NumObservation)

        integer(int32) :: iObs

        !! Temperature
        if (self%doHeat .and. &
            self%Observation%Temperature%doOutput .and. &
            present(Temp) &
            ) then
            call self%Initialize_Observation_Header("Temperature")
            select case (self%Observation%ObservationType)
            case (1)
                call Output_Observation_Line(self%Observation%Temperature%numUnit, time, Temp(self%Observation%ObsNodeID(:)))
            case (2)
                call self%Interpolate_ObsValues(Temp, obsValues)
                call Output_Observation_Line(self%Observation%Temperature%numUnit, time, obsValues)
            end select
        end if

        !! Ice content
        if (self%doHeat .and. &
            self%Observation%Si%doOutput .and. &
            present(Si) &
            ) then
            select case (self%Observation%ObservationType)
            case (1)
                call Output_Observation_Line(self%Observation%Si%numUnit, time, Si(self%Observation%ObsNodeID(:)))
            case (2)
                obsValues(:) = 0.0d0
                tmpValues(:) = 0.0d0
                if (present(Thermal) .and. present(Temp) .and. present(phi)) then
                    call self%Interpolate_ObsValues(Si, obsValues)

                    select type (Ice => Thermal%Ice(1)%f)
                    type is (Type_Ice_GCC)
                        call self%Interpolate_ObsValues(phi, tmpValues)
                        call self%Interpolate_ObsValues(Temp, obsValues)
                        do iObs = 1, self%Observation%NumObservation
                            obsValues(iObs) = Ice%Calculate_Ice(T=obsValues(iObs), phi=tmpValues(iObs))
                        end do
                    type is (Type_Ice_EXP)
                        call self%Interpolate_ObsValues(phi, tmpValues)
                        call self%Interpolate_ObsValues(Temp, obsValues)
                        do iObs = 1, self%Observation%NumObservation
                            obsValues(iObs) = Ice%Calculate_Ice(T=obsValues(iObs), phi=tmpValues(iObs))
                        end do
                    end select
                    ! print *, obsValues(:)
                    ! stop
                    call Output_Observation_Line(self%Observation%Si%numUnit, time, obsValues)
                else
                    call self%Interpolate_ObsValues(Si, obsValues)
                    call Output_Observation_Line(self%Observation%Si%numUnit, time, obsValues)
                end if
            end select
        end if

    end subroutine Output_Process_Observation

end submodule Inout_Output_Obaservation
