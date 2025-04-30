module Inout_Output
    use, intrinsic :: iso_fortran_env, only: int8, int32, real64
    use :: Inout_SetProjectPath, only:GetProjectPath => Inout_SetProjectPath_GetProjectPath
    use :: error
    use :: Allocate_Allocate
    use :: Inout_Input
    use :: Types
    use :: Solver_Element
    use :: Main_Thermal

    implicit none
    private

    type :: Output_Config
        logical(4) :: doOutput
        character(:), allocatable :: Filename
        character(:), allocatable :: VariableUnit
        integer(int32) :: numUnit
    end type

    type :: Output_Observation
        type(Output_Config) :: Temperature
        type(Output_Config) :: Si
        type(Output_Config) :: TC
        type(Output_Config) :: C
        type(Output_Config) :: Pressure
        type(Output_Config) :: wFlux
        type(Output_Config) :: K

        integer(int32) :: ObservationType
        integer(int32) :: NumObservation
        type(DP3d) :: Cood_Obs
        type(ElementHolder), allocatable :: Element(:)
        real(real64), allocatable :: obs_xi(:)
        real(real64), allocatable :: obs_eta(:)
        integer(int32), allocatable :: ObsNodeID(:)
    end type

    type :: Output_VTK_Series
        integer(int32) :: nPoints
        integer(int32) :: nCell
        type(DP3d) :: Coordinates
        integer(int32), allocatable :: connectivity(:)
        integer(int32), allocatable :: offset(:)
        integer(int8), allocatable :: CellType(:)
    end type

    type :: Type_Output
        private
        character(:), allocatable :: fextend
        type(Output_Observation) :: Observation

        logical(4) :: doOutput_stdout

        character(:), allocatable :: dir_Output
        character(:), allocatable :: dir_FileOutput
        character(:), allocatable :: format_output

        type(Output_VTK_Series) :: VTKInfo
        character(:), allocatable :: Output_TimeUnit
        character(:), allocatable :: Interval_TimeUnit

        logical(4) :: doHeat
        logical(4) :: doPressure
        logical(4) :: doStress

    contains
        procedure, pass(self) :: Output_All_vtu => Inout_Output_All_vtu
        procedure, pass(self) :: Output_All_vtk => Inout_Output_All_vtk
        procedure, pass(self) :: Output_All_vtk_Scalar => Inout_Output_All_vtk_Scalar_Field
        procedure, pass(self) :: Output_All_vtk_Vector => Inout_Output_All_vtk_Vector_Field
        procedure, pass(self), public :: Output_All => Inout_Output_All
        procedure, pass(self) :: Output_Observation_Header => Inout_Output_Observation_Header
        procedure, pass(self), public :: Output_Observation => Inout_Output_Observation
    end type Type_Output

    interface Type_Output
        module procedure Output_Constructor
    end interface

    public :: Type_Output

contains

    function Output_Constructor(Structure_Input, Thermal) result(Structure)
        implicit none
        type(Type_Input), intent(in) :: Structure_Input
        class(Abstract_Thermal), intent(in), optional :: Thermal
        type(Type_Output) :: Structure

        character(256) :: dir_Path
        logical(4) :: exists

        character(:), allocatable :: command
        integer(int32) :: i, j, idx
        integer(int32) :: total
        real(real64) :: tmp_xi, tmp_eta
        logical(4) :: is_inside

        ! Path settings
        dir_Path = GetProjectPath()

        Structure%dir_Output = trim(adjustl(dir_Path))//"Output/"

        inquire (DIRECTORY=Structure%dir_Output, exist=exists)
        if (.not. exists) then
#ifdef _WIN32
            command = "mkdir "//'"'//trim(adjustl(Structure%dir_Output))//'"'
            call system(command)
#endif
#ifdef __linux__
            command = "mkdir -p "//'"'//trim(adjustl(Structure%dir_Output))//'"'
            call system(command)
#endif
        else
#ifdef _WIN32
            ! Windows
            command = "del /Q "//'"'//trim(adjustl(Structure%dir_Output))//"*.dat"//'"'
            call system(command)
            command = "del /Q "//'"'//trim(adjustl(Structure%dir_Output))//"*.csv"//'"'
            call system(command)
#endif
#ifdef __linux__
            ! Linux .datと.csvだけ削除
            command = "rm -f "//trim(adjustl(Structure%dir_Output))//"*.dat"
            call system(command)
            command = "rm -f "//trim(adjustl(Structure%dir_Output))//"*.csv"
            call system(command)
#endif
        end if

        Structure%dir_FileOutput = trim(adjustl(dir_Path))//"Output/Files/"

        inquire (DIRECTORY=Structure%dir_FileOutput, exist=exists)
        if (.not. exists) then
#ifdef _WIN32
            command = "mkdir "//'"'//trim(adjustl(Structure%dir_FileOutput))//'"'
            call system(command)
#endif
#ifdef __linux__
            command = "mkdir -p "//'"'//trim(adjustl(Structure%dir_FileOutput))//'"'
            call system(command)
#endif
        else
#ifdef _WIN32
            ! Windows .datと.csvだけ削除
            command = "del /Q "//'"'//trim(adjustl(Structure%dir_FileOutput))//"*.dat"//'"'
            call system(command)
            command = "del /Q "//'"'//trim(adjustl(Structure%dir_FileOutput))//"*.csv"//'"'
            call system(command)
            command = "del /Q "//'"'//trim(adjustl(Structure%dir_FileOutput))//"*.vtk"//'"'
            call system(command)
            command = "del /Q "//'"'//trim(adjustl(Structure%dir_FileOutput))//"*.vtu"//'"'
            call system(command)
#endif
#ifdef __linux__
            ! Linux .datと.csvだけ削除
            command = "rm -f "//trim(adjustl(Structure%dir_FileOutput))//"*.dat"
            call system(command)
            command = "rm -f "//trim(adjustl(Structure%dir_FileOutput))//"*.csv"
            call system(command)
            command = "rm -f "//trim(adjustl(Structure%dir_FileOutput))//"*.vtk"
            call system(command)
            command = "rm -f "//trim(adjustl(Structure%dir_FileOutput))//"*.vtu"
            call system(command)
#endif
        end if

        Structure%Observation%Temperature%Filename = trim(adjustl(Structure%dir_Output))//"obsf_T.dat"
        Structure%Observation%Si%Filename = trim(adjustl(Structure%dir_Output))//"obsf_Si.dat"
        Structure%Observation%TC%Filename = trim(adjustl(Structure%dir_Output))//"obsf_TC.dat"
        Structure%Observation%C%Filename = trim(adjustl(Structure%dir_Output))//"obsf_C.dat"
        Structure%Observation%Pressure%Filename = trim(adjustl(Structure%dir_Output))//"obsf_P.dat"
        Structure%Observation%wFlux%Filename = trim(adjustl(Structure%dir_Output))//"obsf_Flux.dat"
        Structure%Observation%K%Filename = trim(adjustl(Structure%dir_Output))//"obsf_K.dat"

        Structure%Observation%Temperature%doOutput = Structure_Input%OutputSettings%outTemp
        Structure%Observation%Si%doOutput = Structure_Input%OutputSettings%outSi
        Structure%Observation%TC%doOutput = Structure_Input%OutputSettings%outTC
        Structure%Observation%C%doOutput = Structure_Input%OutputSettings%outC
        Structure%Observation%Pressure%doOutput = Structure_Input%OutputSettings%outPres
        Structure%Observation%wFlux%doOutput = Structure_Input%OutputSettings%outFlux
        Structure%Observation%K%doOutput = Structure_Input%OutputSettings%outK

        Structure%Observation%Temperature%numUnit = -1
        Structure%Observation%Si%numUnit = -1
        Structure%Observation%TC%numUnit = -1
        Structure%Observation%C%numUnit = -1
        Structure%Observation%Pressure%numUnit = -1
        Structure%Observation%wFlux%numUnit = -1
        Structure%Observation%K%numUnit = -1

        Structure%Observation%Temperature%VariableUnit = "°C"
        Structure%Observation%Si%VariableUnit = "-"
        Structure%Observation%TC%VariableUnit = "W/m/K"
        Structure%Observation%C%VariableUnit = "kg/m^3"
        Structure%Observation%Pressure%VariableUnit = Structure_Input%Regions(1)%Ice%c_unit
        Structure%Observation%wFlux%VariableUnit = "m/s"
        Structure%Observation%K%VariableUnit = "m/s"

        Structure%Output_TimeUnit = Structure_Input%OutputSettings%Output_TimeUnit
        Structure%Interval_TimeUnit = Structure_Input%OutputSettings%Interval_TimeUnit

        Structure%doHeat = any(Structure_Input%Regions(:)%Flag%isHeat)
        Structure%doPressure = any(Structure_Input%Regions(:)%Flag%isWater)
        Structure%doStress = any(Structure_Input%Regions(:)%Flag%isStress)

        Structure%fextend = "."//trim(adjustl(Structure_Input%OutputSettings%FileFormat))

        Structure%doOutput_stdout = Structure_Input%Basic%shouldDisplayPrompt

        Structure%format_output = '(a,a,i5.5,a)'

        select case (Structure%fextend)
        case (".vtk")
            Structure%VTKInfo%nPoints = Structure_Input%VTK%numPoints
            Structure%VTKInfo%nCell = Structure_Input%VTK%numTotalCells
            call Structure%VTKInfo%Coordinates%allocate(Structure%VTKInfo%nPoints)
            Structure%VTKInfo%Coordinates = Structure_Input%VTK%POINTS

            call Allocate_Array(Structure%VTKInfo%offset, Structure%VTKInfo%nCell)
            call Allocate_Array(Structure%VTKInfo%CellType, Structure%VTKInfo%nCell)

            do i = 1, Structure%VTKInfo%nCell
                Structure%VTKInfo%offset(i) = Structure_Input%VTK%CELLS(i)%offset
                Structure%VTKInfo%CellType(i) = Structure_Input%VTK%CELLS(i)%CellType
            end do
            total = sum(Structure%VTKInfo%offset(:))

            call Allocate_Array(Structure%VTKInfo%connectivity, total)
            idx = 0
            do i = 1, Structure%VTKInfo%nCell
                do j = 1, Structure_Input%VTK%CELLS(i)%offset
                    idx = idx + 1
                    Structure%VTKInfo%connectivity(idx) = &
                        Structure_Input%VTK%CELLS(i)%connectivity(j) - 1
                end do
            end do
        case (".vtu")
            Structure%VTKInfo%nPoints = Structure_Input%VTK%numPoints
            Structure%VTKInfo%nCell = Structure_Input%VTK%numTotalCells
            call Structure%VTKInfo%Coordinates%allocate(Structure%VTKInfo%nPoints)
            Structure%VTKInfo%Coordinates = Structure_Input%VTK%POINTS

            call Allocate_Array(Structure%VTKInfo%offset, Structure%VTKInfo%nCell)
            call Allocate_Array(Structure%VTKInfo%CellType, Structure%VTKInfo%nCell)

            do i = 1, Structure%VTKInfo%nCell
                if (i == 1) then
                    Structure%VTKInfo%offset(i) = Structure_Input%VTK%CELLS(i)%offset
                else
                    Structure%VTKInfo%offset(i) = Structure%VTKInfo%offset(i - 1) + &
                                                  Structure_Input%VTK%CELLS(i)%offset
                end if
                Structure%VTKInfo%CellType(i) = Structure_Input%VTK%CELLS(i)%CellType
            end do
            total = Structure%VTKInfo%offset(Structure%VTKInfo%nCell)

            call Allocate_Array(Structure%VTKInfo%connectivity, total)
            do i = 1, Structure%VTKInfo%nCell
                if (i == 1) then
                    do j = 1, Structure_Input%VTK%CELLS(i)%offset
                        Structure%VTKInfo%connectivity(j) = Structure_Input%VTK%CELLS(i)%connectivity(j) - 1
                    end do
                else
                    do j = 1, Structure_Input%VTK%CELLS(i)%offset
                        Structure%VTKInfo%connectivity(Structure%VTKInfo%offset(i - 1) + j) = &
                            Structure_Input%VTK%CELLS(i)%connectivity(j) - 1
                    end do
                end if
            end do
        end select

        !! Search Observation Point

        Structure%Observation%ObservationType = Structure_Input%OutputSettings%ObservationType
        Structure%Observation%NumObservation = Structure_Input%OutputSettings%NumObservation
        select case (Structure%Observation%ObservationType)
        case (1)
            allocate (Structure%Observation%ObsNodeID, source=Structure_Input%OutputSettings%ObsID)
        case (2)
            call Structure%Observation%Cood_Obs%allocate(Structure%Observation%NumObservation)
            Structure%Observation%Cood_Obs = Structure_Input%OutputSettings%Cood_Obs
            allocate (Structure%Observation%Element(Structure%Observation%NumObservation))
            allocate (Structure%Observation%obs_xi(Structure%Observation%NumObservation))
            allocate (Structure%Observation%obs_eta(Structure%Observation%NumObservation))
            if (.not. present(Thermal)) then
                stop "need Elements"
            else
                if (Structure_Input%Basic%DimensionType == 1) then
                    do i = 1, Structure%Observation%NumObservation
                        do j = 1, size(Thermal%Elements)
                            call Thermal%Elements(j)%e%is_inside(Structure%Observation%Cood_Obs%x(i), &
                                                                 Structure%Observation%Cood_Obs%y(i), &
                                                                 tmp_xi, &
                                                                 tmp_eta, &
                                                                 is_inside)
                            if (is_inside) then
                                select case (Thermal%Elements(j)%e%ElementType)
                                case (5)
                                    Structure%Observation%Element(i)%e = TriangleFirst( &
                                                                         j, &
                                                                         Thermal%Coordinate, &
                                                                         Thermal%Elements(j)%e%conn, &
                                                                         Structure_Input%Basic%DimensionType)
                                case (9)
                                    Structure%Observation%Element(i)%e = SquareFirst( &
                                                                         j, &
                                                                         Thermal%Coordinate, &
                                                                         Thermal%Elements(j)%e%conn, &
                                                                         Structure_Input%Basic%DimensionType)
                                end select
                                Structure%Observation%obs_xi(i) = tmp_xi
                                Structure%Observation%obs_eta(i) = tmp_eta
                            end if
                        end do
                    end do
                else if (Structure_Input%Basic%DimensionType == 2) then
                    do i = 1, Structure%Observation%NumObservation
                        do j = 1, size(Thermal%Elements)
                            call Thermal%Elements(j)%e%is_inside(Structure%Observation%Cood_Obs%x(i), &
                                                                 Structure%Observation%Cood_Obs%z(i), &
                                                                 tmp_xi, &
                                                                 tmp_eta, &
                                                                 is_inside)
                            if (is_inside) then
                                select case (Thermal%Elements(j)%e%ElementType)
                                case (5)
                                    Structure%Observation%Element(i)%e = TriangleFirst( &
                                                                         j, &
                                                                         Thermal%Coordinate, &
                                                                         Thermal%Elements(j)%e%conn, &
                                                                         Structure_Input%Basic%DimensionType)
                                case (9)
                                    Structure%Observation%Element(i)%e = SquareFirst( &
                                                                         j, &
                                                                         Thermal%Coordinate, &
                                                                         Thermal%Elements(j)%e%conn, &
                                                                         Structure_Input%Basic%DimensionType)
                                end select
                                Structure%Observation%obs_xi(i) = tmp_xi
                                Structure%Observation%obs_eta(i) = tmp_eta
                            end if
                        end do
                    end do
                end if
            end if
        end select

    end function Output_Constructor

    subroutine Inout_Output_All(self, fc, Temp, Si, Pres, wFlux)
        implicit none
        class(Type_Output) :: self
        integer(int32), intent(in) :: fc

        real(real64), intent(in), optional :: Temp(:)
        real(real64), intent(in), optional :: Si(:)
        real(real64), intent(in), optional :: Pres(:)
        type(DP3d), intent(in), optional :: wFlux

        select case (trim(adjustl(self%fextend)))
        case (".vtk")
            call self%Output_All_vtk(fc=fc, Temp=Temp, Si=Si, Pres=Pres, wFlux=wFlux)
        case (".vtu")
            call self%Output_All_vtu(fc=fc, Temp=Temp, Si=Si, Pres=Pres, wFlux=wFlux)
        end select

    end subroutine Inout_Output_All

    subroutine Inout_Output_All_vtk(self, fc, Temp, Si, Pres, wFlux)
        use :: stdlib_strings, only:to_string
        implicit none
        class(Type_Output) :: self
        integer(int32), intent(in) :: fc

        real(real64), intent(in), optional :: Temp(:)
        real(real64), intent(in), optional :: Si(:)
        real(real64), intent(in), optional :: Pres(:)
        type(DP3d), intent(in), optional :: wFlux

        integer(int32) :: status
        integer(int32) :: unit_num
        integer(int32) :: iN, iE, idx

        character(256) :: outName

        ! Initialize VTK file
        write (outName, self%format_output) trim(self%dir_FileOutput), "Out_", fc, self%fextend
        open (newunit=unit_num, file=outName, status='replace', action='write', iostat=status)
        if (status /= 0) call error_message(931)

        write (unit_num, '(a)') "# vtk DataFile Version 2.0"
        write (unit_num, '(a)') "Analysis ASCII VTK file"
        write (unit_num, '(a)') "ASCII"
        write (unit_num, '(a)') "DATASET UNSTRUCTURED_GRID"
        write (unit_num, '(a,i0,a)') "POINTS ", self%VTKInfo%nPoints, " double"

        do iN = 1, self%VTKInfo%nPoints
            write (unit_num, '(3(es22.15,x))') self%VTKInfo%Coordinates%x(iN), self%VTKInfo%Coordinates%y(iN), self%VTKInfo%Coordinates%z(iN)
        end do
        write (unit_num, '(a)') ""

        write (unit_num, '(a,i0,x,i0,a)') "CELLS ", self%VTKInfo%nCell, sum(self%VTKInfo%offset(:)) + self%VTKInfo%nCell
        idx = 1
        do iE = 1, self%VTKInfo%nCell
            write (unit_num, '(i0,'//to_string(self%VTKInfo%offset(iE))//'(x,i0))') self%VTKInfo%offset(iE), self%VTKInfo%connectivity(idx:idx + self%VTKInfo%offset(iE) - 1)
            idx = idx + self%VTKInfo%offset(iE)
        end do
        write (unit_num, '(a)') ""

        write (unit_num, '(a,i0)') "CELL_TYPES ", self%VTKInfo%nCell
        do iE = 1, self%VTKInfo%nCell
            write (unit_num, '(i0)') self%VTKInfo%CellType(iE)
        end do
        write (unit_num, '(a)') ""

        write (unit_num, '(a, i0)') "POINT_DATA ", self%VTKInfo%nPoints
        if (present(Temp)) then
            call self%Output_All_vtk_Scalar(unit_num=unit_num, &
                                            data_name='Temperature', &
                                            x=Temp)
        end if
        if (present(Si)) then
            call self%Output_All_vtk_Scalar(unit_num=unit_num, &
                                            data_name='Si', &
                                            x=Si)
        end if
        if (present(Pres)) then
            call self%Output_All_vtk_Scalar(unit_num=unit_num, &
                                            data_name='Pressure', &
                                            x=Pres)
        end if
        if (present(wFlux)) then
            call self%Output_All_vtk_Vector(unit_num=unit_num, &
                                            data_name='waterFlux', &
                                            x=wFlux%x, &
                                            y=wFlux%y, &
                                            z=wFlux%z)
        end if

    end subroutine Inout_Output_All_vtk

    subroutine Inout_Output_All_vtk_Scalar_Field(self, unit_num, data_name, x)
        implicit none
        class(Type_Output) :: self
        integer(int32), intent(in) :: unit_num
        character(*), intent(in) :: data_name
        real(real64), intent(in) :: x(:)

        write (unit_num, '(3a)') "SCALARS ", trim(adjustl(data_name)), " double 1"
        write (unit_num, '(a)') "LOOKUP_TABLE default"
        write (unit_num, '(es22.15)') x(:)
        write (unit_num, '(a)') ""

    end subroutine Inout_Output_All_vtk_Scalar_Field

    subroutine Inout_Output_All_vtk_Vector_Field(self, unit_num, data_name, x, y, z)
        implicit none
        class(Type_Output) :: self
        integer(int32), intent(in) :: unit_num
        character(*), intent(in) :: data_name
        real(real64), intent(in) :: x(:), y(:), z(:)
        integer(int32) :: i

        write (unit_num, '(3a)') "VECTORS ", trim(adjustl(data_name)), " double"
        do i = 1, size(x)
            write (unit_num, '(3(es22.15,x))') x(i), y(i), z(i)
        end do
        write (unit_num, '(a)') ""
    end subroutine

    subroutine Inout_Output_All_vtu(self, fc, Temp, Si, Pres, wFlux)
        use :: vtk_fortran, only:vtk_file
        implicit none
        class(Type_Output) :: self
        integer(int32), intent(in) :: fc

        real(real64), intent(in), optional :: Temp(:)
        real(real64), intent(in), optional :: Si(:)
        real(real64), intent(in), optional :: Pres(:)
        type(DP3d), intent(in), optional :: wFlux

        type(vtk_file) :: vtu
        integer(int32) :: status

        character(256) :: outName

        ! Initialize VTK file
        write (outName, self%format_output) trim(self%dir_FileOutput), "Out_", fc, self%fextend
        status = vtu%initialize(format='ascii', filename=trim(outName), mesh_topology='UnstructuredGrid')

        ! Write data
        status = vtu%xml_writer%write_piece(np=self%VTKInfo%nPoints, &
                                            nc=self%VTKInfo%nCell)
        status = vtu%xml_writer%write_geo(np=self%VTKInfo%nPoints, &
                                          nc=self%VTKInfo%nCell, &
                                          x=self%VTKInfo%Coordinates%x, &
                                          y=self%VTKInfo%Coordinates%y, &
                                          z=self%VTKInfo%Coordinates%z)
        status = vtu%xml_writer%write_connectivity(nc=self%VTKInfo%nCell, &
                                                   connectivity=self%VTKInfo%connectivity, &
                                                   offset=self%VTKInfo%offset, &
                                                   cell_type=self%VTKInfo%CellType)
        status = vtu%xml_writer%write_dataarray(location='node', action='open')
        if (present(Temp)) then
            status = vtu%xml_writer%write_dataarray(data_name='Temperature', &
                                                    x=Temp)
        end if
        if (present(Si)) then
            status = vtu%xml_writer%write_dataarray(data_name='Si', &
                                                    x=Si)
        end if
        if (present(Pres)) then
            status = vtu%xml_writer%write_dataarray(data_name='Pressure', &
                                                    x=Pres)
        end if
        if (present(wFlux)) then
            status = vtu%xml_writer%write_dataarray(data_name='waterFlux', &
                                                    x=wFlux%x, &
                                                    y=wFlux%y, &
                                                    z=wFlux%z)
        end if
        status = vtu%xml_writer%write_dataarray(location='node', action='close')
        status = vtu%xml_writer%write_piece()

        ! Finalize VTK file
        status = vtu%finalize()

    end subroutine Inout_Output_All_vtu

    subroutine Inout_Output_Observation(self, time, Temp, Si, TC, C, Pres, wFlux, K, Thermal)
        use :: stdlib_strings, only:to_string
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

        integer(int32) :: iObs, iE, iS, iN
        integer(int32) :: nNodes, nObs
        real(real64) :: obsValues(self%Observation%NumObservation)
        real(real64) :: obsValues2d(2 * self%Observation%NumObservation)

        nObs = self%Observation%NumObservation

        !! Temperature
        if (self%doHeat .and. &
            present(Temp) .and. &
            self%Observation%Temperature%doOutput &
            ) then
            call self%Output_Observation_Header(time, "Temperature")
            select case (self%Observation%ObservationType)
            case (1)
                write (self%Observation%Temperature%numUnit, '(es22.15,'//to_string(nObs)//'(2x,es22.15))') &
                    time, (Temp(self%Observation%ObsNodeID(iObs)), iObs=1, nObs)
            case (2)
                obsValues(:) = 0.0d0
                do iObs = 1, nObs
                    nNodes = self%Observation%Element(iObs)%e%getNumNodes()
                    do iN = 1, nNodes
                        obsValues(iObs) = obsValues(iObs) + &
                                          (Temp(self%Observation%Element(iObs)%e%conn(iN)) * &
                                           self%Observation%Element(iN)%e%psi(iN, &
                                                                              self%Observation%obs_xi(iObs), &
                                                                              self%Observation%obs_eta(iObs)))
                    end do
                end do
                write (self%Observation%Temperature%numUnit, '(es22.15,'//to_string(nObs)//'(x,es22.15))') &
                    time, (obsValues(iObs), iObs=1, nObs)
            end select
        end if

        !! Ice content
        if (self%doHeat .and. &
            present(Temp) .and. &
            present(Si) .and. &
            present(Thermal) .and. &
            self%Observation%Si%doOutput &
            ) then
            call self%Output_Observation_Header(time, "Si")
            select case (self%Observation%ObservationType)
            case (1)
                write (self%Observation%Si%numUnit, '(es22.15,'//to_string(nObs)//'(2x,es22.15))') &
                    time, (Si(self%Observation%ObsNodeID(iObs)), iObs=1, nObs)
            case (2)
                obsValues(:) = 0.0d0
                do iObs = 1, nObs
                    nNodes = self%Observation%Element(iObs)%e%getNumNodes()
                    select type (Ice => Thermal%Ice)
                    type is (Type_Ice_TRM)
                        do iN = 1, nNodes
                            obsValues(iObs) = obsValues(iObs) + &
                                              (Si(self%Observation%Element(iObs)%e%conn(iN)) * &
                                               self%Observation%Element(iN)%e%psi(iN, &
                                                                                  self%Observation%obs_xi(iObs), &
                                                                                  self%Observation%obs_eta(iObs)))
                        end do
                    type is (Type_Ice_GCC)
                        do iN = 1, nNodes
                            obsValues(iObs) = obsValues(iObs) + &
                                              (Temp(self%Observation%Element(iObs)%e%conn(iN)) * &
                                               self%Observation%Element(iN)%e%psi(iN, &
                                                                                  self%Observation%obs_xi(iObs), &
                                                                                  self%Observation%obs_eta(iObs)))
                        end do
                        obsValues(iObs) = Ice%Calculate_Ice(obsValues(iObs))
                    type is (Type_Ice_EXP)
                        do iN = 1, nNodes
                            obsValues(iObs) = obsValues(iObs) + &
                                              (Temp(self%Observation%Element(iObs)%e%conn(iN)) * &
                                               self%Observation%Element(iN)%e%psi(iN, &
                                                                                  self%Observation%obs_xi(iObs), &
                                                                                  self%Observation%obs_eta(iObs)))
                        end do
                        obsValues(iObs) = Ice%Calculate_Ice(obsValues(iObs))
                    end select
                end do
                write (self%Observation%Si%numUnit, '(es22.15,'//to_string(nObs)//'(x,es22.15))') &
                    time, (obsValues(iObs), iObs=1, nObs)
            end select
        end if

    end subroutine Inout_Output_Observation

    subroutine Inout_Output_Observation_Header(self, time, data_name)
        use :: stdlib_strings, only:to_string
        implicit none
        class(Type_Output) :: self
        real(real64), intent(in) :: time
        character(*), intent(in) :: data_name

        integer(int32) :: iObs, iN
        integer(int32) :: nObs, nNodes

        nObs = self%Observation%NumObservation
        select case (data_name)
        case ("Temperature")
            if (self%Observation%Temperature%numUnit < 0) then
                open (newunit=self%Observation%Temperature%numUnit, &
                      file=trim(adjustl(self%Observation%Temperature%Filename)), &
                      status='replace', &
                      action='write')
                !! Write file header
                write (self%Observation%Temperature%numUnit, '(a)') "# "//data_name//" time varieation"
                write (self%Observation%Temperature%numUnit, '(a)') "#"
                select case (self%Observation%ObservationType)
                case (1)
                    write (self%Observation%Temperature%numUnit, '(a)') "# Observation Node ID"
                    do iObs = 1, nObs
                        write (self%Observation%Temperature%numUnit, '(a,i0,a,x,i0)') &
                            "# Node ID ", iObs, ":", self%Observation%ObsNodeID(iObs)
                    end do
                case (2)
                    write (self%Observation%Temperature%numUnit, '(a)') "# Observation Coordinate (x,y,z)"
                    do iObs = 1, nObs
                        write (self%Observation%Temperature%numUnit, '(a,x,i0,a,3(x,es18.11,a),a)') &
                            "# Point", iObs, ": (", &
                            self%Observation%Cood_Obs%x(iObs), ",", &
                            self%Observation%Cood_Obs%y(iObs), ",", &
                            self%Observation%Cood_Obs%z(iObs), &
                            ")"
                    end do
                end select
                write (self%Observation%Temperature%numUnit, '(a)') "#"
                write (self%Observation%Temperature%numUnit, '(a)') &
                    "# Output Unit: Time ["//trim(adjustl(self%Output_TimeUnit))//"], Temperature ["//self%Observation%Temperature%VariableUnit//"]"
                write (self%Observation%Temperature%numUnit, '(a)') "#"
                write (self%Observation%Temperature%numUnit, '(a,'//to_string(nObs)//'(2x,a))') &
                    "Time", ("Obs"//to_string(iObs), iObs=1, nObs)
            end if
        case ("Si")
            if (self%Observation%Si%numUnit < 0) then
                open (newunit=self%Observation%Si%numUnit, &
                      file=trim(adjustl(self%Observation%Si%Filename)), &
                      status='replace', &
                      action='write')
                !! Write file header
                write (self%Observation%Si%numUnit, '(a)') "# Ice content time varieation"
                write (self%Observation%Si%numUnit, '(a)') "#"
                select case (self%Observation%ObservationType)
                case (1)
                    write (self%Observation%Si%numUnit, '(a)') "# Observation Node ID"
                    do iObs = 1, nObs
                        write (self%Observation%Si%numUnit, '(a,i0,a,x,i0)') &
                            "# Node ID ", iObs, ":", self%Observation%ObsNodeID(iObs)
                    end do
                case (2)
                    write (self%Observation%Si%numUnit, '(a)') "# Observation Coordinate (x,y,z)"
                    do iObs = 1, nObs
                        write (self%Observation%Si%numUnit, '(a,x,i0,a,3(x,es18.11,a),a)') &
                            "# Point", iObs, ": (", &
                            self%Observation%Cood_Obs%x(iObs), ",", &
                            self%Observation%Cood_Obs%y(iObs), ",", &
                            self%Observation%Cood_Obs%z(iObs), &
                            ")"
                    end do
                end select
                write (self%Observation%Si%numUnit, '(a)') "#"
                write (self%Observation%Si%numUnit, '(a)') &
                    "# Output Unit: Time ["//trim(adjustl(self%Output_TimeUnit))//"], Si ["//self%Observation%Si%VariableUnit//"]"
                write (self%Observation%Si%numUnit, '(a)') "#"
                write (self%Observation%Si%numUnit, '(a,'//to_string(nObs)//'(2x,a))') &
                    "Time", ("Obs"//to_string(iObs), iObs=1, nObs)
            end if
        case ("TC")
            if (self%Observation%TC%numUnit < 0) then
                open (newunit=self%Observation%TC%numUnit, &
                      file=trim(adjustl(self%Observation%TC%Filename)), &
                      status='replace', &
                      action='write')
                !! Write file header
                write (self%Observation%TC%numUnit, '(a)') "# Thermal conductivity time varieation"
                write (self%Observation%TC%numUnit, '(a)') "#"
                select case (self%Observation%ObservationType)
                case (1)
                    write (self%Observation%TC%numUnit, '(a)') "# Observation Node ID"
                    do iObs = 1, nObs
                        write (self%Observation%TC%numUnit, '(a,i0,a,x,i0)') &
                            "# Node ID ", iObs, ":", self%Observation%ObsNodeID(iObs)
                    end do
                case (2)
                    write (self%Observation%TC%numUnit, '(a)') "# Observation Coordinate (x,y,z)"
                    do iObs = 1, nObs
                        write (self%Observation%TC%numUnit, '(a,x,i0,a,3(x,es18.11,a),a)') &
                            "# Point", iObs, ": (", &
                            self%Observation%Cood_Obs%x(iObs), ",", &
                            self%Observation%Cood_Obs%y(iObs), ",", &
                            self%Observation%Cood_Obs%z(iObs), &
                            ")"
                    end do
                end select
                write (self%Observation%TC%numUnit, '(a)') "#"
                write (self%Observation%TC%numUnit, '(a)') &
                    "# Output Unit: Time ["//trim(adjustl(self%Output_TimeUnit))//"], TC ["//self%Observation%TC%VariableUnit//"]"
                write (self%Observation%TC%numUnit, '(a)') "#"
                write (self%Observation%TC%numUnit, '(a,'//to_string(nObs)//'(2x,a))') &
                    "Time", ("Obs"//to_string(iObs), iObs=1, nObs)
            end if
        case ("C")
            if (self%Observation%C%numUnit < 0) then
                open (newunit=self%Observation%C%numUnit, &
                      file=trim(adjustl(self%Observation%C%Filename)), &
                      status='replace', &
                      action='write')
                !! Write file header
                write (self%Observation%C%numUnit, '(a)') "# Specific Heat time varieation"
                write (self%Observation%C%numUnit, '(a)') "#"
                select case (self%Observation%ObservationType)
                case (1)
                    write (self%Observation%C%numUnit, '(a)') "# Observation Node ID"
                    do iObs = 1, nObs
                        write (self%Observation%C%numUnit, '(a,i0,a,x,i0)') &
                            "# Node ID ", iObs, ":", self%Observation%ObsNodeID(iObs)
                    end do
                case (2)
                    write (self%Observation%C%numUnit, '(a)') "# Observation Coordinate (x,y,z)"
                    do iObs = 1, nObs
                        write (self%Observation%C%numUnit, '(a,x,i0,a,3(x,es18.11,a),a)') &
                            "# Point", iObs, ": (", &
                            self%Observation%Cood_Obs%x(iObs), ",", &
                            self%Observation%Cood_Obs%y(iObs), ",", &
                            self%Observation%Cood_Obs%z(iObs), &
                            ")"
                    end do
                end select
                write (self%Observation%C%numUnit, '(a)') "#"
                write (self%Observation%C%numUnit, '(a)') &
                    "# Output Unit: Time ["//trim(adjustl(self%Output_TimeUnit))//"], C ["//self%Observation%C%VariableUnit//"]"
                write (self%Observation%C%numUnit, '(a)') "#"
                write (self%Observation%C%numUnit, '(a,'//to_string(nObs)//'(2x,a))') &
                    "Time", ("Obs"//to_string(iObs), iObs=1, nObs)
            end if
        case ("Pressure")
            if (self%Observation%Pressure%numUnit < 0) then
                open (newunit=self%Observation%Pressure%numUnit, &
                      file=trim(adjustl(self%Observation%Pressure%Filename)), &
                      status='replace', &
                      action='write')
                !! Write file header
                write (self%Observation%Pressure%numUnit, '(a)') "# "//data_name//" time varieation"
                write (self%Observation%Pressure%numUnit, '(a)') "#"
                select case (self%Observation%ObservationType)
                case (1)
                    write (self%Observation%Pressure%numUnit, '(a)') "# Observation Node ID"
                    do iObs = 1, nObs
                        write (self%Observation%Pressure%numUnit, '(a,i0,a,x,i0)') &
                            "# Node ID ", iObs, ":", self%Observation%ObsNodeID(iObs)
                    end do
                case (2)
                    write (self%Observation%Pressure%numUnit, '(a)') "# Observation Coordinate (x,y,z)"
                    do iObs = 1, nObs
                        write (self%Observation%Pressure%numUnit, '(a,x,i0,a,3(x,es18.11,a),a)') &
                            "# Point", iObs, ": (", &
                            self%Observation%Cood_Obs%x(iObs), ",", &
                            self%Observation%Cood_Obs%y(iObs), ",", &
                            self%Observation%Cood_Obs%z(iObs), &
                            ")"
                    end do
                end select
                write (self%Observation%Pressure%numUnit, '(a)') "#"
                write (self%Observation%Pressure%numUnit, '(a)') &
                    "# Output Unit: Time ["//trim(adjustl(self%Output_TimeUnit))//"], Pressure ["//self%Observation%Pressure%VariableUnit//"]"
                write (self%Observation%Pressure%numUnit, '(a)') "#"
                write (self%Observation%Pressure%numUnit, '(a,'//to_string(nObs)//'(2x,a))') &
                    "Time", ("Obs"//to_string(iObs), iObs=1, nObs)
            end if
        case ("wFlux")
            if (self%Observation%wFlux%numUnit < 0) then
                open (newunit=self%Observation%wFlux%numUnit, &
                      file=trim(adjustl(self%Observation%wFlux%Filename)), &
                      status='replace', &
                      action='write')
                !! Write file header
                write (self%Observation%wFlux%numUnit, '(a)') "# Water flux time varieation"
                write (self%Observation%wFlux%numUnit, '(a)') "#"
                select case (self%Observation%ObservationType)
                case (1)
                    write (self%Observation%wFlux%numUnit, '(a)') "# Observation Node ID"
                    do iObs = 1, nObs
                        write (self%Observation%wFlux%numUnit, '(a,i0,a,x,i0)') &
                            "# Node ID ", iObs, ":", self%Observation%ObsNodeID(iObs)
                    end do
                case (2)
                    write (self%Observation%wFlux%numUnit, '(a)') "# Observation Coordinate (x,y,z)"
                    do iObs = 1, nObs
                        write (self%Observation%wFlux%numUnit, '(a,x,i0,a,3(x,es18.11,a),a)') &
                            "# Point", iObs, ": (", &
                            self%Observation%Cood_Obs%x(iObs), ",", &
                            self%Observation%Cood_Obs%y(iObs), ",", &
                            self%Observation%Cood_Obs%z(iObs), &
                            ")"
                    end do
                end select
                write (self%Observation%wFlux%numUnit, '(a)') "#"
                write (self%Observation%wFlux%numUnit, '(a)') &
                    "# Output Unit: Time ["//trim(adjustl(self%Output_TimeUnit))//"], wFlux ["//self%Observation%wFlux%VariableUnit//"]"
                write (self%Observation%wFlux%numUnit, '(a)') "#"
                write (self%Observation%wFlux%numUnit, '(a,'//to_string(nObs)//'(2x,a))') &
                    "Time", ("Obs"//to_string(iObs), iObs=1, nObs)
            end if
        case ("K")
            if (self%Observation%K%numUnit < 0) then
                open (newunit=self%Observation%K%numUnit, &
                      file=trim(adjustl(self%Observation%K%Filename)), &
                      status='replace', &
                      action='write')
                !! Write file header
                write (self%Observation%K%numUnit, '(a)') "# Hydraulic conductivity time varieation"
                write (self%Observation%K%numUnit, '(a)') "#"
                select case (self%Observation%ObservationType)
                case (1)
                    write (self%Observation%K%numUnit, '(a)') "# Observation Node ID"
                    do iObs = 1, nObs
                        write (self%Observation%K%numUnit, '(a,i0,a,x,i0)') &
                            "# Node ID ", iObs, ":", self%Observation%ObsNodeID(iObs)
                    end do
                case (2)
                    write (self%Observation%K%numUnit, '(a)') "# Observation Coordinate (x,y,z)"
                    do iObs = 1, nObs
                        write (self%Observation%K%numUnit, '(a,x,i0,a,3(x,es18.11,a),a)') &
                            "# Point", iObs, ": (", &
                            self%Observation%Cood_Obs%x(iObs), ",", &
                            self%Observation%Cood_Obs%y(iObs), ",", &
                            self%Observation%Cood_Obs%z(iObs), &
                            ")"
                    end do
                end select
                write (self%Observation%K%numUnit, '(a)') "#"
                write (self%Observation%K%numUnit, '(a)') &
                    "# Output Unit: Time ["//trim(adjustl(self%Output_TimeUnit))//"], K ["//self%Observation%K%VariableUnit//"]"
                write (self%Observation%K%numUnit, '(a)') "#"
                write (self%Observation%K%numUnit, '(a,'//to_string(nObs)//'(2x,a))') &
                    "Time", ("Obs"//to_string(iObs), iObs=1, nObs)
            end if
        end select
    end subroutine Inout_Output_Observation_Header

end module Inout_Output
