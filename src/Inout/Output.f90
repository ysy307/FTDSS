module Inout_Output
    use, intrinsic :: iso_fortran_env, only: int8, int32, real64
    use :: Inout_SetProjectPath, only:GetProjectPath => Inout_SetProjectPath_GetProjectPath
    use :: error
    use :: Allocate_Allocate
    use :: Inout_Input
    use :: Types

    implicit none
    private

    type :: Output_Config
        logical(4) :: doOutput
        character(:), allocatable :: Filename
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
        ! private
        character(:), allocatable :: fextend

        type(Output_Config) :: Temperature
        type(Output_Config) :: Si
        type(Output_Config) :: TC
        type(Output_Config) :: C
        type(Output_Config) :: Pressure
        type(Output_Config) :: Flux
        type(Output_Config) :: K

        logical(4) :: doOutput_stdout

        character(:), allocatable :: dir_Output
        character(:), allocatable :: dir_FileOutput
        character(:), allocatable :: format_output

        type(Output_VTK_Series) :: VTKInfo

    contains
        procedure, private, pass(self) :: Inout_Output_All_vtu
        procedure, public, pass(self) :: Output_All => Inout_Output_All
        ! procedure :: Output_All => Inout_Output_All
        ! procedure :: Output_Observation => Inout_Output_Observation
    end type Type_Output

    interface Type_Output
        module procedure Output_Constructor
    end interface

    public :: Type_Output

contains

    function Output_Constructor(Structure_Input) result(Structure)
        implicit none
        type(Type_Input), intent(in) :: Structure_Input
        type(Type_Output) :: Structure
        character(256) :: dir_Path
        logical(4) :: exists

        character(:), allocatable :: command
        integer(int32) :: i, j
        integer(int32) :: total

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
            ! Windows:
            command = "del /Q "//'"'//trim(adjustl(Structure%dir_Output))//"*.dat"//'"'
            call system(command)
            command = "del /Q "//'"'//trim(adjustl(Structure%dir_Output))//"*.csv"//'"'
            call system(command)
#endif
#ifdef __linux__
            ! Linux: .datと.csvだけ削除
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
            ! Windows: .datと.csvだけ削除
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
            ! Linux: .datと.csvだけ削除
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

        Structure%Temperature%Filename = trim(adjustl(Structure%dir_Output))//"obsf_T.dat"
        Structure%Si%Filename = trim(adjustl(Structure%dir_Output))//"obsf_Si.dat"
        Structure%TC%Filename = trim(adjustl(Structure%dir_Output))//"obsf_TC.dat"
        Structure%C%Filename = trim(adjustl(Structure%dir_Output))//"obsf_C.dat"
        Structure%Pressure%Filename = trim(adjustl(Structure%dir_Output))//"obsf_P.dat"
        Structure%Flux%Filename = trim(adjustl(Structure%dir_Output))//"obsf_Flux.dat"
        Structure%K%Filename = trim(adjustl(Structure%dir_Output))//"obsf_K.dat"

        Structure%Temperature%doOutput = Structure_Input%OutputSettings%outTemp
        Structure%Si%doOutput = Structure_Input%OutputSettings%outSi
        Structure%TC%doOutput = Structure_Input%OutputSettings%outTC
        Structure%C%doOutput = Structure_Input%OutputSettings%outC
        Structure%Pressure%doOutput = Structure_Input%OutputSettings%outPres
        Structure%Flux%doOutput = Structure_Input%OutputSettings%outFlux
        Structure%K%doOutput = Structure_Input%OutputSettings%outK

        Structure%fextend = "."//trim(adjustl(Structure_Input%OutputSettings%FileFormat))

        Structure%doOutput_stdout = Structure_Input%Basic%shouldDisplayPrompt

        Structure%format_output = '(a,a,i5.5,a)'

        select case (Structure%fextend)
        case (".vtk", ".vtu")
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
            total = Structure%VTKInfo%offset(Structure%VTKInfo%nCell) + Structure_Input%VTK%CELLS(Structure%VTKInfo%nCell)%offset
            call Allocate_Array(Structure%VTKInfo%connectivity, total)
            do i = 1, Structure%VTKInfo%nCell
                if (i == 1) then
                    do j = 1, Structure%VTKInfo%offset(i)
                        Structure%VTKInfo%connectivity(j) = Structure_Input%VTK%CELLS(i)%connectivity(j) - 1
                    end do
                else
                    do j = 1, Structure%VTKInfo%offset(i) - Structure%VTKInfo%offset(i - 1)
                        Structure%VTKInfo%connectivity(j + Structure%VTKInfo%offset(i - 1)) = &
                            Structure_Input%VTK%CELLS(i)%connectivity(j) - 1
                    end do
                end if
            end do
        end select

    end function Output_Constructor

    subroutine Inout_Output_All(self, fc, Temp, Si, Pres, wFlux)
        use vtk_fortran, only: vtk_file
        implicit none
        class(Type_Output) :: self
        integer(int32), intent(in) :: fc

        real(real64), intent(in), optional :: Temp(:)
        real(real64), intent(in), optional :: Si(:)
        real(real64), intent(in), optional :: Pres(:)
        type(DP3d), intent(in), optional :: wFlux

        select case (trim(adjustl(self%fextend)))
        case (".vtu")
            call self%Inout_Output_All_vtu(fc=fc, Temp=Temp, Si=Si, Pres=Pres, wFlux=wFlux)
        end select

    end subroutine Inout_Output_All

    subroutine Inout_Output_All_vtu(self, fc, Temp, Si, Pres, wFlux)
        use vtk_fortran, only: vtk_file
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
        status = vtu%initialize(format='ascii', filename=outName, mesh_topology='UnstructuredGrid')

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
end module Inout_Output
