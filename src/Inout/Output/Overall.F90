submodule(Inout_Output) Inout_Output_Overall
    implicit none

contains
    module subroutine Inout_Output_Overall_initialize(self, Input, Coordinate, Domain)
        implicit none
        class(Output_Overall), intent(inout) :: self
        type(Type_Input), intent(in) :: Input
        type(DP3d), intent(in) :: Coordinate
        type(Domain_t), intent(in) :: Domain

        select case (self%fextend)
        case (".vtk")
            call self%initialize_vtk(Input, Coordinate, Domain)
        case (".vtu")
            call self%initialize_vtu(Input, Coordinate, Domain)
        case default
            write (*, '(a)') "Error: Unsupported file format for Overall output."
            stop
        end select

    end subroutine Inout_Output_Overall_initialize

    module subroutine Inout_Output_Overall_initialize_vtk(self, Input, Coordinate, Domain)
        implicit none
        class(Output_Overall), intent(inout) :: self
        type(Type_Input), intent(in) :: Input
        type(DP3d), intent(in) :: Coordinate
        type(Domain_t), intent(in) :: Domain

        integer(int32) :: i, j, idx, total

        self%VTK%nPoints = Input%VTK%numPoints
        self%VTK%nCell = Input%VTK%numTotalCells
        call self%VTK%Coordinates%allocate(self%VTK%nPoints)
        self%VTK%Coordinates = Input%VTK%POINTS

        call Allocate_Array(self%VTK%offset, self%VTK%nCell)
        call Allocate_Array(self%VTK%CellType, self%VTK%nCell)

        do i = 1, self%VTK%nCell
            self%VTK%offset(i) = Input%VTK%CELLS(i)%offset
            self%VTK%CellType(i) = Input%VTK%CELLS(i)%CellType
        end do
        total = sum(self%VTK%offset(:))

        call Allocate_Array(self%VTK%connectivity, total)
        idx = 0
        do i = 1, self%VTK%nCell
            do j = 1, Input%VTK%CELLS(i)%offset
                idx = idx + 1
                self%VTK%connectivity(idx) = &
                    Input%VTK%CELLS(i)%connectivity(j) - 1
            end do
        end do

    end subroutine Inout_Output_Overall_initialize_vtk

    module subroutine Inout_Output_Overall_initialize_vtu(self, Input, Coordinate, Domain)
        implicit none
        class(Output_Overall), intent(inout) :: self
        type(Type_Input), intent(in) :: Input
        type(DP3d), intent(in) :: Coordinate
        type(Domain_t), intent(in) :: Domain

        integer(int32) :: i, j, idx, total

        self%VTK%nPoints = Input%VTK%numPoints
        self%VTK%nCell = Input%VTK%numTotalCells
        call self%VTK%Coordinates%allocate(self%VTK%nPoints)
        self%VTK%Coordinates = Input%VTK%POINTS

        call Allocate_Array(self%VTK%offset, self%VTK%nCell)
        call Allocate_Array(self%VTK%CellType, self%VTK%nCell)

        do i = 1, self%VTK%nCell
            if (i == 1) then
                self%VTK%offset(i) = Input%VTK%CELLS(i)%offset
            else
                self%VTK%offset(i) = self%VTK%offset(i - 1) + &
                                     Input%VTK%CELLS(i)%offset
            end if
            self%VTK%CellType(i) = Input%VTK%CELLS(i)%CellType
        end do
        total = self%VTK%offset(self%VTK%nCell)

        call Allocate_Array(self%VTK%connectivity, total)
        do i = 1, self%VTK%nCell
            if (i == 1) then
                do j = 1, Input%VTK%CELLS(i)%offset
                    self%VTK%connectivity(j) = Input%VTK%CELLS(i)%connectivity(j) - 1
                end do
            else
                do j = 1, Input%VTK%CELLS(i)%offset
                    self%VTK%connectivity(self%VTK%offset(i - 1) + j) = &
                        Input%VTK%CELLS(i)%connectivity(j) - 1
                end do
            end if
        end do

    end subroutine Inout_Output_Overall_initialize_vtu

    module subroutine Inout_Output_Overall_Output(self, fc, RCM_Perm, Temp, Si, Pres, wFlux)
        implicit none
        class(Output_Overall) :: self
        integer(int32), intent(in) :: fc
        integer(int32), intent(in), optional :: RCM_Perm(:)
        real(real64), intent(in), optional :: Temp(:)
        real(real64), intent(in), optional :: Si(:)
        real(real64), intent(in), optional :: Pres(:)
        type(DP3d), intent(in), optional :: wFlux

        select case (trim(adjustl(self%fextend)))
        case (".vtk")
            call self%Output_vtk(fc=fc, RCM_Perm=RCM_Perm, Temp=Temp, Si=Si, Pres=Pres, wFlux=wFlux)
        case (".vtu")
            call self%Output_vtu(fc=fc, RCM_Perm=RCM_Perm, Temp=Temp, Si=Si, Pres=Pres, wFlux=wFlux)
        end select

    end subroutine Inout_Output_Overall_Output

    module subroutine Inout_Output_Overall_Output_vtk(self, fc, RCM_Perm, Temp, Si, Pres, wFlux)
        use :: stdlib_strings, only:to_string
        implicit none
        class(Output_Overall), intent(inout) :: self
        integer(int32), intent(in) :: fc
        integer(int32), intent(in), optional :: RCM_Perm(:)
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
        write (unit_num, '(a,i0,a)') "POINTS ", self%VTK%nPoints, " double"

        do iN = 1, self%VTK%nPoints
            write (unit_num, '(3(es22.15,x))') self%VTK%Coordinates%x(iN), self%VTK%Coordinates%y(iN), self%VTK%Coordinates%z(iN)
        end do
        write (unit_num, '(a)') ""

        write (unit_num, '(a,i0,x,i0,a)') "CELLS ", self%VTK%nCell, sum(self%VTK%offset(:)) + self%VTK%nCell
        idx = 1
        do iE = 1, self%VTK%nCell
            write (unit_num, '(i0,'//to_string(self%VTK%offset(iE))//'(x,i0))') self%VTK%offset(iE), self%VTK%connectivity(idx:idx + self%VTK%offset(iE) - 1)
            idx = idx + self%VTK%offset(iE)
        end do
        write (unit_num, '(a)') ""

        write (unit_num, '(a,i0)') "CELL_TYPES ", self%VTK%nCell
        do iE = 1, self%VTK%nCell
            write (unit_num, '(i0)') self%VTK%CellType(iE)
        end do
        write (unit_num, '(a)') ""

        write (unit_num, '(a, i0)') "POINT_DATA ", self%VTK%nPoints
        if (present(Temp)) then
            call self%Output_vtk_scalar(RCM_Perm=RCM_Perm, &
                                        unit_num=unit_num, &
                                        data_name='Temperature', &
                                        x=Temp)
        end if
        if (present(Si)) then
            call self%Output_vtk_scalar(RCM_Perm=RCM_Perm, &
                                        unit_num=unit_num, &
                                        data_name='Si', &
                                        x=Si)
        end if
        if (present(Pres)) then
            call self%Output_vtk_scalar(RCM_Perm=RCM_Perm, &
                                        unit_num=unit_num, &
                                        data_name='Pressure', &
                                        x=Pres)
        end if
        if (present(wFlux)) then
            call self%Output_vtk_vector(RCM_Perm=RCM_Perm, &
                                        unit_num=unit_num, &
                                        data_name='waterFlux', &
                                        x=wFlux%x, &
                                        y=wFlux%y, &
                                        z=wFlux%z)
        end if

    end subroutine Inout_Output_Overall_Output_vtk

    module subroutine Inout_Output_Overall_Output_vtk_scalar(self, RCM_Perm, unit_num, data_name, x)
        implicit none
        class(Output_Overall) :: self
        integer(int32), intent(in), optional :: RCM_Perm(:)
        integer(int32), intent(in) :: unit_num
        character(*), intent(in) :: data_name
        real(real64), intent(in) :: x(:)

        real(real64), allocatable :: tmp(:)
        integer(int32) :: status

        call Allocate_Array(tmp, self%VTK%nPoints)
        call reorder_to_original(x, RCM_Perm, tmp, status)

        write (unit_num, '(3a)') "SCALARS ", trim(adjustl(data_name)), " double 1"
        write (unit_num, '(a)') "LOOKUP_TABLE default"
        write (unit_num, '(es22.15)') tmp(:)
        write (unit_num, '(a)') ""

        deallocate (tmp)

    end subroutine Inout_Output_Overall_Output_vtk_scalar

    module subroutine Inout_Output_Overall_Output_vtk_vector(self, RCM_Perm, unit_num, data_name, x, y, z)
        implicit none
        class(Output_Overall) :: self
        integer(int32), intent(in), optional :: RCM_Perm(:)
        integer(int32), intent(in) :: unit_num
        character(*), intent(in) :: data_name
        real(real64), intent(in) :: x(:), y(:), z(:)

        real(real64), allocatable :: tmp_x(:), tmp_y(:), tmp_z(:)
        integer(int32) :: i
        integer(int32) :: status

        call Allocate_Array(tmp_x, self%VTK%nPoints)
        call Allocate_Array(tmp_y, self%VTK%nPoints)
        call Allocate_Array(tmp_z, self%VTK%nPoints)

        call reorder_to_original(x, RCM_Perm, tmp_x, status)
        call reorder_to_original(y, RCM_Perm, tmp_y, status)
        call reorder_to_original(z, RCM_Perm, tmp_z, status)

        write (unit_num, '(3a)') "VECTORS ", trim(adjustl(data_name)), " double"
        do i = 1, self%VTK%nPoints
            write (unit_num, '(3(es22.15,x))') tmp_x(i), tmp_y(i), tmp_z(i)
        end do
        write (unit_num, '(a)') ""

    end subroutine Inout_Output_Overall_Output_vtk_vector

    module subroutine Inout_Output_Overall_Output_vtu(self, fc, RCM_Perm, Temp, Si, Pres, wFlux)
        use :: vtk_fortran, only:vtk_file
        implicit none
        class(Output_Overall), intent(inout) :: self
        integer(int32), intent(in) :: fc
        integer(int32), intent(in), optional :: RCM_Perm(:)
        real(real64), intent(in), optional :: Temp(:)
        real(real64), intent(in), optional :: Si(:)
        real(real64), intent(in), optional :: Pres(:)
        type(DP3d), intent(in), optional :: wFlux

        type(vtk_file) :: vtu
        integer(int32) :: status

        real(real64), allocatable :: tmp(:), tmp_vector(:, :)

        character(256) :: outName

        call Allocate_Array(tmp, self%VTK%nPoints)
        call Allocate_Array(tmp_vector, 3_int32, self%VTK%nPoints)

        ! Initialize VTK file
        write (outName, self%format_output) trim(self%dir_FileOutput), "Out_", fc, self%fextend
        status = vtu%initialize(format='binary', filename=trim(outName), mesh_topology='UnstructuredGrid')

        ! Write data
        status = vtu%xml_writer%write_piece(np=self%VTK%nPoints, &
                                            nc=self%VTK%nCell)
        status = vtu%xml_writer%write_geo(np=self%VTK%nPoints, &
                                          nc=self%VTK%nCell, &
                                          x=self%VTK%Coordinates%x, &
                                          y=self%VTK%Coordinates%y, &
                                          z=self%VTK%Coordinates%z)
        status = vtu%xml_writer%write_connectivity(nc=self%VTK%nCell, &
                                                   connectivity=self%VTK%connectivity, &
                                                   offset=self%VTK%offset, &
                                                   cell_type=self%VTK%CellType)
        status = vtu%xml_writer%write_dataarray(location='node', action='open')
        if (present(Temp)) then
            call reorder_to_original(Temp, RCM_Perm, tmp, status)
            status = vtu%xml_writer%write_dataarray(data_name='Temperature', &
                                                    x=tmp)
        end if
        if (present(Si)) then
            call reorder_to_original(Si, RCM_Perm, tmp, status)
            status = vtu%xml_writer%write_dataarray(data_name='Si', &
                                                    x=tmp)
        end if
        if (present(Pres)) then
            call reorder_to_original(Pres, RCM_Perm, tmp, status)
            status = vtu%xml_writer%write_dataarray(data_name='Pressure', &
                                                    x=tmp)
        end if
        if (present(wFlux)) then
            call reorder_to_original(wFlux%x, RCM_Perm, tmp_vector(:, 1), status)
            call reorder_to_original(wFlux%y, RCM_Perm, tmp_vector(:, 2), status)
            call reorder_to_original(wFlux%z, RCM_Perm, tmp_vector(:, 3), status)
            status = vtu%xml_writer%write_dataarray(data_name='waterFlux', &
                                                    x=tmp_vector(:, 1), &
                                                    y=tmp_vector(:, 2), &
                                                    z=tmp_vector(:, 3))
        end if
        status = vtu%xml_writer%write_dataarray(location='node', action='close')
        status = vtu%xml_writer%write_piece()

        ! Finalize VTK file
        status = vtu%finalize()

    end subroutine Inout_Output_Overall_Output_vtu

end submodule Inout_Output_Overall
