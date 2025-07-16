submodule(input_output) input_output_overall_vtk
    implicit none

contains

    module subroutine initialize_output_overall_vtk(self, input, coordinate, domain)
        implicit none
        class(type_output_overall), intent(inout) :: self
        type(type_input), intent(in) :: input
        type(type_dp_3d), intent(in) :: coordinate
        type(type_domain), intent(inout) :: domain

        integer(int32) :: i, j, idx, total

        self%VTK%nPoints = input%VTK%num_points
        self%VTK%nCell = input%VTK%num_total_cells
        call self%VTK%coordinates%initialize(self%VTK%nPoints)
        self%VTK%coordinates = input%VTK%POINTS

        call Allocate_Array(self%VTK%offset, self%VTK%nCell)
        call Allocate_Array(self%VTK%CellType, self%VTK%nCell)

        do i = 1, self%VTK%nCell
            self%VTK%offset(i) = input%VTK%CELLS(i)%num_nodes_in_cell
            self%VTK%CellType(i) = input%VTK%CELLS(i)%cell_type
        end do
        total = sum(self%VTK%offset(:))

        call Allocate_Array(self%VTK%connectivity, total)
        idx = 0
        do i = 1, self%VTK%nCell
            do j = 1, input%VTK%CELLS(i)%num_nodes_in_cell
                idx = idx + 1
                self%VTK%connectivity(idx) = input%VTK%CELLS(i)%connectivity(j) - 1
            end do
        end do

    end subroutine initialize_input_type_output_overall_vtk

    module subroutine input_output_Overall_Output(self, fc, rcm, Temp, Si, Pres, wFlux, Colors)
        implicit none
        class(Output_Overall) :: self
        integer(int32), intent(in) :: fc
        type(type_rcm), intent(in), optional :: rcm
        real(real64), intent(in), optional :: Temp(:)
        real(real64), intent(in), optional :: Si(:)
        real(real64), intent(in), optional :: Pres(:)
        type(type_dp_3d), intent(in), optional :: wFlux
        integer(int32), intent(in), optional :: Colors(:)

        select case (trim(adjustl(self%fextend)))
        case (".vtk")
            call self%Output_vtk(fc=fc, Temp=Temp, Si=Si, Pres=Pres, wFlux=wFlux)
            ! call self%Output_vtk(fc=fc, iperm=rcm%iperm, Temp=Temp, Si=Si, Pres=Pres, wFlux=wFlux)
        case (".vtu")
            if (present(Colors)) then
                call self%Output_vtu(fc=fc, Colors=Colors)
            else
                call self%Output_vtu(fc=fc, rcm=rcm, Temp=Temp, Si=Si, Pres=Pres, wFlux=wFlux)
            end if
        end select

    end subroutine input_output_Overall_Output

    module subroutine input_output_Overall_Output_vtk(self, fc, iperm, Temp, Si, Pres, wFlux, Colors)
        use :: stdlib_strings, only:to_string
        implicit none
        class(Output_Overall), intent(inout) :: self
        integer(int32), intent(in) :: fc
        integer(int32), intent(in), optional :: iperm(:)
        real(real64), intent(in), optional :: Temp(:)
        real(real64), intent(in), optional :: Si(:)
        real(real64), intent(in), optional :: Pres(:)
        type(type_dp_3d), intent(in), optional :: wFlux
        integer(int32), intent(in), optional :: Colors(:)

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
            write (unit_num, '(3(es22.15,x))') self%VTK%coordinates%x(iN), self%VTK%coordinates%y(iN), self%VTK%coordinates%z(iN)
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
            call self%Output_vtk_scalar(iperm=iperm, &
                                        unit_num=unit_num, &
                                        data_name='Temperature', &
                                        x=Temp)
        end if
        if (present(Si)) then
            call self%Output_vtk_scalar(iperm=iperm, &
                                        unit_num=unit_num, &
                                        data_name='Si', &
                                        x=Si)
        end if
        if (present(Pres)) then
            call self%Output_vtk_scalar(iperm=iperm, &
                                        unit_num=unit_num, &
                                        data_name='Pressure', &
                                        x=Pres)
        end if
        if (present(wFlux)) then
            call self%Output_vtk_vector(iperm=iperm, &
                                        unit_num=unit_num, &
                                        data_name='waterFlux', &
                                        x=wFlux%x, &
                                        y=wFlux%y, &
                                        z=wFlux%z)
        end if

    end subroutine input_output_Overall_Output_vtk

    module subroutine input_output_Overall_Output_vtk_scalar_real64(self, iperm, unit_num, data_name, x)
        implicit none
        class(Output_Overall) :: self
        integer(int32), intent(in), optional :: iperm(:)
        integer(int32), intent(in) :: unit_num
        character(*), intent(in) :: data_name
        real(real64), intent(in) :: x(:)

        real(real64), allocatable :: Original(:)
        integer(int32) :: status

        call Allocate_Array(Original, self%VTK%nPoints)
        ! call rcm
        ! call Reorder_to_Original(x, Original, iperm, status)

        write (unit_num, '(3a)') "SCALARS ", trim(adjustl(data_name)), " double 1"
        write (unit_num, '(a)') "LOOKUP_TABLE default"
        write (unit_num, '(es22.15)') Original(:)
        write (unit_num, '(a)') ""

        deallocate (Original)

    end subroutine input_output_Overall_Output_vtk_scalar_real64

    module subroutine input_output_Overall_Output_vtk_scalar_int32(self, iperm, unit_num, data_name, x)
        implicit none
        class(Output_Overall) :: self
        integer(int32), intent(in), optional :: iperm(:)
        integer(int32), intent(in) :: unit_num
        character(*), intent(in) :: data_name
        integer(int32), intent(in) :: x(:)

        integer(int32), allocatable :: Original(:)
        integer(int32) :: status

        call Allocate_Array(Original, self%VTK%nPoints)
        ! call Reorder_to_Original(x, Original, iperm, status)

        write (unit_num, '(3a)') "SCALARS ", trim(adjustl(data_name)), " int 1"
        write (unit_num, '(a)') "LOOKUP_TABLE default"
        write (unit_num, '(es22.15)') Original(:)
        write (unit_num, '(a)') ""

        deallocate (Original)

    end subroutine input_output_Overall_Output_vtk_scalar_int32

    module subroutine input_output_Overall_Output_vtk_vector(self, iperm, unit_num, data_name, x, y, z)
        implicit none
        class(Output_Overall) :: self
        integer(int32), intent(in), optional :: iperm(:)
        integer(int32), intent(in) :: unit_num
        character(*), intent(in) :: data_name
        real(real64), intent(in) :: x(:), y(:), z(:)

        real(real64), allocatable :: Original_x(:), Original_y(:), Original_z(:)
        integer(int32) :: i
        integer(int32) :: status

        call Allocate_Array(Original_x, self%VTK%nPoints)
        call Allocate_Array(Original_y, self%VTK%nPoints)
        call Allocate_Array(Original_z, self%VTK%nPoints)

        ! call Reorder_to_Original(x, Original_x, iperm, status)
        ! call Reorder_to_Original(y, Original_y, iperm, status)
        ! call Reorder_to_Original(z, Original_z, iperm, status)

        write (unit_num, '(3a)') "VECTORS ", trim(adjustl(data_name)), " double"
        do i = 1, self%VTK%nPoints
            write (unit_num, '(3(es22.15,x))') Original_x(i), Original_y(i), Original_z(i)
        end do
        write (unit_num, '(a)') ""

    end subroutine input_output_Overall_Output_vtk_vector

    module subroutine input_output_Overall_Output_vtu(self, fc, rcm, Temp, Si, Pres, wFlux, Colors)
        use :: vtk_fortran, only:vtk_file
        implicit none
        class(Output_Overall), intent(inout) :: self
        integer(int32), intent(in) :: fc
        type(type_rcm), intent(in), optional :: rcm
        real(real64), intent(in), optional :: Temp(:)
        real(real64), intent(in), optional :: Si(:)
        real(real64), intent(in), optional :: Pres(:)
        type(type_dp_3d), intent(in), optional :: wFlux
        integer(int32), intent(in), optional :: Colors(:)

        type(vtk_file) :: vtu
        integer(int32) :: status

        real(real64), allocatable :: Original(:), Original_vector(:, :)
        integer(int32), allocatable :: Cell_add_Colors(:)
        integer(int32) :: nsize

        character(256) :: outName

        call Allocate_Array(Original, self%VTK%nPoints)
        call Allocate_Array(Original_vector, 3_int32, self%VTK%nPoints)

        ! Initialize VTK file
        if (present(Colors)) then
            write (outName, '(3a)') trim(self%dir_FileOutput), "Coloring", self%fextend
        else
            write (outName, self%format_output) trim(self%dir_FileOutput), "Out_", fc, self%fextend
        end if
        status = vtu%initialize(format='binary', filename=trim(outName), mesh_topology='UnstructuredGrid')

        ! Write data
        status = vtu%xml_writer%write_piece(np=self%VTK%nPoints, &
                                            nc=self%VTK%nCell)
        status = vtu%xml_writer%write_geo(np=self%VTK%nPoints, &
                                          nc=self%VTK%nCell, &
                                          x=self%VTK%coordinates%x, &
                                          y=self%VTK%coordinates%y, &
                                          z=self%VTK%coordinates%z)
        status = vtu%xml_writer%write_connectivity(nc=self%VTK%nCell, &
                                                   connectivity=self%VTK%connectivity, &
                                                   offset=self%VTK%offset, &
                                                   cell_type=self%VTK%CellType)

        ! --- データセクション ---
        if (present(Colors)) then
            call Allocate_Array(Cell_add_Colors, self%VTK%nCell)
            nsize = size(Colors)
            Cell_add_Colors(:) = 0
            Cell_add_Colors(1:self%VTK%nCell - nsize) = 0
            Cell_add_Colors(self%VTK%nCell - nsize + 1:self%VTK%nCell) = Colors(:)
            status = vtu%xml_writer%write_dataarray(location='cell', action='open')
            status = vtu%xml_writer%write_dataarray(data_name='Colors', x=Cell_add_Colors)
            status = vtu%xml_writer%write_dataarray(location='cell', action='close')
            deallocate (Cell_add_Colors)
        else
            status = vtu%xml_writer%write_dataarray(location='node', action='open')
            if (present(Temp)) then
                call rcm%reorder_to_original(Temp, Original)
                status = vtu%xml_writer%write_dataarray(data_name='Temperature', &
                                                        x=Original)
            end if
            if (present(Si)) then
                call rcm%reorder_to_original(Si, Original)
                status = vtu%xml_writer%write_dataarray(data_name='Si', &
                                                        x=Original)
            end if
            if (present(Pres)) then
                call rcm%reorder_to_original(Pres, Original)
                status = vtu%xml_writer%write_dataarray(data_name='Pressure', &
                                                        x=Original)
            end if
            if (present(wFlux)) then
                call rcm%reorder_to_original(wFlux%x, Original_vector(:, 1))
                call rcm%reorder_to_original(wFlux%y, Original_vector(:, 2))
                call rcm%reorder_to_original(wFlux%z, Original_vector(:, 3))
                status = vtu%xml_writer%write_dataarray(data_name='waterFlux', &
                                                        x=Original_vector(:, 1), &
                                                        y=Original_vector(:, 2), &
                                                        z=Original_vector(:, 3))
            end if
            status = vtu%xml_writer%write_dataarray(location='node', action='close')
        end if
        status = vtu%xml_writer%write_piece()

        ! Finalize VTK file
        status = vtu%finalize()

    end subroutine input_output_Overall_Output_vtu

end submodule input_output_overall_vtk
