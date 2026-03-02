submodule(io_output_overall) output_overall_vtk
    implicit none

    interface write_field
        module procedure :: output_vtk_points_scalar_real64
        module procedure :: output_vtk_points_scalar_int32
        module procedure :: output_vtk_vector_real64
        module procedure :: output_vtk_vector_int32
    end interface

    interface write_cell
        module procedure :: output_vtk_cell_real64
        module procedure :: output_vtk_cell_int32
    end interface

contains

    module subroutine initialize_output_overall_vtk(self, input, domain)
        implicit none
        class(type_output_overall), intent(inout) :: self
        type(type_input), intent(in) :: input
        type(type_domain), intent(inout) :: domain

        integer(int32) :: i, j, idx, total

        self%vtk%num_points = input%geometry%vtk%num_points
        self%vtk%num_cells = input%geometry%vtk%num_total_cells
        call self%vtk%coordinate%initialize(self%vtk%num_points)
        self%vtk%coordinate = input%geometry%vtk%POINTS

        call allocate_array(self%vtk%offsets, self%vtk%num_cells)
        call allocate_array(self%vtk%cell_types, self%vtk%num_cells)

        do i = 1, self%vtk%num_cells
            self%vtk%offsets(i) = input%geometry%vtk%CELLS(i)%num_nodes_in_cell
            self%vtk%cell_types(i) = input%geometry%vtk%CELLS(i)%cell_type
        end do
        total = sum(self%vtk%offsets(:))

        call allocate_array(self%vtk%connectivities, total)
        idx = 0
        do i = 1, self%vtk%num_cells
            do j = 1, input%geometry%vtk%CELLS(i)%num_nodes_in_cell
                idx = idx + 1
                self%vtk%connectivities(idx) = input%geometry%vtk%CELLS(i)%connectivity(j) - 1
            end do
        end do

        if (associated(self%write_fields)) nullify (self%write_fields)
        self%write_fields => output_overall_vtk_fields

        if (associated(self%write_cell)) nullify (self%write_cell)
        self%write_cell => output_overall_vtk_cell

    end subroutine initialize_output_overall_vtk

    subroutine output_overall_vtk_fields(self, file_counts, domain, porosity, temperature, si, pressure, water_flux)
        implicit none
        class(type_output_overall), intent(inout) :: self
        integer(int32), intent(in) :: file_counts
        type(type_domain), intent(in) :: domain
        real(real64), intent(in), optional :: porosity(:)
        real(real64), intent(in), optional :: temperature(:)
        real(real64), intent(in), optional :: si(:)
        real(real64), intent(in), optional :: pressure(:)
        type(type_coordinate_array_dp), intent(in), optional :: water_flux

        integer(int32) :: status
        integer(int32) :: unit_num
        integer(int32) :: iN, iE, idx, i

        character(256) :: output_name

        write (output_name, self%format_output) trim(self%dir_output_field), "Out_", file_counts, self%file_extension
        open (newunit=unit_num, file=output_name, status='replace', action='write', iostat=status)
        if (status /= 0) call raise_error(ERROR_CODES%OPEN_FILE_FAILED, opt=output_name)

        write (unit_num, '(a)') "# vtk DataFile Version 2.0"
        write (unit_num, '(a)') "Analysis ASCII VTK file"
        write (unit_num, '(a)') "ASCII"
        write (unit_num, '(a)') "DATASET UNSTRUCTURED_GRID"
        write (unit_num, '(a,i0,a)') "POINTS ", self%vtk%num_points, " double"

        do iN = 1, self%vtk%num_points
            write (unit_num, '(3(es22.15,x))') self%vtk%coordinate%x(iN), self%vtk%coordinate%y(iN), self%vtk%coordinate%z(iN)
        end do
        write (unit_num, '(a)') ""

        write (unit_num, '(a,i0,x,i0,a)') "CELLS ", self%vtk%num_cells, sum(self%vtk%offsets(:)) + self%vtk%num_cells
        idx = 1
        do iE = 1, self%vtk%num_cells
            write (unit_num, '(i0,'//to_string(self%vtk%offsets(iE))//'(x,i0))') self%vtk%offsets(iE), &
                self%vtk%connectivities(idx:idx + self%vtk%offsets(iE) - 1)
            idx = idx + self%vtk%offsets(iE)
        end do
        write (unit_num, '(a)') ""

        write (unit_num, '(a,i0)') "CELL_TYPES ", self%vtk%num_cells
        write (unit_num, '(i0)') self%vtk%cell_types(:)
        write (unit_num, '(a)') ""

        do i = 1, size(self%variable_names)
            if (i == 1) write (unit_num, '(a, i0)') "POINT_DATA ", self%vtk%num_points
            select case (self%variable_names(i))
            case ("temperature")
                if (present(temperature)) call write_field(unit_num, "Temperature", temperature)
            case ("ice_saturation")
                if (present(si)) call write_field(unit_num, "Si", si)
            case ("thermal_conductivity")
                print *, "Warning: 'thermal_conductivity' is not implemented in VTK output."
            case ("volumetric_heat_capacity")
                print *, "Warning: 'volumetric_heat_capacity' is not implemented in VTK output."
            case ("pressure")
                if (present(pressure)) call write_field(unit_num, "Pressure", pressure)
            case ("water_flux")
                if (present(water_flux)) call write_field(unit_num, "waterFlux", water_flux%x, water_flux%y, water_flux%z)
            case ("hydraulic_conductivity")
                print *, "Warning: 'hydraulic_conductivity' is not implemented in VTK output."
            end select
        end do

        close (unit_num)

    end subroutine output_overall_vtk_fields

    subroutine output_overall_vtk_cell(self, file_name, variable_name, variable)
        implicit none
        class(type_output_overall), intent(inout) :: self
        character(*), intent(in) :: file_name
        character(*), intent(in) :: variable_name
        integer(int32), intent(in) :: variable(:)

        integer(int32) :: status
        integer(int32) :: unit_num
        integer(int32) :: iN, iE, idx, i

        open (newunit=unit_num, file=trim(self%dir_output_field)//trim(file_name)//trim(self%file_extension), &
              status='replace', action='write', iostat=status)
        if (status /= 0) call raise_error(ERROR_CODES%OPEN_FILE_FAILED, &
                                          opt=trim(self%dir_output_field)//trim(file_name)//trim(self%file_extension))

        write (unit_num, '(a)') "# vtk DataFile Version 2.0"
        write (unit_num, '(a)') "Analysis ASCII VTK file"
        write (unit_num, '(a)') "ASCII"
        write (unit_num, '(a)') "DATASET UNSTRUCTURED_GRID"
        write (unit_num, '(a,i0,a)') "POINTS ", self%vtk%num_points, " double"

        do iN = 1, self%vtk%num_points
            write (unit_num, '(3(es22.15,x))') self%vtk%coordinate%x(iN), self%vtk%coordinate%y(iN), self%vtk%coordinate%z(iN)
        end do
        write (unit_num, '(a)') ""

        write (unit_num, '(a,i0,x,i0,a)') "CELLS ", self%vtk%num_cells, sum(self%vtk%offsets(:)) + self%vtk%num_cells
        idx = 1
        do iE = 1, self%vtk%num_cells
            write (unit_num, '(i0,'//to_string(self%vtk%offsets(iE))//'(x,i0))') self%vtk%offsets(iE), &
                self%vtk%connectivities(idx:idx + self%vtk%offsets(iE) - 1)
            idx = idx + self%vtk%offsets(iE)
        end do
        write (unit_num, '(a)') ""

        write (unit_num, '(a,i0)') "CELL_TYPES ", self%vtk%num_cells
        write (unit_num, '(i0)') self%vtk%cell_types(:)
        write (unit_num, '(a)') ""

        write (unit_num, '(a,i0)') "CELL_DATA ", self%vtk%num_cells
        call write_cell(unit_num, variable_name, variable)

        close (unit_num)

    end subroutine output_overall_vtk_cell

    subroutine output_vtk_points_scalar_real64(unit_num, data_name, x)
        implicit none
        integer(int32), intent(in) :: unit_num
        character(*), intent(in) :: data_name
        real(real64), intent(in) :: x(:)

        write (unit_num, '(3a)') "SCALARS ", trim(adjustl(data_name)), " double 1"
        write (unit_num, '(a)') "LOOKUP_TABLE default"
        write (unit_num, '(es22.15)') x(:)
        write (unit_num, '(a)') ""

    end subroutine output_vtk_points_scalar_real64

    subroutine output_vtk_points_scalar_int32(unit_num, data_name, x)
        implicit none
        integer(int32), intent(in) :: unit_num
        character(*), intent(in) :: data_name
        integer(int32), intent(in) :: x(:)

        write (unit_num, '(3a)') "SCALARS ", trim(adjustl(data_name)), " int 1"
        write (unit_num, '(a)') "LOOKUP_TABLE default"
        write (unit_num, '(i0)') x(:)
        write (unit_num, '(a)') ""

    end subroutine output_vtk_points_scalar_int32

    subroutine output_vtk_vector_real64(unit_num, data_name, x, y, z)
        implicit none
        integer(int32), intent(in) :: unit_num
        character(*), intent(in) :: data_name
        real(real64), intent(in) :: x(:), y(:), z(:)

        integer(int32) :: i

        write (unit_num, '(3a)') "VECTORS ", trim(adjustl(data_name)), " double"
        do i = 1, size(x)
            write (unit_num, '(3(es22.15,x))') x(i), y(i), z(i)
        end do
        write (unit_num, '(a)') ""

    end subroutine output_vtk_vector_real64

    subroutine output_vtk_vector_int32(unit_num, data_name, x, y, z)
        implicit none
        integer(int32), intent(in) :: unit_num
        character(*), intent(in) :: data_name
        integer(int32), intent(in) :: x(:), y(:), z(:)

        integer(int32) :: i

        write (unit_num, '(3a)') "VECTORS ", trim(adjustl(data_name)), " double"
        do i = 1, size(x)
            write (unit_num, '(3(i0,x))') x(i), y(i), z(i)
        end do
        write (unit_num, '(a)') ""

    end subroutine output_vtk_vector_int32

    subroutine output_vtk_cell_real64(unit_num, data_name, x)
        implicit none
        integer(int32), intent(in) :: unit_num
        character(*), intent(in) :: data_name
        real(real64), intent(in) :: x(:)

        write (unit_num, '(3a)') "SCALARS ", trim(adjustl(data_name)), " double 1"
        write (unit_num, '(a)') "LOOKUP_TABLE default"
        write (unit_num, '(es22.15)') x(:)
        write (unit_num, '(a)') ""

    end subroutine output_vtk_cell_real64

    subroutine output_vtk_cell_int32(unit_num, data_name, x)
        implicit none
        integer(int32), intent(in) :: unit_num
        character(*), intent(in) :: data_name
        integer(int32), intent(in) :: x(:)

        write (unit_num, '(3a)') "SCALARS ", trim(adjustl(data_name)), " double 1"
        write (unit_num, '(a)') "LOOKUP_TABLE default"
        write (unit_num, '(i0)') x(:)
        write (unit_num, '(a)') ""

    end subroutine output_vtk_cell_int32

end submodule output_overall_vtk
