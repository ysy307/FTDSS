submodule(input_output) input_output_overall_vtu
    implicit none

contains
    module subroutine initialize_output_overall_vtu(self, Input, Coordinate, Domain)
        implicit none
        class(type_output_overall), intent(inout) :: self
        type(Type_Input), intent(in) :: Input
        type(type_dp_3d), intent(in) :: Coordinate
        type(type_domain), intent(inout) :: Domain

        integer(int32) :: i, j
        integer(int32) :: total_connectivity_size, current_offset, start_index

        self%vtk%num_points = input%geometry%vtk%num_points
        self%vtk%num_cells = input%geometry%vtk%num_total_cells
        call self%vtk%coordinate%initialize(self%vtk%num_points)
        self%vtk%coordinate = input%geometry%vtk%POINTS

        ! --- offset と CellType 配列を確保 ---
        call allocate_array(self%vtk%offsets, self%vtk%num_cells)
        call allocate_array(self%vtk%cell_types, self%vtk%num_cells)

        ! ★★★ 修正箇所: offset配列を累積和として正しく計算 ★★★
        current_offset = 0
        do i = 1, self%vtk%num_cells
            self%vtk%cell_types(i) = input%geometry%vtk%CELLS(i)%cell_type
            current_offset = current_offset + input%geometry%vtk%CELLS(i)%num_nodes_in_cell
            self%vtk%offsets(i) = current_offset
        end do

        ! --- connectivity配列を正しい合計サイズで確保 ---
        if (self%vtk%num_cells > 0) then
            total_connectivity_size = self%vtk%offsets(self%vtk%num_cells)
        else
            total_connectivity_size = 0
        end if
        call allocate_array(self%VTK%connectivities, total_connectivity_size)

        ! ★★★ 修正箇所: 正しいオフセットを用いてconnectivity配列にデータを格納 ★★★
        do i = 1, self%vtk%num_cells
            ! 各セルの書き込み開始インデックスを計算
            if (i == 1) then
                start_index = 0
            else
                start_index = self%vtk%offsets(i - 1)
            end if

            ! 各セルの接続情報をコピー (VTKは0-based indexなので-1する)
            do j = 1, input%geometry%vtk%CELLS(i)%num_nodes_in_cell
                self%VTK%connectivities(start_index + j) = input%geometry%vtk%CELLS(i)%connectivity(j) - 1
            end do
        end do

        if (associated(self%write_fields)) nullify (self%write_fields)
        self%write_fields => output_overall_vtu_fields

        if (associated(self%write_cell)) nullify (self%write_cell)
        self%write_cell => output_overall_vtu_cell

    end subroutine initialize_output_overall_vtu

    subroutine output_overall_vtu_fields(self, file_counts, domain, porosity, temperature, si, pressure, water_flux)
        implicit none
        class(type_output_overall), intent(inout) :: self
        integer(int32), intent(in) :: file_counts
        type(type_domain), intent(in) :: domain
        real(real64), intent(in), optional :: porosity(:)
        real(real64), intent(in), optional :: temperature(:)
        real(real64), intent(in), optional :: si(:)
        real(real64), intent(in), optional :: pressure(:)
        type(type_dp_3d), intent(in), optional :: water_flux

        type(vtk_file) :: vtu
        integer(int32) :: status
        integer(int32) :: unit_num

        real(real64), allocatable :: original(:), original_vector(:, :)

        integer(int32) :: nsize, i

        character(256) :: output_name

        ! Initialize VTK file
        write (output_name, self%format_output) trim(self%dir_output_field), "Out_", file_counts, self%file_extension

        status = vtu%initialize(format='binary', filename=trim(output_name), mesh_topology='UnstructuredGrid')

        ! Write data
        status = vtu%xml_writer%write_piece(np=self%vtk%num_points, &
                                            nc=self%vtk%num_cells)
        status = vtu%xml_writer%write_geo(np=self%vtk%num_points, &
                                          nc=self%vtk%num_cells, &
                                          x=self%vtk%coordinate%x, &
                                          y=self%vtk%coordinate%y, &
                                          z=self%vtk%coordinate%z)
        status = vtu%xml_writer%write_connectivity(nc=self%vtk%num_cells, &
                                                   connectivity=self%VTK%connectivities, &
                                                   offset=self%vtk%offsets, &
                                                   cell_type=self%vtk%cell_types)

        ! --- データセクション ---

        do i = 1, size(self%variable_names)
            if (i == 1) status = vtu%xml_writer%write_dataarray(location='node', action='open')
            select case (self%variable_names(i))
            case ("temperature")
                if (present(temperature)) then
                    call allocate_array(original, self%vtk%num_points)
                    call domain%reordering%to_original_value(temperature, original)
                    status = vtu%xml_writer%write_dataarray(data_name='Temperature', &
                                                            x=original)
                    call deallocate_array(original)
                end if
            case ("ice_saturation")
                if (present(si)) then
                    call allocate_array(original, self%vtk%num_points)
                    call domain%reordering%to_original_value(si, original)
                    status = vtu%xml_writer%write_dataarray(data_name='Si', &
                                                            x=original)
                    call deallocate_array(original)
                end if
            case ("thermal_conductivity")
                print *, "Warning: 'thermal_conductivity' is not implemented in VTK output."
            case ("volumetric_heat_capacity")
                print *, "Warning: 'volumetric_heat_capacity' is not implemented in VTK output."
            case ("pressure")
                if (present(pressure)) then
                    call allocate_array(original, self%vtk%num_points)
                    call domain%reordering%to_original_value(pressure, original)
                    status = vtu%xml_writer%write_dataarray(data_name='Pressure', &
                                                            x=original)
                    call deallocate_array(original)
                end if
            case ("water_flux")
                if (present(water_flux)) then
                    call allocate_array(original_vector, 3_int32, self%vtk%num_points)
                    call domain%reordering%to_original_value(water_flux%x, original_vector(:, 1))
                    call domain%reordering%to_original_value(water_flux%y, original_vector(:, 2))
                    call domain%reordering%to_original_value(water_flux%z, original_vector(:, 3))
                    status = vtu%xml_writer%write_dataarray(data_name='waterFlux', &
                                                            x=original_vector(:, 1), &
                                                            y=original_vector(:, 2), &
                                                            z=original_vector(:, 3))
                    call deallocate_array(original_vector)
                end if
            case ("hydraulic_conductivity")
                print *, "Warning: 'hydraulic_conductivity' is not implemented in VTK output."
            end select

        end do
        status = vtu%xml_writer%write_dataarray(location='node', action='close')
        status = vtu%xml_writer%write_piece()

        ! Finalize VTK file
        status = vtu%finalize()

    end subroutine output_overall_vtu_fields

    subroutine output_overall_vtu_cell(self, file_name, variable_name, variable)
        implicit none
        class(type_output_overall), intent(inout) :: self
        character(*), intent(in) :: file_name
        character(*), intent(in) :: variable_name
        integer(int32), intent(in) :: variable(:)

        type(vtk_file) :: vtu
        integer(int32) :: status
        integer(int32) :: iN, iE, idx, i

        status = vtu%initialize(format='binary', filename=trim(self%dir_output_field)//trim(file_name)//trim(self%file_extension), &
                                mesh_topology='UnstructuredGrid')

        ! Write data
        status = vtu%xml_writer%write_piece(np=self%vtk%num_points, &
                                            nc=self%vtk%num_cells)
        status = vtu%xml_writer%write_geo(np=self%vtk%num_points, &
                                          nc=self%vtk%num_cells, &
                                          x=self%vtk%coordinate%x, &
                                          y=self%vtk%coordinate%y, &
                                          z=self%vtk%coordinate%z)
        status = vtu%xml_writer%write_connectivity(nc=self%vtk%num_cells, &
                                                   connectivity=self%VTK%connectivities, &
                                                   offset=self%vtk%offsets, &
                                                   cell_type=self%vtk%cell_types)

        status = vtu%xml_writer%write_dataarray(location='cell', action='open')
        status = vtu%xml_writer%write_dataarray(data_name=variable_name, x=variable)
        status = vtu%xml_writer%write_dataarray(location='cell', action='close')
        status = vtu%xml_writer%write_piece()

        status = vtu%finalize()

    end subroutine output_overall_vtu_cell

end submodule input_output_overall_vtu
