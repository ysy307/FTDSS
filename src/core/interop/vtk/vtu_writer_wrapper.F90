module core_vtk_vtu_writer_wrapper
    !> @brief ISO_C_BINDING interfaces and concrete Fortran type for the
    !>        VTK XML unstructured-grid writer.
    !>
    !> Provides:
    !>
    !> * Low-level C-binding interfaces mapping directly to the
    !>   @c c_vtu_writer_* functions in @c c_wrapper.cpp.
    !> * @c type_vtu_writer — concrete extension of @c abst_vtk_writer
    !>   that holds an opaque @c c_ptr handle to a C++ @c VtuWriter
    !>   object and delegates all operations through the C layer.
    !>
    !> **Calling chain**
    !> \[ \text{type\_vtu\_writer (Fortran)}
    !>    \xrightarrow{\text{ISO\_C\_BINDING}} \text{c\_vtu\_writer\_* (C wrapper)}
    !>    \rightarrow \text{VtuWriter (C++)} \]
    !>
    !> **Performance**
    !> Field arrays (scalar / vector) are passed directly to the C++
    !> layer without heap copies via @c vtkDoubleArray::SetArray().
    !> Connectivity is expected to be 0-based and is forwarded without
    !> temporary conversion arrays.
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: core_vtk_vtk_writer_base, only: abst_vtk_writer
    implicit none
    private

    public :: type_vtu_writer

    ! ================================================================
    ! ISO_C_BINDING interfaces — map to extern "C" functions in
    ! c_wrapper.cpp.  All array args use assumed-size (*) so that
    ! Fortran passes the address of the first element (consistent with
    ! C pointer semantics).
    ! ================================================================
    interface

        !> @brief Create a VtuWriter and bind it to an output file path.
        !> @param[in]  filename  Null-terminated output .vtu file path.
        !> @return     Opaque handle to the C++ VtuWriter object.
        function vtu_writer_create(filename) &
                result(handle) bind(c, name='c_vtu_writer_create')
            import :: c_ptr, c_char
            type(c_ptr) :: handle
            !> Null-terminated output file path.
            character(kind=c_char), intent(in) :: filename(*)
        end function vtu_writer_create

        !> @brief Set the mesh topology.
        subroutine vtu_writer_set_mesh( &
                handle,                 &
                num_points, points,     &
                num_cells, conn_size,   &
                connectivity, offsets,  &
                cell_types)             &
                bind(c, name='c_vtu_writer_set_mesh')
            import :: c_ptr, c_int, c_double
            !> Writer handle.
            type(c_ptr), value, intent(in) :: handle
            !> Number of mesh vertices.
            integer(c_int), value, intent(in) :: num_points
            !> Interleaved (x,y,z) coordinates; length = 3*num_points.
            real(c_double), intent(in) :: points(*)
            !> Number of cells.
            integer(c_int), value, intent(in) :: num_cells
            !> Total length of the connectivity array.
            integer(c_int), value, intent(in) :: conn_size
            !> 0-based flat connectivity; length = conn_size.
            integer(c_int), intent(in) :: connectivity(*)
            !> VTK XML per-cell end offsets; length = num_cells.
            integer(c_int), intent(in) :: offsets(*)
            !> VTK cell-type IDs; length = num_cells.
            integer(c_int), intent(in) :: cell_types(*)
        end subroutine vtu_writer_set_mesh

        !> @brief Attach a named scalar point-data array (float64).
        subroutine vtu_writer_add_scalar_point_data( &
                handle, name, num_points, data)        &
                bind(c, name='c_vtu_writer_add_scalar_point_data')
            import :: c_ptr, c_char, c_int, c_double
            !> Writer handle.
            type(c_ptr), value, intent(in) :: handle
            !> Null-terminated array name.
            character(kind=c_char), intent(in) :: name(*)
            !> Number of mesh vertices.
            integer(c_int), value, intent(in) :: num_points
            !> Scalar values; length = num_points.
            real(c_double), intent(in) :: data(*)
        end subroutine vtu_writer_add_scalar_point_data

        !> @brief Attach a named 3-component vector point-data array (float64).
        subroutine vtu_writer_add_vector_point_data( &
                handle, name, num_points, data)        &
                bind(c, name='c_vtu_writer_add_vector_point_data')
            import :: c_ptr, c_char, c_int, c_double
            !> Writer handle.
            type(c_ptr), value, intent(in) :: handle
            !> Null-terminated array name.
            character(kind=c_char), intent(in) :: name(*)
            !> Number of mesh vertices.
            integer(c_int), value, intent(in) :: num_points
            !> Interleaved (vx,vy,vz) components; length = 3*num_points.
            real(c_double), intent(in) :: data(*)
        end subroutine vtu_writer_add_vector_point_data

        !> @brief Attach a named scalar cell-data array (float64).
        subroutine vtu_writer_add_scalar_cell_data( &
                handle, name, num_cells, data)        &
                bind(c, name='c_vtu_writer_add_scalar_cell_data')
            import :: c_ptr, c_char, c_int, c_double
            !> Writer handle.
            type(c_ptr), value, intent(in) :: handle
            !> Null-terminated array name.
            character(kind=c_char), intent(in) :: name(*)
            !> Number of cells.
            integer(c_int), value, intent(in) :: num_cells
            !> Scalar values; length = num_cells.
            real(c_double), intent(in) :: data(*)
        end subroutine vtu_writer_add_scalar_cell_data

        !> @brief Attach a named 3-component vector cell-data array (float64).
        subroutine vtu_writer_add_vector_cell_data( &
                handle, name, num_cells, data)        &
                bind(c, name='c_vtu_writer_add_vector_cell_data')
            import :: c_ptr, c_char, c_int, c_double
            !> Writer handle.
            type(c_ptr), value, intent(in) :: handle
            !> Null-terminated array name.
            character(kind=c_char), intent(in) :: name(*)
            !> Number of cells.
            integer(c_int), value, intent(in) :: num_cells
            !> Interleaved (vx,vy,vz) components; length = 3*num_cells.
            real(c_double), intent(in) :: data(*)
        end subroutine vtu_writer_add_vector_cell_data

        !> @brief Flush all accumulated data to disk.
        subroutine vtu_writer_write(handle) &
                bind(c, name='c_vtu_writer_write')
            import :: c_ptr
            !> Writer handle.
            type(c_ptr), value, intent(in) :: handle
        end subroutine vtu_writer_write

        !> @brief Release all VTK resources and destroy the writer object.
        subroutine vtu_writer_destroy(handle) &
                bind(c, name='c_vtu_writer_destroy')
            import :: c_ptr
            !> Writer handle (invalid after this call).
            type(c_ptr), value, intent(in) :: handle
        end subroutine vtu_writer_destroy

    end interface

    ! ================================================================
    ! Concrete Fortran VTU writer type
    ! ================================================================

    !> @brief Concrete VTK XML unstructured-grid writer.
    !>
    !> Extends @c abst_vtk_writer and delegates all operations to the
    !> C++ @c VtuWriter class via the ISO_C_BINDING interface above.
    !>
    !> The type stores a single @c c_ptr handle; all VTK pipeline
    !> objects live on the C++ heap and are managed by @c VtuWriter
    !> via @c vtkSmartPointer.
    !>
    !> **Usage example**
    !> @code
    !> type(type_vtu_writer) :: w
    !> call w%initialize("output_001.vtu")
    !> call w%write_mesh(num_pts, pts, num_cells, conn_sz, conn, offs, types)
    !> call w%write_scalar_point_data("temperature", num_pts, T)
    !> call w%write()
    !> call w%finalize()
    !> @endcode
    type, extends(abst_vtk_writer) :: type_vtu_writer
        !> Opaque handle to the underlying C++ VtuWriter object.
        type(c_ptr) :: writer_handle = c_null_ptr
    contains
        ! --- Lifecycle ---
        !> Bind to an output file path and configure binary VTK XML output.
        procedure :: initialize => initialize_type_vtu_writer
        !> Transfer mesh topology (0-based connectivity expected).
        procedure :: write_mesh => write_mesh_type_vtu_writer
        !> Flush all accumulated data to disk.
        procedure :: write      => write_type_vtu_writer
        !> Release all VTK resources.
        procedure :: finalize   => finalize_type_vtu_writer
        ! --- Field attachment ---
        !> Attach a named scalar point-data array (real64).
        procedure :: write_scalar_point_data => write_scalar_point_data_type_vtu_writer
        !> Attach a named 3-component vector point-data array (real64).
        procedure :: write_vector_point_data => write_vector_point_data_type_vtu_writer
        !> Attach a named scalar cell-data array (real64).
        procedure :: write_scalar_cell_data  => write_scalar_cell_data_type_vtu_writer
        !> Attach a named 3-component vector cell-data array (real64).
        procedure :: write_vector_cell_data  => write_vector_cell_data_type_vtu_writer
        ! --- Automatic cleanup ---
        final :: auto_destroy_type_vtu_writer
    end type type_vtu_writer

contains

    ! ----------------------------------------------------------------
    ! initialize
    ! ----------------------------------------------------------------

    !> @brief Bind the writer to an output file path.
    !>
    !> Passes the null-terminated filename to the C layer, which
    !> creates a @c VtuWriter object and configures binary VTK XML
    !> output with raw (non-base64) appended data.
    !>
    !> - Arithmetic complexity: O(1)
    !> - Memory complexity: O(1)
    subroutine initialize_type_vtu_writer(self, filename)
        class(type_vtu_writer), intent(inout) :: self
        !> Path to the target .vtu output file.
        character(*), intent(in) :: filename

        self%writer_handle = vtu_writer_create(trim(filename)//c_null_char)
    end subroutine initialize_type_vtu_writer

    ! ----------------------------------------------------------------
    ! write_mesh
    ! ----------------------------------------------------------------

    !> @brief Transfer mesh topology to the C++ VtuWriter.
    !>
    !> Connectivity is expected to be 0-based and all arrays are passed
    !> directly to the C layer without temporary copies.
    !>
    !> - Arithmetic complexity: O(1)
    !> - Memory complexity: O(1)
    subroutine write_mesh_type_vtu_writer( &
            self, num_points, points,       &
            num_cells, conn_size,           &
            connectivity, offsets, cell_types)
        class(type_vtu_writer), intent(inout) :: self
        !> Number of mesh vertices.
        integer(c_int), intent(in) :: num_points
        !> Coordinates; shape = (3, num_points).
        real(c_double), intent(in) :: points(3, num_points)
        !> Number of cells.
        integer(c_int), intent(in) :: num_cells
        !> Total length of the connectivity array.
        integer(c_int), intent(in) :: conn_size
        !> 0-based flat connectivity; length = conn_size.
        integer(c_int), intent(in) :: connectivity(conn_size)
        !> VTK XML per-cell end offsets; length = num_cells.
        integer(c_int), intent(in) :: offsets(num_cells)
        !> VTK cell-type IDs; length = num_cells.
        integer(c_int), intent(in) :: cell_types(num_cells)

        call vtu_writer_set_mesh(self%writer_handle,  &
            num_points, points,                        &
            num_cells, conn_size,                      &
            connectivity, offsets, cell_types)
    end subroutine write_mesh_type_vtu_writer

    ! ----------------------------------------------------------------
    ! write_scalar_point_data
    ! ----------------------------------------------------------------

    !> @brief Attach a named scalar point-data array (real64).
    !>
    !> The Fortran array is passed zero-copy to the C++ layer.
    !>
    !> - Arithmetic complexity: O(1)
    !> - Memory complexity: O(1)
    subroutine write_scalar_point_data_type_vtu_writer( &
            self, name, num_points, data)
        class(type_vtu_writer), intent(inout) :: self
        !> Array name written to the VTU file.
        character(*), intent(in) :: name
        !> Number of mesh vertices.
        integer(c_int), intent(in) :: num_points
        !> Scalar values; length = num_points.
        real(c_double), intent(in) :: data(num_points)

        call vtu_writer_add_scalar_point_data( &
            self%writer_handle,                 &
            trim(name)//c_null_char,            &
            num_points, data)
    end subroutine write_scalar_point_data_type_vtu_writer

    ! ----------------------------------------------------------------
    ! write_vector_point_data
    ! ----------------------------------------------------------------

    !> @brief Attach a named 3-component vector point-data array (real64).
    !>
    !> The Fortran column-major layout for @c data(3,num_points) maps
    !> directly to the interleaved (vx,vy,vz) layout expected by VTK.
    !>
    !> - Arithmetic complexity: O(1)
    !> - Memory complexity: O(1)
    subroutine write_vector_point_data_type_vtu_writer( &
            self, name, num_points, data)
        class(type_vtu_writer), intent(inout) :: self
        !> Array name written to the VTU file.
        character(*), intent(in) :: name
        !> Number of mesh vertices.
        integer(c_int), intent(in) :: num_points
        !> Interleaved (vx,vy,vz) components; shape = (3, num_points).
        real(c_double), intent(in) :: data(3, num_points)

        call vtu_writer_add_vector_point_data( &
            self%writer_handle,                 &
            trim(name)//c_null_char,            &
            num_points, data)
    end subroutine write_vector_point_data_type_vtu_writer

    ! ----------------------------------------------------------------
    ! write_scalar_cell_data
    ! ----------------------------------------------------------------

    !> @brief Attach a named scalar cell-data array (real64).
    !>
    !> - Arithmetic complexity: O(1)
    !> - Memory complexity: O(1)
    subroutine write_scalar_cell_data_type_vtu_writer( &
            self, name, num_cells, data)
        class(type_vtu_writer), intent(inout) :: self
        !> Array name written to the VTU file.
        character(*), intent(in) :: name
        !> Number of cells.
        integer(c_int), intent(in) :: num_cells
        !> Scalar values; length = num_cells.
        real(c_double), intent(in) :: data(num_cells)

        call vtu_writer_add_scalar_cell_data( &
            self%writer_handle,                &
            trim(name)//c_null_char,           &
            num_cells, data)
    end subroutine write_scalar_cell_data_type_vtu_writer

    ! ----------------------------------------------------------------
    ! write_vector_cell_data
    ! ----------------------------------------------------------------

    !> @brief Attach a named 3-component vector cell-data array (real64).
    !>
    !> - Arithmetic complexity: O(1)
    !> - Memory complexity: O(1)
    subroutine write_vector_cell_data_type_vtu_writer( &
            self, name, num_cells, data)
        class(type_vtu_writer), intent(inout) :: self
        !> Array name written to the VTU file.
        character(*), intent(in) :: name
        !> Number of cells.
        integer(c_int), intent(in) :: num_cells
        !> Interleaved (vx,vy,vz) components; shape = (3, num_cells).
        real(c_double), intent(in) :: data(3, num_cells)

        call vtu_writer_add_vector_cell_data( &
            self%writer_handle,                &
            trim(name)//c_null_char,           &
            num_cells, data)
    end subroutine write_vector_cell_data_type_vtu_writer

    ! ----------------------------------------------------------------
    ! write
    ! ----------------------------------------------------------------

    !> @brief Flush all accumulated mesh and field data to disk.
    !>
    !> After the flush, point- and cell-data arrays are cleared
    !> on the C++ side; the mesh topology is preserved for reuse
    !> in subsequent time steps.
    !>
    !> - Arithmetic complexity: O(num_points + num_cells) — I/O bound
    subroutine write_type_vtu_writer(self)
        class(type_vtu_writer), intent(inout) :: self

        call vtu_writer_write(self%writer_handle)
    end subroutine write_type_vtu_writer

    ! ----------------------------------------------------------------
    ! finalize
    ! ----------------------------------------------------------------

    !> @brief Release all VTK pipeline resources.
    !>
    !> Calls @c c_vtu_writer_destroy which invokes @c VtuWriter::finalize()
    !> and @c delete on the C++ heap object.  The handle is nullified
    !> so that double-free is safe.
    !>
    !> - Arithmetic complexity: O(1)
    !> - Memory complexity: O(1)
    subroutine finalize_type_vtu_writer(self)
        class(type_vtu_writer), intent(inout) :: self

        if (c_associated(self%writer_handle)) then
            call vtu_writer_destroy(self%writer_handle)
            self%writer_handle = c_null_ptr
        end if
    end subroutine finalize_type_vtu_writer

    ! ----------------------------------------------------------------
    ! Final destructor (automatic cleanup when object goes out of scope)
    ! ----------------------------------------------------------------

    !> @brief Automatic destructor invoked by the Fortran runtime.
    !>
    !> Guards against double-free by checking @c c_associated before
    !> calling @c finalize.
    subroutine auto_destroy_type_vtu_writer(self)
        type(type_vtu_writer), intent(inout) :: self

        if (c_associated(self%writer_handle)) then
            call vtu_writer_destroy(self%writer_handle)
            self%writer_handle = c_null_ptr
        end if
    end subroutine auto_destroy_type_vtu_writer

end module core_vtk_vtu_writer_wrapper
