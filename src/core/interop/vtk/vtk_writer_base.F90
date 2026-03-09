module core_vtk_vtk_writer_base
    use, intrinsic :: iso_c_binding
    implicit none
    private

    public :: abst_vtk_writer

    type, abstract :: abst_vtk_writer
    contains
        procedure(initialize_vtk_writer_interface), deferred :: initialize
        procedure(write_mesh_vtk_writer_interface), deferred :: write_mesh
        procedure(write_scalar_point_data_vtk_writer_interface), deferred :: write_scalar_point_data
        procedure(write_vector_point_data_vtk_writer_interface), deferred :: write_vector_point_data
        procedure(write_scalar_cell_data_vtk_writer_interface), deferred :: write_scalar_cell_data
        procedure(write_vector_cell_data_vtk_writer_interface), deferred :: write_vector_cell_data
        procedure(write_vtk_writer_interface), deferred :: write
        procedure(finalize_vtk_writer_interface), deferred :: finalize
    end type abst_vtk_writer

    abstract interface
        subroutine initialize_vtk_writer_interface(self, filename)
            import :: abst_vtk_writer
            class(abst_vtk_writer), intent(inout) :: self
            character(*), intent(in) :: filename
        end subroutine initialize_vtk_writer_interface

        subroutine write_mesh_vtk_writer_interface( &
                self, num_points, points,            &
                num_cells, conn_size,                &
                connectivity, offsets, cell_types)
            import :: abst_vtk_writer, c_int, c_double
            class(abst_vtk_writer), intent(inout) :: self
            integer(c_int), intent(in) :: num_points
            real(c_double), intent(in) :: points(3, num_points)
            integer(c_int), intent(in) :: num_cells
            integer(c_int), intent(in) :: conn_size
            integer(c_int), intent(in) :: connectivity(conn_size)
            integer(c_int), intent(in) :: offsets(num_cells)
            integer(c_int), intent(in) :: cell_types(num_cells)
        end subroutine write_mesh_vtk_writer_interface

        subroutine write_scalar_point_data_vtk_writer_interface(self, name, num_points, data)
            import :: abst_vtk_writer, c_int, c_double
            class(abst_vtk_writer), intent(inout) :: self
            character(*), intent(in) :: name
            integer(c_int), intent(in) :: num_points
            real(c_double), intent(in) :: data(num_points)
        end subroutine write_scalar_point_data_vtk_writer_interface

        subroutine write_vector_point_data_vtk_writer_interface(self, name, num_points, data)
            import :: abst_vtk_writer, c_int, c_double
            class(abst_vtk_writer), intent(inout) :: self
            character(*), intent(in) :: name
            integer(c_int), intent(in) :: num_points
            real(c_double), intent(in) :: data(3, num_points)
        end subroutine write_vector_point_data_vtk_writer_interface

        subroutine write_scalar_cell_data_vtk_writer_interface(self, name, num_cells, data)
            import :: abst_vtk_writer, c_int, c_double
            class(abst_vtk_writer), intent(inout) :: self
            character(*), intent(in) :: name
            integer(c_int), intent(in) :: num_cells
            real(c_double), intent(in) :: data(num_cells)
        end subroutine write_scalar_cell_data_vtk_writer_interface

        subroutine write_vector_cell_data_vtk_writer_interface(self, name, num_cells, data)
            import :: abst_vtk_writer, c_int, c_double
            class(abst_vtk_writer), intent(inout) :: self
            character(*), intent(in) :: name
            integer(c_int), intent(in) :: num_cells
            real(c_double), intent(in) :: data(3, num_cells)
        end subroutine write_vector_cell_data_vtk_writer_interface

        subroutine write_vtk_writer_interface(self)
            import :: abst_vtk_writer
            class(abst_vtk_writer), intent(inout) :: self
        end subroutine write_vtk_writer_interface

        subroutine finalize_vtk_writer_interface(self)
            import :: abst_vtk_writer
            class(abst_vtk_writer), intent(inout) :: self
        end subroutine finalize_vtk_writer_interface
    end interface

end module core_vtk_vtk_writer_base
