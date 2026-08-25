module io_output_overall
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding, only: c_int, c_double
    use :: stdlib_strings, only:to_string, strip
    use :: stdlib_ascii, only:to_lower
    use :: stdlib_io, only:open
    use :: module_core
    use :: domain_mesh_plex, only:type_mesh_plex
    implicit none
    private

    public :: abst_output_overall
    public :: type_output_overall_vtk
    public :: type_output_overall_vtu
    public :: type_output_overall_hdf5

    ! Base abstract type containing all common variables
    type, abstract :: abst_output_overall
        private
        ! Settings
        character(:), allocatable :: dir_output_field
        character(:), allocatable :: format_output_file
        type(type_constant_id) :: file_format
        type(type_constant_id), allocatable :: variables(:)
        ! Common mesh data
        integer(int32) :: num_points
        integer(int32) :: num_cells
        type(type_coordinate_array_dp) :: coordinate
        integer(int32), allocatable :: connectivities(:)
        integer(int32), allocatable :: offsets(:)
        integer(int8), allocatable :: cell_types(:)
    contains
        procedure(abst_initialize), deferred, public, pass(self) :: initialize
        procedure(abst_write_fields), deferred, public, pass(self) :: write_fields
        procedure(abst_write_cell), deferred, public, pass(self) :: write_cell
    end type

    ! Extended types only specify the procedures for their specific format
    !> Writes the mesh and the nodal fields through PETSc's HDF5 viewer, with an
    !> XDMF descriptor beside it so a viewer can read the pair. The geometry is
    !> the DM itself, so nothing here has to agree with a node ordering of its
    !> own, and the viewer is collective, so one file is written however many
    !> ranks there are.
    type, extends(abst_output_overall) :: type_output_overall_hdf5
        private
        type(type_mesh_plex), pointer :: mesh => null()
        character(:), allocatable :: directory
        !> Physical time of each frame written so far, for the collection file.
        real(real64), allocatable :: recorded_time(:)
        integer(int32) :: num_recorded = 0
        real(real64) :: current_time = 0.0d0
        !> Names of the fields that actually reached the file, so the XDMF
        !> descriptor never points at a dataset that was never written.
        character(len=32), allocatable :: written_fields(:)
        integer(int32) :: num_written_fields = 0
        integer(int32) :: space_dimension = 2
    contains
        procedure, public, pass(self) :: initialize => initialize_type_output_overall_hdf5
        procedure, public, pass(self) :: write_fields => write_fields_hdf5
        procedure, public, pass(self) :: write_cell => write_cell_hdf5
    end type type_output_overall_hdf5

    type, extends(abst_output_overall) :: type_output_overall_vtk
        private
    contains
        procedure, public, pass(self) :: initialize => initialize_type_output_overall_vtk
        procedure, public, pass(self) :: write_fields => write_fields_vtk
        procedure, public, pass(self) :: write_cell => write_cell_vtk

        generic, public :: output_field => output_points_scalar_real64, &
            output_points_scalar_int32, &
            output_points_vector_real64, &
            output_points_vector_int32
        procedure, private, pass(self) :: output_points_scalar_real64 => output_points_scalar_real64_vtk
        procedure, private, pass(self) :: output_points_scalar_int32 => output_points_scalar_int32_vtk
        procedure, private, pass(self) :: output_points_vector_real64 => output_points_vector_real64_vtk
        procedure, private, pass(self) :: output_points_vector_int32 => output_points_vector_int32_vtk
        generic, public :: output_cell => output_cell_real64, &
            output_cell_int32
        procedure, private, pass(self) :: output_cell_real64 => output_cell_real64_vtk
        procedure, private, pass(self) :: output_cell_int32 => output_cell_int32_vtk
    end type

    type, extends(abst_output_overall) :: type_output_overall_vtu
        private
        real(c_double), allocatable :: points_xyz(:, :)
        integer(c_int), allocatable :: connectivity_c(:)
        integer(c_int), allocatable :: offsets_vtk(:)
        integer(c_int), allocatable :: cell_types_c(:)
    contains
        procedure, public, pass(self) :: initialize => initialize_type_output_overall_vtu
        procedure, public, pass(self) :: write_fields => write_fields_vtu
        procedure, public, pass(self) :: write_cell => write_cell_vtu
    end type

    abstract interface
        subroutine abst_initialize(self, dir_output, config, mesh)
            import :: abst_output_overall, type_config_overall, type_mesh_plex
            implicit none
            class(abst_output_overall), intent(inout) :: self
            character(*), intent(in) :: dir_output
            type(type_config_overall), intent(in) :: config
            !> The mesh, for a writer that describes the geometry from the DM
            !> instead of from a transcribed copy of it.
            type(type_mesh_plex), intent(inout), optional, target :: mesh
        end subroutine abst_initialize

        subroutine abst_write_fields(self, file_counts, temperature, &
                water_content, ice_content, vapor_content, pressure, water_flux, time)
            import :: abst_output_overall, type_coordinate_array_dp, real64, int32
            implicit none
            class(abst_output_overall), intent(inout) :: self
            integer(int32), intent(in) :: file_counts
            real(real64), intent(in), optional :: temperature(:)
            real(real64), intent(in), optional :: water_content(:)
            real(real64), intent(in), optional :: ice_content(:)
            real(real64), intent(in), optional :: vapor_content(:)
            real(real64), intent(in), optional :: pressure(:)
            type(type_coordinate_array_dp), intent(in), optional :: water_flux
            !> Physical time of this frame, for a writer that records a series.
            real(real64), intent(in), optional :: time
        end subroutine abst_write_fields

        subroutine abst_write_cell(self, file_name, variable_name, variable)
            import :: abst_output_overall, int32
            implicit none
            class(abst_output_overall), intent(inout) :: self
            character(*), intent(in) :: file_name
            character(*), intent(in) :: variable_name
            integer(int32), intent(in) :: variable(:)
        end subroutine abst_write_cell
    end interface

    interface
        ! Concrete implementations for type_output_overall_vtk
        module subroutine initialize_type_output_overall_vtk(self, dir_output, config, mesh)
            implicit none
            class(type_output_overall_vtk), intent(inout) :: self
            character(*), intent(in) :: dir_output
            type(type_config_overall), intent(in) :: config
            type(type_mesh_plex), intent(inout), optional, target :: mesh
        end subroutine initialize_type_output_overall_vtk

        module subroutine write_fields_vtk(self, file_counts, temperature, water_content, ice_content, &
                                           vapor_content, pressure, water_flux, time)
            implicit none
            class(type_output_overall_vtk), intent(inout) :: self
            integer(int32), intent(in) :: file_counts
            real(real64), intent(in), optional :: temperature(:)
            real(real64), intent(in), optional :: water_content(:)
            real(real64), intent(in), optional :: ice_content(:)
            real(real64), intent(in), optional :: vapor_content(:)
            real(real64), intent(in), optional :: pressure(:)
            type(type_coordinate_array_dp), intent(in), optional :: water_flux
            real(real64), intent(in), optional :: time
        end subroutine write_fields_vtk

        module subroutine write_cell_vtk(self, file_name, variable_name, variable)
            implicit none
            class(type_output_overall_vtk), intent(inout) :: self
            character(*), intent(in) :: file_name
            character(*), intent(in) :: variable_name
            integer(int32), intent(in) :: variable(:)
        end subroutine write_cell_vtk

        module subroutine output_points_scalar_real64_vtk(self, unit, data_name, x)
            implicit none
            class(type_output_overall_vtk), intent(in) :: self
            integer(int32), intent(in) :: unit
            character(*), intent(in) :: data_name
            real(real64), intent(in) :: x(:)

        end subroutine output_points_scalar_real64_vtk

        module subroutine output_points_scalar_int32_vtk(self, unit, data_name, x)
            implicit none
            class(type_output_overall_vtk), intent(in) :: self
            integer(int32), intent(in) :: unit
            character(*), intent(in) :: data_name
            integer(int32), intent(in) :: x(:)

        end subroutine output_points_scalar_int32_vtk

        module subroutine output_points_vector_real64_vtk(self, unit, data_name, x, y, z)
            implicit none
            class(type_output_overall_vtk), intent(in) :: self
            integer(int32), intent(in) :: unit
            character(*), intent(in) :: data_name
            real(real64), intent(in) :: x(:), y(:), z(:)

        end subroutine output_points_vector_real64_vtk

        module subroutine output_points_vector_int32_vtk(self, unit, data_name, x, y, z)
            implicit none
            class(type_output_overall_vtk), intent(in) :: self
            integer(int32), intent(in) :: unit
            character(*), intent(in) :: data_name
            integer(int32), intent(in) :: x(:), y(:), z(:)

        end subroutine output_points_vector_int32_vtk

        module subroutine output_cell_real64_vtk(self, unit, data_name, x)
            implicit none
            class(type_output_overall_vtk), intent(in) :: self
            integer(int32), intent(in) :: unit
            character(*), intent(in) :: data_name
            real(real64), intent(in) :: x(:)

        end subroutine output_cell_real64_vtk

        module subroutine output_cell_int32_vtk(self, unit, data_name, x)
            implicit none
            class(type_output_overall_vtk), intent(in) :: self
            integer(int32), intent(in) :: unit
            character(*), intent(in) :: data_name
            integer(int32), intent(in) :: x(:)

        end subroutine output_cell_int32_vtk

        ! Concrete implementations for type_output_overall_vtu
        module subroutine initialize_type_output_overall_vtu(self, dir_output, config, mesh)
            implicit none
            class(type_output_overall_vtu), intent(inout) :: self
            character(*), intent(in) :: dir_output
            type(type_config_overall), intent(in) :: config
            type(type_mesh_plex), intent(inout), optional, target :: mesh
        end subroutine initialize_type_output_overall_vtu

        module subroutine write_fields_vtu(self, file_counts, temperature, water_content, ice_content, &
                                           vapor_content, pressure, water_flux, time)
            implicit none
            class(type_output_overall_vtu), intent(inout) :: self
            integer(int32), intent(in) :: file_counts
            real(real64), intent(in), optional :: temperature(:)
            real(real64), intent(in), optional :: water_content(:)
            real(real64), intent(in), optional :: ice_content(:)
            real(real64), intent(in), optional :: vapor_content(:)
            real(real64), intent(in), optional :: pressure(:)
            type(type_coordinate_array_dp), intent(in), optional :: water_flux
            real(real64), intent(in), optional :: time
        end subroutine write_fields_vtu

        module subroutine write_cell_vtu(self, file_name, variable_name, variable)
            implicit none
            class(type_output_overall_vtu), intent(inout) :: self
            character(*), intent(in) :: file_name
            character(*), intent(in) :: variable_name
            integer(int32), intent(in) :: variable(:)
        end subroutine write_cell_vtu

    end interface

    interface
        module subroutine initialize_type_output_overall_hdf5(self, dir_output, config, mesh)
            implicit none
            class(type_output_overall_hdf5), intent(inout) :: self
            character(*), intent(in) :: dir_output
            type(type_config_overall), intent(in) :: config
            type(type_mesh_plex), intent(inout), optional, target :: mesh
        end subroutine initialize_type_output_overall_hdf5

        module subroutine write_fields_hdf5(self, file_counts, temperature, water_content, &
                                            ice_content, vapor_content, pressure, water_flux, time)
            implicit none
            class(type_output_overall_hdf5), intent(inout) :: self
            integer(int32), intent(in) :: file_counts
            real(real64), intent(in), optional :: temperature(:)
            real(real64), intent(in), optional :: water_content(:)
            real(real64), intent(in), optional :: ice_content(:)
            real(real64), intent(in), optional :: vapor_content(:)
            real(real64), intent(in), optional :: pressure(:)
            type(type_coordinate_array_dp), intent(in), optional :: water_flux
            real(real64), intent(in), optional :: time
        end subroutine write_fields_hdf5

        module subroutine write_cell_hdf5(self, file_name, variable_name, variable)
            implicit none
            class(type_output_overall_hdf5), intent(inout) :: self
            character(*), intent(in) :: file_name
            character(*), intent(in) :: variable_name
            integer(int32), intent(in) :: variable(:)
        end subroutine write_cell_hdf5
    end interface

end module io_output_overall
