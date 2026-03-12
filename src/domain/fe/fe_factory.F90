!>
!> A factory for creating concrete finite element objects based on VTK cell type IDs.
!>
module domain_fe_factory
    use, intrinsic :: iso_fortran_env
    use :: stdlib_logger, only:global_logger
    use :: stdlib_strings, only:to_string
    use :: module_core, only:vtk_constants
    use :: domain_base_fe, only:abst_fe
    use :: domain_fe_side
    use :: domain_fe_element
    use :: domain_fe_volume

    implicit none
    private

    public :: create_fe

    integer(int32), private, save :: id_line = -1
    integer(int32), private, save :: id_quadratic_edge = -1
    integer(int32), private, save :: id_triangle = -1
    integer(int32), private, save :: id_quad = -1
    integer(int32), private, save :: id_quadratic_triangle = -1
    integer(int32), private, save :: id_quadratic_quad = -1
    integer(int32), private, save :: id_lagrange_triangle = -1
    integer(int32), private, save :: id_lagrange_quad = -1
    integer(int32), private, save :: id_biquadratic_quad = -1
    integer(int32), private, save :: id_tetra = -1
    integer(int32), private, save :: id_quadratic_tetra = -1
    integer(int32), private, save :: id_lagrange_tetra = -1
    integer(int32), private, save :: id_hexahedron = -1
    integer(int32), private, save :: id_triquadratic_hexa = -1
    integer(int32), private, save :: id_quadratic_hexa = -1
    integer(int32), private, save :: id_lagrange_hexa = -1

    logical, private, save :: is_initialized = .false.

contains

    !>
    !> Creates a concrete finite element object based on a VTK cell type ID.
    !>
    function create_fe(id, integration_order) result(fe)
        implicit none
        !> The VTK cell type ID for the element to create.
        integer(int32), intent(in) :: id
        !> The integration order for the element.
        integer(int32), intent(in) :: integration_order
        !> The newly allocated finite element object.
        class(abst_fe), allocatable :: fe

        character(len=*), parameter :: func_name = "create_fe"

        if (.not. is_initialized) call initialize_factory_ids()

        if (id == id_line) then
            fe = construct_side_first(integration_order)
        else if (id == id_quadratic_edge) then
            fe = construct_side_second(integration_order)
        else if (id == id_triangle) then
            fe = construct_triangle_first(integration_order)
        else if (id == id_quad) then
            fe = construct_square_first(integration_order)
        else if (id == id_quadratic_triangle) then
            fe = construct_triangle_second(integration_order)
        else if (id == id_quadratic_quad) then
            fe = construct_square_second(integration_order)
        else if (id == id_lagrange_triangle) then
            fe = construct_triangle_third(integration_order)
        else if (id == id_lagrange_quad) then
            fe = construct_square_third(integration_order)
        else if (id == id_biquadratic_quad) then
            fe = construct_square_second_lagrange(integration_order)
        else if (id == id_tetra) then
            fe = construct_tetra_first(integration_order)
        else if (id == id_quadratic_tetra) then
            fe = construct_tetra_second(integration_order)
        else if (id == id_lagrange_tetra) then
            fe = construct_tetra_third(integration_order)
        else if (id == id_hexahedron) then
            fe = construct_hexa_first(integration_order)
        else if (id == id_triquadratic_hexa) then
            fe = construct_hexa_second(integration_order)
        else if (id == id_quadratic_hexa) then
            fe = construct_hexa_second_serendipity(integration_order)
        else if (id == id_lagrange_hexa) then
            fe = construct_hexa_third(integration_order)
        else
            call global_logger%log_error( &
                func_name//": No constructor for ID = " &
                //to_string(id))
        end if

    end function create_fe

    !>
    !> Initializes the VTK cell type IDs for dispatch.
    !>
    subroutine initialize_factory_ids()
        implicit none

        if (is_initialized) return

        id_line = vtk_constants%get_cell_type("Line")
        id_quadratic_edge = vtk_constants%get_cell_type("QuadraticEdge")
        id_triangle = vtk_constants%get_cell_type("Triangle")
        id_quad = vtk_constants%get_cell_type("Quad")
        id_quadratic_triangle = vtk_constants%get_cell_type("QuadraticTriangle")
        id_quadratic_quad = vtk_constants%get_cell_type("QuadraticQuad")
        id_lagrange_triangle = vtk_constants%get_cell_type("LagrangeTriangle")
        id_lagrange_quad = vtk_constants%get_cell_type("LagrangeQuadrilateral")
        id_biquadratic_quad = vtk_constants%get_cell_type("BiQuadraticQuad")
        id_tetra = vtk_constants%get_cell_type("Tetra")
        id_quadratic_tetra = vtk_constants%get_cell_type("QuadraticTetra")
        id_lagrange_tetra = vtk_constants%get_cell_type("LagrangeTetrahedron")
        id_hexahedron = vtk_constants%get_cell_type("Hexahedron")
        id_triquadratic_hexa = vtk_constants%get_cell_type("TriQuadraticHexahedron")
        id_quadratic_hexa = vtk_constants%get_cell_type("QuadraticHexahedron")
        id_lagrange_hexa = vtk_constants%get_cell_type("LagrangeHexahedron")

        is_initialized = .true.

    end subroutine initialize_factory_ids

end module domain_fe_factory
