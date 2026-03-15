!>
!> A factory for creating concrete finite element objects based on FE_TYPE IDs.
!>
module domain_fe_factory
    use, intrinsic :: iso_fortran_env
    use :: stdlib_logger, only:global_logger
    use :: stdlib_strings, only:to_string
    use :: module_core, only:FE_TYPE
    use :: domain_base_fe, only:abst_fe
    use :: domain_fe_side
    use :: domain_fe_element
    use :: domain_fe_volume

    implicit none
    private

    public :: create_fe

contains

    !>
    !> Creates a concrete finite element object based on an FE_TYPE ID.
    !>
    function create_fe(id, integration_order) result(fe)
        implicit none
        !> The FE_TYPE ID for the element to create.
        integer(int32), intent(in) :: id
        !> The integration order for the element.
        integer(int32), intent(in) :: integration_order
        !> The newly allocated finite element object.
        class(abst_fe), allocatable :: fe

        character(len=*), parameter :: func_name = "create_fe"

        if (FE_TYPE%LINE == id) then
            fe = construct_side_first(integration_order)
        else if (FE_TYPE%QUADRATIC_EDGE == id) then
            fe = construct_side_second(integration_order)
        else if (FE_TYPE%TRIANGLE == id) then
            fe = construct_triangle_first(integration_order)
        else if (FE_TYPE%QUAD == id) then
            fe = construct_square_first(integration_order)
        else if (FE_TYPE%QUADRATIC_TRIANGLE == id) then
            fe = construct_triangle_second(integration_order)
        else if (FE_TYPE%QUADRATIC_QUAD == id) then
            fe = construct_square_second(integration_order)
        else if (FE_TYPE%LAGRANGE_TRIANGLE == id .or. FE_TYPE%HIGHER_ORDER_TRIANGLE == id) then
            fe = construct_triangle_third(integration_order)
        else if (FE_TYPE%LAGRANGE_QUADRILATERAL == id) then
            fe = construct_square_third(integration_order)
        else if (FE_TYPE%BIQUADRATIC_QUAD == id) then
            fe = construct_square_second_lagrange(integration_order)
        else if (FE_TYPE%HIGHER_ORDER_QUAD == id) then
            fe = construct_square_third_serendipity(integration_order)
        else if (FE_TYPE%TETRA == id) then
            fe = construct_tetra_first(integration_order)
        else if (FE_TYPE%QUADRATIC_TETRA == id) then
            fe = construct_tetra_second(integration_order)
        else if (FE_TYPE%LAGRANGE_TETRAHEDRON == id .or. FE_TYPE%HIGHER_ORDER_TETRAHEDRON == id) then
            fe = construct_tetra_third(integration_order)
        else if (FE_TYPE%HEXAHEDRON == id) then
            fe = construct_hexa_first(integration_order)
        else if (FE_TYPE%TRIQUADRATIC_HEXAHEDRON == id) then
            fe = construct_hexa_second(integration_order)
        else if (FE_TYPE%QUADRATIC_HEXAHEDRON == id) then
            fe = construct_hexa_second_serendipity(integration_order)
        else if (FE_TYPE%LAGRANGE_HEXAHEDRON == id) then
            fe = construct_hexa_third(integration_order)
        else if (FE_TYPE%HIGHER_ORDER_HEXAHEDRON == id) then
            fe = construct_hexa_third_serendipity(integration_order)
        else
            call global_logger%log_error( &
                func_name//": No constructor for ID = " &
                //to_string(id))
        end if

    end function create_fe

end module domain_fe_factory
