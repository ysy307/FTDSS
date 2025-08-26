module module_mesh
    use :: domain_mesh, only:abst_mesh
    use :: module_mesh_side, only: &
        abst_side, type_side_first, type_side_second, holder_sides, create_side
    use :: module_mesh_element, only: &
        abst_element, type_triangle_first, type_triangle_second, type_square_first, &
        type_square_second, holder_elements, create_element
    implicit none
    private

    !-------------------------------------------------------------------------------------------------------------------------------
    ! Holder for polymorphic objects
    !-------------------------------------------------------------------------------------------------------------------------------
    public :: holder_sides
    public :: holder_elements
    !-------------------------------------------------------------------------------------------------------------------------------
    ! derived types
    !-------------------------------------------------------------------------------------------------------------------------------
    public :: abst_mesh
    public :: abst_side
    public :: abst_element
    public :: type_side_first
    public :: type_side_second
    public :: type_triangle_first
    public :: type_triangle_second
    public :: type_square_first
    public :: type_square_second
    !-------------------------------------------------------------------------------------------------------------------------------
    ! side types
    !-------------------------------------------------------------------------------------------------------------------------------
    public :: create_side
    public :: create_element

end module module_mesh
