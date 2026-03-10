!>
!> Domain package module
!> Aggregates public types and interfaces from domain sub-modules.
!>
module module_domain
    use :: module_fe, only: &
        abst_fe, &
        holder_fes, &
        type_side_first, &
        type_side_second, &
        type_triangle_first, &
        type_triangle_second, &
        type_square_first, &
        type_square_second, &
        create_fe, &
        type_fe_manager
    use :: domain_adjacency, only: &
        type_node_adjacency, &
        type_map_node_to_element
    use :: domain_multicoloring, only: &
        type_coloring

    use :: domain_components_boundaries, only: &
        type_boundaries_manager, &
        type_boundary_patch
    use :: domain_components_elements, only: &
        type_elements_manager
    use :: domain_components_nodes, only: &
        type_nodes_manager
    use :: domain_domain_manager, only: &
        type_domain

    implicit none
    private

    ! Public types
    public :: abst_fe
    public :: holder_fes
    public :: type_side_first
    public :: type_side_second
    public :: type_triangle_first
    public :: type_triangle_second
    public :: type_square_first
    public :: type_square_second
    public :: create_fe
    public :: type_fe_manager

    public :: type_domain
    public :: type_node_adjacency
    public :: type_map_node_to_element
    public :: type_coloring


    public :: type_elements_manager
    public :: type_boundary_patch
    public :: type_boundaries_manager
    public :: type_nodes_manager
end module module_domain
