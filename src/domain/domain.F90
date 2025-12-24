module module_domain
    !>
    !> Domain package module
    !> Aggregates public types and interfaces from domain sub-modules.
    !>
    use :: module_fe, only:abst_fe
    use :: domain_adjacency, only:type_node_adjacency, type_map_node_to_element
    use :: domain_manager, only:type_domain
    use :: domain_multicoloring, only:type_coloring

    implicit none
    private

    ! Public types
    public :: abst_fe
    public :: type_domain
    public :: type_node_adjacency
    public :: type_map_node_to_element
    public :: type_coloring

end module module_domain
