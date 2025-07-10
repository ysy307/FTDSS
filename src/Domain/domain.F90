module module_domain
    use :: domain_element, only:holder_elements, abst_element
    use :: domain_element_factory, only:create_element
    use :: domain_side, only:holder_sides, abst_side
    use :: domain_side_Factory, only:create_side
    use :: domain_adjacency, only:type_node_adjacency, type_crs_adjacency_element
    use :: domain_multicoloring, only:type_coloring
    use :: domain_rcm, only:type_rcm
    use :: domain_manager, only:type_domain
    implicit none
    private

    public :: holder_elements, abst_element, create_element
    public :: holder_sides, abst_side, create_side
    public :: type_crs_adjacency_element, type_node_adjacency
    public :: type_domain
    public :: type_coloring
    public :: type_rcm

end module module_domain
