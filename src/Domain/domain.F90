module module_domain
    use :: domain_element, only:holder_elements
    use :: domain_element_factory, only:create_element
    use :: domain_side, only:holder_sides
    use :: domain_side_Factory, only:create_side
    use :: domain_adjacency, only:type_node_adjacency, type_element_adjacency
    use :: domain_multicoloring, only:type_coloring
    use :: domain_rcm, only:type_rcm
    use :: domain_manager, only:type_domain
    implicit none
    private

    public :: holder_elements, &
              create_element, &
              holder_sides, &
              create_side, &
              type_node_adjacency, &
              type_element_adjacency, &
              type_coloring, &
              type_rcm, &
              type_domain

end module module_domain
