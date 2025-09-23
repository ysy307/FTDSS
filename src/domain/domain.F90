module module_domain
    ! use :: module_mesh
    ! use :: domain_adjacency, only:type_node_adjacency, type_crs_adjacency_element
    ! use :: domain_multicoloring, only:type_coloring
    ! use :: domain_reordering, only:type_reordering
    use :: domain_manager, only:type_domain
    implicit none
    private

    ! public :: holder_elements, abst_element, create_element, abst_mesh
    ! public :: holder_sides, abst_side, create_side
    ! public :: type_crs_adjacency_element, type_node_adjacency
    public :: type_domain
    ! public :: type_coloring

    ! public :: type_reordering

end module module_domain
