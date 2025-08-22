add_library(Domain OBJECT
    element/element_interface.F90
    element/element_triangle_first.F90
    element/element_triangle_second.F90
    element/element_square_first.F90
    element/element_square_second.F90
    element/element_factory.F90
    element/element.F90
    side/side.F90
    side/side_first.F90
    side/side_second.F90
    side/side_factory.F90
    adjacency/adjacency_element.F90
    adjacency/adjacency_node.F90
    adjacency/adjacency_node_element.F90
    adjacency/adjacency.F90
    multicoloring/multicoloring.F90
    multicoloring/welch_powell.F90
    multicoloring/dsatur.F90
    multicoloring/lfo.F90
    reordering/reordering.F90
    reordering/methods.F90
    reordering/to_original.F90
    reordering/to_reordered.F90
    domain_manager.F90
    domain.F90
)
target_link_libraries(Domain PRIVATE
    Core
    Input
)
enable_build_flags(Domain)
enable_thirdparty (Domain)