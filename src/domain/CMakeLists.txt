add_library(DOMAIN STATIC
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
target_include_directories(DOMAIN PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
target_link_libraries(DOMAIN PUBLIC
    CORE
    INPUT
    MESH
)
enable_build_flags(DOMAIN)
enable_thirdparty (DOMAIN)