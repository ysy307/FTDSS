# CMakeLists.txt for src/domain
#
# Defines components related to the computational domain and mesh.

# ====================================================================
# 1. Component Libraries
# ====================================================================

# --- Mesh Library ---
# This is a foundational component for the domain.
set(DOMAIN_MESH_SOURCES
    mesh/mesh_interface.F90
    mesh/element/element_interface.F90
    mesh/element/element_triangle_first.F90
    mesh/element/element_triangle_second.F90
    mesh/element/element_square_first.F90
    mesh/element/element_square_second.F90
    mesh/element/element_factory.F90
    mesh/element/element.F90
    mesh/side/side_interface.F90
    mesh/side/side_first.F90
    mesh/side/side_second.F90
    mesh/side/side_factory.F90
    mesh/side/side.F90
    mesh/mesh.F90
)
add_library(Domain_Mesh STATIC ${DOMAIN_MESH_SOURCES})
target_include_directories(Domain_Mesh PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
# Mesh depends on Core for types and Input for reading geometry.
target_link_libraries(Domain_Mesh PUBLIC
    Solver::Core
    Solver::Input
)
enable_build_flags(Domain_Mesh)
enable_thirdparty(Domain_Mesh)

# --- Adjacency Library ---
# Computes connectivity information (e.g., node-to-element).
set(DOMAIN_ADJACENCY_SOURCES
    adjacency/adjacency_element.F90
    adjacency/adjacency_node.F90
    adjacency/adjacency_node_element.F90
    adjacency/adjacency.F90
)
add_library(Domain_Adjacency STATIC ${DOMAIN_ADJACENCY_SOURCES})
target_include_directories(Domain_Adjacency PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
# Adjacency calculations depend on the mesh structure.
target_link_libraries(Domain_Adjacency PUBLIC Domain_Mesh)
enable_build_flags(Domain_Adjacency)
enable_thirdparty(Domain_Adjacency)


# --- Multicoloring Library ---
# Graph coloring algorithms for parallelization.
set(DOMAIN_MULTICOLORING_SOURCES
    multicoloring/multicoloring.F90
    # multicoloring/welch_powell.F90
    # multicoloring/dsatur.F90
    # multicoloring/lfo.F90
)
add_library(Domain_Multicoloring STATIC ${DOMAIN_MULTICOLORING_SOURCES})
target_include_directories(Domain_Multicoloring PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
# Coloring depends on the graph, which is derived from adjacency info.
target_link_libraries(Domain_Multicoloring PUBLIC Domain_Adjacency)
enable_build_flags(Domain_Multicoloring)
enable_thirdparty(Domain_Multicoloring)

# --- Reordering Library ---
# Node/element reordering algorithms to improve matrix properties.
set(DOMAIN_REORDERING_SOURCES
    reordering/reordering.F90
    reordering/methods.F90
    reordering/to_original.F90
    reordering/to_reordered.F90
)
add_library(Domain_Reordering STATIC ${DOMAIN_REORDERING_SOURCES})
target_include_directories(Domain_Reordering PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
# Reordering also depends on the graph/adjacency info.
target_link_libraries(Domain_Reordering PUBLIC Domain_Adjacency)
enable_build_flags(Domain_Reordering)
enable_thirdparty(Domain_Reordering)

# --- Domain Manager Library ---
# The main service/manager for the domain module.
set(DOMAIN_MAIN_SOURCES
    domain_manager.F90
    domain.F90
)
add_library(Domain_Main STATIC ${DOMAIN_MAIN_SOURCES})
target_include_directories(Domain_Main PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
# The manager uses all other domain components.
target_link_libraries(Domain_Main PUBLIC
    Domain_Mesh
    Domain_Adjacency
    Domain_Multicoloring
    Domain_Reordering
    Solver::Core
    Solver::Input
)
enable_build_flags(Domain_Main)
enable_thirdparty(Domain_Main)


# ====================================================================
# 2. Aggregator Interface Library
# ====================================================================
add_library(Domain_Interface INTERFACE)

# Link all component libraries to this target.
target_link_libraries(Domain_Interface INTERFACE
    Domain_Main
)

# Create a clean, namespaced ALIAS for other directories to use.
add_library(Solver::Domain ALIAS Domain_Interface)