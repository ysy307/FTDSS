# ============================================================
# Domain Library (single static archive)
# ============================================================
add_library(SOLVER_DOMAIN STATIC)

# ------------------------------------------------------------
# Sources
# ------------------------------------------------------------
target_sources(SOLVER_DOMAIN
    PRIVATE
        # ====================================================
        # 1. Mesh (Finite Element Core)
        # ====================================================
        # Abstract finite element definitions
        fe/base/fe_interface.F90
        fe/base/fe_base.F90
        fe/base/fe_interpolation.F90
        fe/base/fe_compute.F90
        fe/base/fe_coefficients.F90
        # 1D Finite Elements
        fe/side/side_interface.F90
        fe/side/side_first.F90
        fe/side/side_second.F90
        # 2D Finite Elements
        fe/element/element_interface.F90
        fe/element/element_triangle_first.F90
        fe/element/element_triangle_second.F90
        fe/element/element_square_first.F90
        fe/element/element_square_second.F90
        # 3D Finite Elements
        fe/volume/volume_interface.F90
        # FE utilities
        fe/fe_integration.F90
        fe/fe_factory.F90
        fe/fe_manager.F90
        fe/fe.F90

        # ====================================================
        # 2. Adjacency
        # ====================================================
        adjacency/node.F90
        adjacency/node_element.F90
        adjacency/adjacency.F90

        # ====================================================
        # 3. Multicoloring
        # ====================================================
        multicoloring/multicoloring.F90

        # ====================================================
        # 4. Domain Components
        # ====================================================
        components/domain_nodes.F90
        components/domain_elements.F90
        components/domain_boundaries.F90

        # ====================================================
        # 5. Domain Service Layer
        # ====================================================
        domain_manager.F90
        domain.F90
)

# ------------------------------------------------------------
# Fortran module output directory
# ------------------------------------------------------------
target_include_directories(SOLVER_DOMAIN
    PUBLIC
        ${CMAKE_CURRENT_BINARY_DIR}
)

# ------------------------------------------------------------
# Dependencies
# ------------------------------------------------------------
target_link_libraries(SOLVER_DOMAIN
    PUBLIC
        SOLVER::CORE
        SOLVER::LINALG
        SOLVER::INPUT
        SOLVER::CONTROL
)

enable_build_flags(SOLVER_DOMAIN)
enable_thirdparty (SOLVER_DOMAIN)

# ------------------------------------------------------------
# Namespaced alias
# ------------------------------------------------------------
add_library(SOLVER::DOMAIN ALIAS SOLVER_DOMAIN)