# ============================================================
# System Library (single static archive)
# ============================================================
add_library(SOLVER_SYSTEM STATIC)

# ------------------------------------------------------------
# Sources
# ------------------------------------------------------------
target_sources(SOLVER_SYSTEM PRIVATE
    jacobian_matrix.F90
    residual_vector.F90
    system.F90
)

# ------------------------------------------------------------
# Fortran module output directory
# ------------------------------------------------------------
target_include_directories(SOLVER_SYSTEM PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR}
)

# ------------------------------------------------------------
# Dependencies
# ------------------------------------------------------------
target_link_libraries(SOLVER_SYSTEM PUBLIC
    SOLVER::CORE
    SOLVER::LINALG
    SOLVER::DOMAIN
)

enable_build_flags(SOLVER_SYSTEM)
enable_thirdparty(SOLVER_SYSTEM)

# ------------------------------------------------------------
# Namespaced alias
# ------------------------------------------------------------
add_library(SOLVER::SYSTEM ALIAS SOLVER_SYSTEM)
