# ============================================================
# Linear Algebra Library (single static archive)
# ============================================================
add_library(SOLVER_LINALG STATIC)

# ------------------------------------------------------------
# Sources
# ------------------------------------------------------------
target_sources(SOLVER_LINALG
    PRIVATE
        mkl_interface.F90
        mkl_backend.F90
        vector_operations.F90
        matrix_operations.F90
        linalg.F90
)

# ------------------------------------------------------------
# Fortran module output directory
# ------------------------------------------------------------
target_include_directories(SOLVER_LINALG
    PUBLIC
        ${CMAKE_CURRENT_BINARY_DIR}
)

# ------------------------------------------------------------
# Dependencies
# ------------------------------------------------------------
target_link_libraries(SOLVER_LINALG
    PUBLIC
        SOLVER::CORE
)

enable_build_flags(SOLVER_LINALG)

# ------------------------------------------------------------
# Namespaced alias
# ------------------------------------------------------------
add_library(SOLVER::LINALG ALIAS SOLVER_LINALG)