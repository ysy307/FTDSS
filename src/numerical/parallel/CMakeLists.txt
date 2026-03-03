# ============================================================
# Parallel Library (single static archive)
# ============================================================
add_library(SOLVER_PARALLEL STATIC)

# ------------------------------------------------------------
# Sources
# ------------------------------------------------------------
target_sources(SOLVER_PARALLEL PRIVATE
    communicator.F90
    parallel.F90
)

# ------------------------------------------------------------
# Fortran module output directory
# ------------------------------------------------------------
target_include_directories(SOLVER_PARALLEL PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR}
)

# ------------------------------------------------------------
# Dependencies
# ------------------------------------------------------------
target_link_libraries(SOLVER_PARALLEL PUBLIC
    SOLVER::CORE
    SOLVER::INPUT
)

enable_build_flags(SOLVER_PARALLEL)
enable_thirdparty(SOLVER_PARALLEL)

# ------------------------------------------------------------
# Namespaced alias
# ------------------------------------------------------------
add_library(SOLVER::PARALLEL ALIAS SOLVER_PARALLEL)
