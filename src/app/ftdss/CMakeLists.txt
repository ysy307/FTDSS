# ============================================================
# FTDSS Application Library
# ============================================================
add_library(SOLVER_FTDSS STATIC)

# ------------------------------------------------------------
# Sources
# ------------------------------------------------------------
target_sources(SOLVER_FTDSS
    PRIVATE
        ftdss_interface.F90
        ftdss_base.F90
        ftdss_boundary.F90
        ftdss_assemble.F90
        ftdss_compute.F90
        ftdss_solve.F90
        ftdss.F90
)

# ------------------------------------------------------------
# Include directory (Fortran module path)
# ------------------------------------------------------------
target_include_directories(SOLVER_FTDSS
    PUBLIC
        ${CMAKE_CURRENT_BINARY_DIR}
)

# ------------------------------------------------------------
# Dependencies
# ------------------------------------------------------------
target_link_libraries(SOLVER_FTDSS
    PUBLIC
        SOLVER::GOVERNING
        SOLVER::CORE
        SOLVER::CONTROL
        SOLVER::INPUT
        SOLVER::OUTPUT
        SOLVER::LINALG
        SOLVER::SOLVER
)

enable_build_flags(SOLVER_FTDSS)
enable_thirdparty(SOLVER_FTDSS)

# ------------------------------------------------------------
# Namespaced alias
# ------------------------------------------------------------
add_library(SOLVER::FTDSS ALIAS SOLVER_FTDSS)