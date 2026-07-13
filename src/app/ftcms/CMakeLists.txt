# ============================================================
# FTCMS Application Library
# ============================================================
add_library(SOLVER_FTCMS STATIC)

# ------------------------------------------------------------
# Sources
# ------------------------------------------------------------
target_sources(SOLVER_FTCMS PRIVATE
    ftcms_interface.F90
    ftcms_base.F90
    ftcms_boundary.F90
    ftcms_assemble.F90
    ftcms_compute.F90
    ftcms_solve.F90
    ftcms_assimilation.F90
    ftcms.F90
)

# ------------------------------------------------------------
# Include directory (Fortran module path)
# ------------------------------------------------------------
target_include_directories(SOLVER_FTCMS PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR}
)

# ------------------------------------------------------------
# Dependencies
# ------------------------------------------------------------
target_link_libraries(SOLVER_FTCMS PUBLIC
    SOLVER::GOVERNING
    SOLVER::CORE
    SOLVER::CONTROL
    SOLVER::INPUT
    SOLVER::OUTPUT
    SOLVER::LINALG
    SOLVER::SOLVER
)

enable_build_flags(SOLVER_FTCMS)
enable_thirdparty(SOLVER_FTCMS)

# ------------------------------------------------------------
# Namespaced alias
# ------------------------------------------------------------
add_library(SOLVER::FTCMS ALIAS SOLVER_FTCMS)
