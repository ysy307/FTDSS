# ============================================================
# Output Library (single static archive)
# ============================================================
add_library(SOLVER_OUTPUT STATIC)

# ------------------------------------------------------------
# Sources
# ------------------------------------------------------------
target_sources(SOLVER_OUTPUT
    PRIVATE
        output_interface.F90
        output_base.F90
        output_observation.F90
        output_overall_base.F90
        output_overall_vtk.F90
        output_overall_vtu.F90
        output_system_logger.F90
        output.F90
)

# ------------------------------------------------------------
# Fortran module output directory
# ------------------------------------------------------------
target_include_directories(SOLVER_OUTPUT
    PUBLIC
        ${CMAKE_CURRENT_BINARY_DIR}
)

# ------------------------------------------------------------
# Dependencies
# ------------------------------------------------------------
target_link_libraries(SOLVER_OUTPUT
    PUBLIC
        SOLVER::CORE
        SOLVER::INPUT
        SOLVER::DOMAIN
        # Solver::Physics
        SOLVER::CONTROL
)

enable_build_flags(SOLVER_OUTPUT)
enable_thirdparty(SOLVER_OUTPUT)

# ------------------------------------------------------------
# Namespaced alias
# ------------------------------------------------------------
add_library(SOLVER::OUTPUT ALIAS SOLVER_OUTPUT)