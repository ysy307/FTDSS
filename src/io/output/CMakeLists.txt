# ============================================================
# Output Library
# ============================================================
add_library(SOLVER_OUTPUT STATIC)

# ------------------------------------------------------------
# Sources
# ------------------------------------------------------------
target_sources(SOLVER_OUTPUT PRIVATE
    output_base.F90
    logging/logging_interface.F90
    logging/system_logger.F90
    observation/observation_interface.F90
    observation/observation_base.F90
    observation/observation_point.F90
    overall/overall_interface.F90
    overall/overall_vtk.F90
    overall/overall_vtu.F90
    output_manager.F90
    output.F90
)

# ------------------------------------------------------------
# Fortran module output directory
# ------------------------------------------------------------
target_include_directories(SOLVER_OUTPUT PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR}
)

# ------------------------------------------------------------
# Dependencies
# ------------------------------------------------------------
target_link_libraries(SOLVER_OUTPUT PUBLIC
    SOLVER::CORE
    SOLVER::DOMAIN
)

enable_build_flags(SOLVER_OUTPUT)
enable_thirdparty(SOLVER_OUTPUT)

# ------------------------------------------------------------
# Namespaced alias
# ------------------------------------------------------------
add_library(SOLVER::OUTPUT ALIAS SOLVER_OUTPUT)
