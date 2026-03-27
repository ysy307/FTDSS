# ============================================================
# Control Library
# ============================================================

add_library(SOLVER_CONTROL STATIC)

# ------------------------------------------------------------
# Sources
# ------------------------------------------------------------
target_sources(SOLVER_CONTROL PRIVATE
    acceleration/acceleration_interface.F90
    acceleration/aitken.F90
    acceleration/anderson.F90

    time/ats.F90
    time/time.F90
    time/time_profiler.F90

    iteration/setting/setting_interface.F90
    iteration/convergence/convergence_interface.F90
    iteration/convergence/convergence_criterion.F90
    iteration/convergence/convergence_control.F90
    iteration/strategy/strategy_interface.F90
    iteration/strategy/strategy_procedures.F90
    iteration/iteration_manager.F90
    iteration/iteration.F90

    scheduler/scheduler_manager.F90

    parallel/openmp.F90
    parallel/parallel.F90

    control_manager.F90
    control.F90
)

# ------------------------------------------------------------
# Module include directory (Fortran .mod output)
# ------------------------------------------------------------
target_include_directories(SOLVER_CONTROL PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR}
)

# ------------------------------------------------------------
# Dependencies
# ------------------------------------------------------------
target_link_libraries(SOLVER_CONTROL PUBLIC
    SOLVER::CORE
    SOLVER::LINALG
)

# ------------------------------------------------------------
# Project flags (existing macro kept)
# ------------------------------------------------------------
enable_build_flags(SOLVER_CONTROL)
enable_thirdparty(SOLVER_CONTROL)

# ------------------------------------------------------------
# Namespaced alias
# ------------------------------------------------------------
add_library(SOLVER::CONTROL ALIAS SOLVER_CONTROL)
