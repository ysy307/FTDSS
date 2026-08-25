# ============================================================
# Solver Library (single static archive)
# ============================================================
add_library(SOLVER_SOLVE STATIC)

# ------------------------------------------------------------
# Sources
# ------------------------------------------------------------
target_sources(SOLVER_SOLVE PRIVATE
    # --- Interfaces ---
    solver_interface.F90

    # --- Factory / Entry ---
    solver.F90
)

# --- PETSc KSP back-end (LINEAR_SOLVER_TYPES%PETSC, id 27) ---
# Optional: a toolchain without a PETSc install still configures, and
# create_solver then reports NOT_IMPLEMENTED for id 27.
if(ENABLE_PETSC)
    find_package(PkgConfig)
    if(PkgConfig_FOUND)
        set(FTCMS_PETSC_PREFIX "${PROJECT_SOURCE_DIR}/third_party/.local/${COMPILER_DIR}")
        set(ENV{PKG_CONFIG_PATH} "${FTCMS_PETSC_PREFIX}/lib/pkgconfig:$ENV{PKG_CONFIG_PATH}")
        pkg_check_modules(PETSC IMPORTED_TARGET PETSc)
    endif()

    if(PETSC_FOUND)
        target_sources(SOLVER_SOLVE PRIVATE petsc/solve_petsc.F90)
        target_link_libraries(SOLVER_SOLVE PUBLIC PkgConfig::PETSC)
        target_compile_definitions(SOLVER_SOLVE PUBLIC _PETSC)
        message(STATUS "PETSc KSP back-end enabled (${PETSC_VERSION})")
    else()
        message(STATUS "PETSc not found; the KSP back-end is disabled")
    endif()
endif()

# ------------------------------------------------------------
# Fortran module output
# ------------------------------------------------------------
target_include_directories(SOLVER_SOLVE PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR}
)

# ------------------------------------------------------------
# Dependencies
# ------------------------------------------------------------
target_link_libraries(SOLVER_SOLVE PUBLIC
    SOLVER::CORE
    SOLVER::LINALG
)

enable_build_flags(SOLVER_SOLVE)
enable_thirdparty(SOLVER_SOLVE)

# ------------------------------------------------------------
# Namespaced alias
# ------------------------------------------------------------
add_library(SOLVER::SOLVER ALIAS SOLVER_SOLVE)
