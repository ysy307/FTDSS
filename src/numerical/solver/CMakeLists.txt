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
    precondition_interface.F90

    # --- Iterative solvers ---
    iterative/solve_bicgstab.F90
    iterative/solve_gmres.F90

    # --- Preconditioners ---
    preconditioner/preconditioner_none.F90
    preconditioner/preconditioner_jacobi.F90
    preconditioner/preconditioner_iluk.F90

    # --- Factory / Entry ---
    solver.F90
)

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
