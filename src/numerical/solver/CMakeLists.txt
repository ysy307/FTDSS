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
    iterative/solve_fgmres.F90

    # --- Preconditioners ---
    preconditioner/preconditioner_none.F90
    preconditioner/preconditioner_jacobi.F90
    preconditioner/preconditioner_iluk.F90
    preconditioner/preconditioner_ssor.F90
    preconditioner/preconditioner_ilut.F90
    preconditioner/preconditioner_saamg.F90
    preconditioner/preconditioner_iluc.F90
    preconditioner/preconditioner_is.F90
    preconditioner/preconditioner_sainv.F90
    preconditioner/preconditioner_hybrid.F90
    preconditioner/preconditioner_adds.F90

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
