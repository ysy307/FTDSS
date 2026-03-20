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
    iterative/solve_cg.F90
    iterative/solve_bicg.F90
    iterative/solve_cgs.F90
    iterative/solve_bicgstab_l.F90
    iterative/solve_gpbicg.F90
    iterative/solve_tfqmr.F90
    iterative/solve_orthomin.F90
    iterative/solve_jacobi_iter.F90
    iterative/solve_gs.F90
    iterative/solve_sor.F90
    iterative/solve_bicgsafe.F90
    iterative/solve_cr.F90
    iterative/solve_bicr.F90
    iterative/solve_crs.F90
    iterative/solve_bicrstab.F90
    iterative/solve_gpbicr.F90
    iterative/solve_bicrsafe.F90
    iterative/solve_idr_s.F90
    iterative/solve_idr1.F90
    iterative/solve_minres.F90
    iterative/solve_cocg.F90
    iterative/solve_cocr.F90

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
