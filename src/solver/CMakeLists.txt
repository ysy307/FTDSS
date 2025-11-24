# CMakeLists.txt for src/solver
#
# Defines the linear solver library, correctly handling dependencies
# between interfaces, implementations, and the factory.

# ====================================================================
# 1. Foundational Interface Library
# ====================================================================

# --- Solver Interface Library ---
# Contains only the abstract interfaces (the "contract").
set(LINEAR_SOLVER_SOURCES
    solver_interface.F90
    precondition_interface.F90
    iterative/solve_bicgstab.F90
    iterative/solve_gmres.F90
    preconditioner/preconditioner_none.F90
    preconditioner/preconditioner_jacobi.F90
)
add_library(Solver_Iterative STATIC ${LINEAR_SOLVER_SOURCES})
target_include_directories(Solver_Iterative PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
target_link_libraries(Solver_Iterative PUBLIC 
    Solver::Core
    Solver::Linalg
)
enable_build_flags(Solver_Iterative)


# ====================================================================
# 3. Main/Factory Library
# ====================================================================
set(SOLVER_MAIN_SOURCES
    solver.F90
)
add_library(Solver_Main STATIC ${SOLVER_MAIN_SOURCES})
target_include_directories(Solver_Main PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
target_link_libraries(Solver_Main PUBLIC
    Solver_Iterative
)
enable_build_flags(Solver_Main)


# ====================================================================
# 4. Aggregator Interface Library
# ====================================================================
add_library(Solver_Interface_Aggregator INTERFACE)

target_link_libraries(Solver_Interface_Aggregator INTERFACE
    Solver_Main
)

# Create a clean, namespaced ALIAS for other directories to use.
add_library(Solver::Solver ALIAS Solver_Interface_Aggregator)