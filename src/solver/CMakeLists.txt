# CMakeLists.txt for src/solver
#
# Defines the linear solver library, correctly handling dependencies
# between interfaces, implementations, and the factory.

# ====================================================================
# 1. Foundational Interface Library
# ====================================================================

# --- Solver Interface Library ---
# Contains only the abstract interfaces (the "contract").
set(SOLVER_INTERFACE_SOURCES
    solve_interface.F90
)
add_library(Solver_Interface STATIC ${SOLVER_INTERFACE_SOURCES})
target_include_directories(Solver_Interface PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
# The interface itself depends on Core and Linalg for types.
target_link_libraries(Solver_Interface PUBLIC Solver::Core Solver::Linalg)
enable_build_flags(Solver_Interface)


# ====================================================================
# 2. Concrete Implementation Libraries
# ====================================================================

# --- Direct Solvers ---
set(SOLVER_DIRECT_SOURCES
    direct/solve_lu.F90
)
add_library(Solver_Direct STATIC ${SOLVER_DIRECT_SOURCES})
target_include_directories(Solver_Direct PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
# Direct solvers depend on the Interface "contract".
target_link_libraries(Solver_Direct PUBLIC Solver_Interface)
enable_build_flags(Solver_Direct)

# --- Iterative Solvers ---
set(SOLVER_ITERATIVE_SOURCES
    iterative/solve_bicgstab.F90
    iterative/prconditioner/solve_preconditioner.F90
)
add_library(Solver_Iterative STATIC ${SOLVER_ITERATIVE_SOURCES})
target_include_directories(Solver_Iterative PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
# Iterative solvers also depend on the Interface "contract".
target_link_libraries(Solver_Iterative PUBLIC Solver_Interface)
enable_build_flags(Solver_Iterative)


# ====================================================================
# 3. Main/Factory Library
# ====================================================================

# --- Solver Main/Factory Library ---
set(SOLVER_MAIN_SOURCES
    solver_factory.F90
    solver.F90
)
add_library(Solver_Main STATIC ${SOLVER_MAIN_SOURCES})
target_include_directories(Solver_Main PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
# The factory depends on the Interface and ALL concrete implementations.
target_link_libraries(Solver_Main PUBLIC
    Solver::Input
    Solver_Interface
    Solver_Direct
    Solver_Iterative
)
enable_build_flags(Solver_Main)


# ====================================================================
# 4. Aggregator Interface Library
# ====================================================================
add_library(Solver_Interface_Aggregator INTERFACE)

# Link the main factory/manager library to the interface.
# This transitively links all other components.
target_link_libraries(Solver_Interface_Aggregator INTERFACE
    Solver_Main
)

# Create a clean, namespaced ALIAS for other directories to use.
add_library(Solver::Solver ALIAS Solver_Interface_Aggregator)