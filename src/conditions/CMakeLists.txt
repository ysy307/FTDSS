# CMakeLists.txt for src/conditions
#
# Defines components related to boundary and initial conditions.

# ====================================================================
# 1. Component Libraries
# ====================================================================

# --- Boundary Condition Library ---
set(CONDITIONS_BOUNDARY_SOURCES
    boundary/boundary_interface.F90
    boundary/boundary_base.F90
    boundary/boundary_adiabatic.F90
    boundary/boundary_dirichlet.F90
    boundary/boundary_manager.F90
    boundary/boundary.F90
)
add_library(Conditions_Boundary STATIC ${CONDITIONS_BOUNDARY_SOURCES})
target_include_directories(Conditions_Boundary PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

# Boundary conditions need info from Core, Domain, InOut, and Field.
target_link_libraries(Conditions_Boundary PUBLIC
    Solver::Core
    Solver::Input
    Solver::Control
    # Solver::Field
)
enable_build_flags(Conditions_Boundary)

# # --- Initial Condition Library ---
# set(CONDITIONS_INITIAL_SOURCES
#     initial/initial_interface.F90
#     initial/initial_uniform.F90
#     initial/initial_laplace.F90
#     initial/initial_manager.F90
#     initial/initial.F90
# )
# add_library(Conditions_Initial STATIC ${CONDITIONS_INITIAL_SOURCES})
# target_include_directories(Conditions_Initial PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

# # Initial conditions depend on Boundary conditions, plus other modules.
# target_link_libraries(Conditions_Initial PUBLIC
#     Conditions_Boundary
#     Solver::Core
#     Solver::Domain
#     Solver::Input
#     Solver::Control
#     Solver::Field
# )
# enable_build_flags(Conditions_Initial)


# ====================================================================
# 2. Aggregator Interface Library
# ====================================================================
add_library(Conditions_Interface INTERFACE)

# Link both component libraries to the interface.
target_link_libraries(Conditions_Interface INTERFACE
    Conditions_Boundary
    # Conditions_Initial
)

# Create a clean, namespaced ALIAS for other directories to use.
add_library(Solver::Conditions ALIAS Conditions_Interface)