# CMakeLists.txt for src/field
#
# Defines components related to the field variables, Jacobian matrix,
# and residual vector assembly.

# ====================================================================
# 1. Component Library
# ====================================================================

# --- Field Library ---
# This library contains the logic for assembling the global system of equations.
set(FIELD_SOURCES
    jacobian_matrix.F90
    residual_vector.F90
    field.F90
)
add_library(Field_Main STATIC ${FIELD_SOURCES})
target_include_directories(Field_Main PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

# Link to the necessary modules.
# The Field component is a high-level orchestrator that requires information
# from Core, Domain, and Physics to assemble the system.
target_link_libraries(Field_Main PUBLIC
    Solver::Core
    Solver::Domain
    Solver::Physics
)
enable_build_flags(Field_Main)
enable_thirdparty (Field_Main)

# ====================================================================
# 2. Aggregator Interface Library
# ====================================================================
# Define the interface library with a simple, private name.
add_library(Field_Interface INTERFACE)

# Link the main component library to the interface.
target_link_libraries(Field_Interface INTERFACE
    Field_Main
)

# Create a clean, namespaced ALIAS for other directories to use.
add_library(Solver::Field ALIAS Field_Interface)