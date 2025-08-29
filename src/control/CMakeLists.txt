# CMakeLists.txt for src/control
#
# Defines components related to simulation control (time stepping, iteration, etc.).

# ====================================================================
# 1. Component Library
# ====================================================================

# --- Control Library ---
# This library contains all core control logic.
set(CONTROL_SOURCES
    time.F90
    iteration.F90
    openmp.F90
    control.F90
)
add_library(Control_Main STATIC ${CONTROL_SOURCES})
target_include_directories(Control_Main PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

# Link to the necessary modules.
# Control logic needs Core for basic types and InOut for reading control settings.
# It may also need Physics to access certain model parameters for convergence checks.
target_link_libraries(Control_Main PUBLIC
    Solver::Core
    Solver::InOut
    Solver::Physics
)
enable_build_flags(Control_Main)

# ====================================================================
# 2. Aggregator Interface Library
# ====================================================================
# Define the interface library with a simple, private name.
add_library(Control_Interface INTERFACE)

# Link the main component library to the interface.
target_link_libraries(Control_Interface INTERFACE
    Control_Main
)

# Create a clean, namespaced ALIAS for other directories to use.
add_library(Solver::Control ALIAS Control_Interface)