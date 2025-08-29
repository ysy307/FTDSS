# CMakeLists.txt for src/apps/thermal
#
# Defines the application-specific logic for the thermal solver.

# ====================================================================
# 1. Component Library
# ====================================================================

# --- Thermal Solver Library ---
# This library contains the specific implementation for the thermal solve.
set(APP_THERMAL_SOURCES
    thermal_workspace.F90
    thermal_assemble.F90
    thermal_interface.F90
    thermal_crs.F90
    thermal.F90
)
add_library(App_Thermal STATIC ${APP_THERMAL_SOURCES})
target_include_directories(App_Thermal PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

# Link to all the necessary solver modules using the clean interface libraries.
target_link_libraries(App_Thermal PUBLIC
    Solver::Core
    Solver::Input
    Solver::Domain
    Solver::Physics
    Solver::Conditions
    Solver::Solver
    Solver::Field
)
enable_build_flags(App_Thermal)

# ====================================================================
# 2. Aggregator Interface Library
# ====================================================================
# Define the interface library with a simple, private name.
add_library(Thermal_Interface INTERFACE)

# Link the main component library to the interface.
target_link_libraries(Thermal_Interface INTERFACE
    App_Thermal
)

# Create a clean, namespaced ALIAS. The final executable will link to this.
add_library(Solver::Thermal ALIAS Thermal_Interface)