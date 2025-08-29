# CMakeLists.txt for src/inout
#
# This file defines the components related to input and output operations.
# It creates individual static libraries for each sub-component (input, output, etc.)
# and then bundles them into a single, public-facing interface library "Solver::InOut".

# ====================================================================
# 1. Component Libraries
# ====================================================================

# --- Project Settings Library ---
# A small component for project-wide settings, including file paths.
set(INOUT_PROJECT_SETTINGS_SOURCES
    project_settings.F90
)
add_library(InOut_ProjectSettings STATIC ${INOUT_PROJECT_SETTINGS_SOURCES})
target_include_directories(InOut_ProjectSettings PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
# This depends on the Core library for basic types.
target_link_libraries(InOut_ProjectSettings PUBLIC Solver::Core)
enable_build_flags(InOut_ProjectSettings)


# --- Input Library ---
# Handles reading all input files and configuring the simulation.
set(INOUT_INPUT_SOURCES
    input/input_interface.F90
    input/input_basic.F90
    input/input_conditions.F90
    input/input_output.F90
    input/input_geometry.F90
    input/input.F90
)
add_library(InOut_Input STATIC ${INOUT_INPUT_SOURCES})
target_include_directories(InOut_Input PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
# Depends on ProjectSettings (for paths) and Core.
target_link_libraries(InOut_Input PUBLIC
    InOut_ProjectSettings
    Solver::Core
)
enable_build_flags(InOut_Input)

# --- Output Library ---
# Handles writing all output files (VTK, logs, observations, etc.).
set(INOUT_OUTPUT_SOURCES
    output/output_interface.F90
    output/output_base.F90
    output/output_observation.F90
    output/output_overall_base.F90
    output/output_overall_vtk.F90
    output/output_overall_vtu.F90
    output/output_system_logger.F90
    output/output.F90
)
add_library(InOut_Output STATIC ${INOUT_OUTPUT_SOURCES})
target_include_directories(InOut_Output PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
# Depends on ProjectSettings (for paths), Input (for settings), and other solver modules.
target_link_libraries(InOut_Output PUBLIC
    InOut_ProjectSettings
    InOut_Input
    Solver::Core
    Solver::Domain
    Solver::Physics
    Solver::Control
)
enable_build_flags(InOut_Output)



# ====================================================================
# 2. Aggregator Interface Library
# ====================================================================
# This section bundles all the above components into a single, easy-to-use target.

# Step 1: Define the interface library with a simple, private name.
add_library(InOut_Interface INTERFACE)

# Step 2: Link all the component libraries created in this file to the interface.
target_link_libraries(InOut_Interface INTERFACE
    InOut_Input
    InOut_Output
    InOut_ProjectSettings
)

# Step 3: Create a clean, namespaced ALIAS. This is the public target that
# other parts of the project (like 'apps') should link against.
add_library(Solver::InOut ALIAS InOut_Interface)