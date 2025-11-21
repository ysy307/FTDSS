# CMakeLists.txt for src/linalg
#
# Defines the linear algebra library.

# ====================================================================
# 1. Component Library
# ====================================================================

# --- Linear Algebra Library ---
set(LINALG_SOURCES
    mkl_interface.F90
    mkl_backend.F90
    vector.F90
    vector_operations.F90
    # matrix_factory.F90
    # matrix_ops.F90
    linalg.F90
)
add_library(Linalg_Main STATIC ${LINALG_SOURCES})
target_include_directories(Linalg_Main PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

# Link to the necessary modules.
# Linear algebra routines depend on the basic types defined in Core.
target_link_libraries(Linalg_Main PUBLIC
    Solver::Core
)
enable_build_flags(Linalg_Main)

# ====================================================================
# 2. Aggregator Interface Library
# ====================================================================
# Define the interface library with a simple, private name.
add_library(Linalg_Interface INTERFACE)

# Link the main component library to the interface.
target_link_libraries(Linalg_Interface INTERFACE
    Linalg_Main
)
enable_build_flags(Linalg_Interface)

# Create a clean, namespaced ALIAS for other directories to use.
add_library(Solver::Linalg ALIAS Linalg_Interface)