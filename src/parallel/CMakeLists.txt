set(Parallel_SOURCES
    communicator.F90
    parallel.F90
)
add_library(Parallel_Main STATIC ${Parallel_SOURCES})
target_include_directories(Parallel_Main PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
# The manager uses all other domain components.
target_link_libraries(Parallel_Main PUBLIC
    Solver::Core
    Solver::Input
)
enable_build_flags(Parallel_Main)
enable_thirdparty(Parallel_Main)


# ====================================================================
# 2. Aggregator Interface Library
# ====================================================================
add_library(Parallel_Interface INTERFACE)

# Link all component libraries to this target.
target_link_libraries(Parallel_Interface INTERFACE
    Parallel_Main
)

# Create a clean, namespaced ALIAS for other directories to use.
add_library(Solver::Parallel ALIAS Parallel_Interface)