add_library(Initial OBJECT
    initial_interface.F90
    initial_uniform.F90
    initial_laplace.F90
    initial_manager.F90
    initial.F90
)
target_link_libraries(Initial PRIVATE
    Core
    Domain
    Input
    Boundary
)
enable_build_flags(Initial)