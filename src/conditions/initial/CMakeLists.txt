add_library(Initial OBJECT
    ic_interface.F90
    ic_uniform.F90
    ic_laplace.F90
    ic_manager.F90
    initial.F90
)
target_link_libraries(Initial PRIVATE
    Core
    # Matrix
    Input
    Domain
    Boundary
)
enable_build_flags(Initial)