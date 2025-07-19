add_library(control OBJECT
    time.F90
    iteration.F90
    openmp.F90
    control.F90
)
target_link_libraries(control PRIVATE
    Core
    Input
)
enable_build_flags(control)