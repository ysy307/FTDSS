add_library(Initial
    Interface.F90
    # Base.F90
    CRS.F90
)
target_link_libraries(Initial PRIVATE
    Core
    # Matrix
    Input
    Boundary
)
enable_build_flags(Initial)