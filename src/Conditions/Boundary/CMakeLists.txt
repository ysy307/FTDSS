add_library(Boundary
    Interface.F90
    Base.F90
    Thermal_CRS.F90
)
target_link_libraries(Boundary PRIVATE
    Core
    Matrix
    Input
)
enable_build_flags(Boundary)