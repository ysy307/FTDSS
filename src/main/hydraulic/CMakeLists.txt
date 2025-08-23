add_library(Hydraulic OBJECT
    hydraulic_assemble.F90
    hydraulic_interface.F90
    hydraulic_crs.F90
    hydraulic.F90

)
target_link_libraries(Hydraulic PRIVATE
    Core
    Input
    Domain
    Properties
    Matrix
    Boundary
    Initial
    Solver
)
enable_build_flags(Hydraulic)