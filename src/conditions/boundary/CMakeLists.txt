add_library(Boundary OBJECT
    boundary_interface.F90
    boundary_base.F90
    boundary_adiabatic.F90
    boundary_dirichlet.F90
    boundary_manager.F90
    boundary.F90
)
target_link_libraries(Boundary PRIVATE
    Core
    Domain
    Matrix
    Input
)
enable_build_flags(Boundary)