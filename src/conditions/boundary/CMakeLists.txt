add_library(Boundary OBJECT
    bc_interface.F90
    bc_base.F90
    bc_adiabatic.F90
    bc_dirichlet.F90
    bc_manager.F90
    boundary.F90
)
target_link_libraries(Boundary PRIVATE
    Core
    Domain
    Matrix
    Input
)
enable_build_flags(Boundary)