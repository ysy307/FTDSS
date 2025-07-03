add_library(Boundary OBJECT
    Interface.F90
    BC_Base.F90
    # Thermal_CRS.F90
    BC_Adiabatic.F90
    BC_Dirichlet.F90
    BC_Manager.F90
)
target_link_libraries(Boundary PRIVATE
    Core
    Domain
    Matrix
    Input
)
enable_build_flags(Boundary)