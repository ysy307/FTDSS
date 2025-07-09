add_library(Thermal OBJECT
    thermal_assemble.f90
    Thermal.F90
    Thermal_3Phase.F90

)
target_link_libraries(Thermal PRIVATE
    Core
    Input
    Domain
    Calc_THC
    Calc_VHC
    Calc_DEN
    Calc_SPH
    Properties
    Matrix
    Boundary
    Initial
    Solver
)
enable_build_flags(Thermal)