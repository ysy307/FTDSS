add_library(Thermal
    Thermal.F90
    Thermal_3Phase.F90
)
target_link_libraries(Thermal PRIVATE
    Core
    Input
    Calc_Ice
    Calc_THC
    Calc_VHC
    Calc_DEN
    Calc_SPH
    Matrix
    Boundary
    Solver
)
enable_build_flags(Thermal)