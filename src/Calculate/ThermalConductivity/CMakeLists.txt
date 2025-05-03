add_library(Calc_THC
        ThermalConductivity.F90
        ThermalConductivity_3Phase.F90
)
target_link_libraries(Calc_THC PRIVATE 
    Core
    Input
)
enable_build_flags(Calc_THC)