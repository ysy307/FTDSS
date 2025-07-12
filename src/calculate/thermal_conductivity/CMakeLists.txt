add_library(Calc_THC OBJECT
    thermal_conductivity_interface.F90
    thermal_conductivity_base.F90
    thermal_conductivity_3phase.F90
)
target_link_libraries(Calc_THC PRIVATE 
    Core
    Input
)
enable_build_flags(Calc_THC)