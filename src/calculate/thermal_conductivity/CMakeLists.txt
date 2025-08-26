add_library(CALC_THC STATIC
    thermal_conductivity_interface.F90
    thermal_conductivity_base.F90
    thermal_conductivity_3phase.F90
)
target_include_directories(CALC_THC PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
target_link_libraries(CALC_THC PUBLIC
    CORE
    INPUT
)
enable_build_flags(CALC_THC)