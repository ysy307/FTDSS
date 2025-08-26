add_library(CALC_SPH STATIC
    specific_heat_interface.F90
    specific_heat_base.F90
    specific_heat_3phase.F90
)
target_include_directories(CALC_SPH PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
target_link_libraries(CALC_SPH PUBLIC
    CORE
    INPUT
)
enable_build_flags(CALC_SPH)