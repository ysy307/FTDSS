add_library(CALC_VHC STATIC
    heat_capacity_interface.F90
    heat_capacity_3phase.F90
    heat_capacity_3phase_apparent.F90
    heat_capacity_base.F90
)
target_include_directories(CALC_VHC PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
target_link_libraries(CALC_VHC PUBLIC
    CORE
    INPUT
    CALC_DEN
)
enable_build_flags(CALC_VHC)