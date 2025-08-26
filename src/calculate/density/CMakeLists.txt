add_library(CALC_DEN STATIC
    density_interface.F90
    density_base.F90
    density_3phase.F90
)
target_include_directories(CALC_DEN PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
target_link_libraries(CALC_DEN PUBLIC
    CORE
    INPUT
)
enable_build_flags(CALC_DEN)
