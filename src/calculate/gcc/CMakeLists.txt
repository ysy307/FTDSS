add_library(CALC_GCC STATIC
    gcc_interface.F90
    gcc_base.F90
    gcc_non_segregation_m.F90
    gcc_non_segregation_pa.F90
    gcc_segregation_m.F90
    gcc_segregation_pa.F90
)
target_include_directories(CALC_GCC PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
target_link_libraries(CALC_GCC PUBLIC
    CORE
    INPUT
)
enable_build_flags(CALC_GCC)