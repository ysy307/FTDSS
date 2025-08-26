add_library(CALC STATIC
    calculate.F90
)
target_include_directories(CALC PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
target_link_libraries(CALC PUBLIC
    INPUT
    CALC_THC
    CALC_DEN
    CALC_SPH
    CALC_VHC
    CALC_GCC
    CALC_WRF
    CALC_LINALG

)
enable_build_flags(CALC)