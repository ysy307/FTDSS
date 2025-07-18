add_library(Calc OBJECT
    calculate.F90
)
target_link_libraries(Calc PRIVATE
    Input
    Calc_THC
    Calc_DEN
    Calc_SPH
    Calc_VHC
    Calc_GCC
    Calc_WRF
    Calc_BLAS

)
enable_build_flags(Calc)