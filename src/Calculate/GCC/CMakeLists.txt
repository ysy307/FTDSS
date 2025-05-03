add_library(Calc_GCC
    GCC.F90
    GCC_NonSegregation_m.F90
    GCC_NonSegregation_Pa.F90
    GCC_Segregation_m.F90
    GCC_Segregation_Pa.F90
)
enable_build_flags(Calc_GCC)