add_library(Calc_GCC OBJECT
    GCC.F90
    GCC_NonSegregation_m.F90
    GCC_NonSegregation_Pa.F90
    GCC_Segregation_m.F90
    GCC_Segregation_Pa.F90
    GCC_Base.F90
)
target_link_libraries(Calc_GCC PRIVATE
    Input
)
enable_build_flags(Calc_GCC)