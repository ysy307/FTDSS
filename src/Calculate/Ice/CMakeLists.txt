add_library(Calc_Ice OBJECT
    Ice.F90
    Ice_TRM.F90
    Ice_GCC.F90
    Ice_EXP.F90
)
target_link_libraries(Calc_Ice PRIVATE
    Core
    Input
    Calc_GCC
    Calc_WRF
    Calc_DEN
)
enable_build_flags(Calc_Ice)