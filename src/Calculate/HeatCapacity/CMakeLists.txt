add_library(Calc_VHC
    Interface.F90
    Base.F90
    3Phase_Apparent.F90
)
target_link_libraries(Calc_VHC PRIVATE
    Core
    Input
    Calc_Ice
    Calc_GCC
    Calc_DEN
)
enable_build_flags(Calc_VHC)