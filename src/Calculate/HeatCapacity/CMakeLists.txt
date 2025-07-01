add_library(Calc_VHC OBJECT
    Interface.F90
    Base.F90
    3Phase.F90
    3Phase_Apparent.F90
)
target_link_libraries(Calc_VHC PRIVATE
    Core
    Input
    Calc_GCC
    Calc_WRF
    Calc_DEN
)
enable_build_flags(Calc_VHC)