add_library(Calc_THC OBJECT
    Interface.F90
    Base.F90
    3Phase.F90
)
target_link_libraries(Calc_THC PRIVATE 
    Core
    Input
)
enable_build_flags(Calc_THC)