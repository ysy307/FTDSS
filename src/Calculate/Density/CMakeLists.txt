add_library(Calc_DEN
    Interface.F90
    Base.F90
    3Phase.F90
)
target_link_libraries(Calc_DEN PRIVATE
    Core
    Input
)
enable_build_flags(Calc_DEN)
