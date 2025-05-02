add_library(Calc_DEN
    Density.F90
    Density_3Phase.F90
)
target_link_libraries(Calc_DEN PRIVATE
    Core
    Input
)
enable_build_flags(Calc_DEN)
