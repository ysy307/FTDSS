add_library(Calc_DEN OBJECT
    density_interface.F90
    density_Base.F90
    density_3Phase.F90
)
target_link_libraries(Calc_DEN PRIVATE
    Core
    Input
)
enable_build_flags(Calc_DEN)
