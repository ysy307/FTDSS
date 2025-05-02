add_library(Calc_SPH
    SpecificHeat.F90
    SpecificHeat_3Phase.F90
)
target_link_libraries(Calc_SPH PRIVATE
    Core
    Input
)
enable_build_flags(Calc_SPH)