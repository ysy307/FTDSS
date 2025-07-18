add_library(Calc_SPH OBJECT
    specific_heat_interface.F90
    specific_heat_base.F90
    specific_heat_3phase.F90
)
target_link_libraries(Calc_SPH PRIVATE
    Core
    Input
)
enable_build_flags(Calc_SPH)