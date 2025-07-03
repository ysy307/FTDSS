add_library(Calc_SPH OBJECT
    Interface.F90
    Base.F90
    3Phase.F90
)
target_link_libraries(Calc_SPH PRIVATE
    Core
    Input
)
enable_build_flags(Calc_SPH)