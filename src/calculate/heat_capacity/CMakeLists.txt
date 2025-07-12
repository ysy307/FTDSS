add_library(Calc_VHC OBJECT
    heat_capacity_interface.F90
    heat_capacity_3phase.F90
    heat_capacity_3phase_apparent.F90
    heat_capacity_base.F90
)
target_link_libraries(Calc_VHC PRIVATE
    Core
    Input
    Calc_DEN
)
enable_build_flags(Calc_VHC)