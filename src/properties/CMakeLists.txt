add_library(Properties OBJECT
    materials_manager.F90
    properties_manager.F90
    properties.F90

)
target_link_libraries(Properties PRIVATE
    Core
    Input
    Calc_THC
    Calc_DEN
    Calc_SPH
    Calc_VHC
    Calc_GCC
    Calc_WRF
)
enable_build_flags(Properties)