add_library(Properties OBJECT
    Material_Manager.F90
    Model_Base.F90 
)
target_link_libraries(Properties PRIVATE
    Core
    Calc_THC
    Calc_DEN
    Calc_SPH
    Calc_VHC
    Calc_GCC
    Calc_WRF
)
enable_build_flags(Properties)