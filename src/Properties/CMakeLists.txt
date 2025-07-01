add_library(Prop_Thermal OBJECT
    Material_Manager.F90
    Model_Base.F90 
    Model_3Phase.F90
)
target_link_libraries(Prop_Thermal PRIVATE
    Core
    Calc_THC
    Calc_DEN
    Calc_SPH
    Calc_VHC
    Calc_GCC
    Calc_WRF
)
enable_build_flags(Prop_Thermal)