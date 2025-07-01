add_library(Prop_Thermal OBJECT
    Material_Manager.F90
    Model_Base.F90 
    Model_3Phase.F90
)
target_link_libraries(Prop_Thermal PRIVATE
    Core
    Calc_THC
    # Domain
)
enable_build_flags(Prop_Thermal)