add_library(Properties OBJECT
    materials_manager.F90
    properties_manager.F90
    properties.F90

)
target_link_libraries(Properties PRIVATE
    Core
    Input
    Calc
    control
)
enable_build_flags(Properties)