add_library(FTDSS OBJECT
    FTDSS.F90
)
target_link_libraries(FTDSS PRIVATE
    Core
    Input
    Thermal
    Hydraulic
    control
    Output
)
enable_build_flags(FTDSS)