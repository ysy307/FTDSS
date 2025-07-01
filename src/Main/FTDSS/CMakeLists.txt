add_library(FTDSS OBJECT
    FTDSS.F90
)
target_link_libraries(FTDSS PRIVATE
    Core
    Input
    Thermal
    Output
)
enable_build_flags(FTDSS)