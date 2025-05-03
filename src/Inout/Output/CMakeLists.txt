add_library(Output
    Output.F90
)
target_link_libraries(Output PRIVATE
    Input
    Core
    Thermal
)
enable_build_flags(Output)
enable_thirdparty (Output)