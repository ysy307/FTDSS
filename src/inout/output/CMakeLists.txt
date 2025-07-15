add_library(Output OBJECT
    output_interface.F90
    output_base.F90
    output_observation.F90
    output_overall.F90
    output_system_logger.F90
    output.F90
)
target_link_libraries(Output PRIVATE
    Core
    Inout
    Input
    Thermal
    Domain
    Properties
    Solver
    control
)
enable_build_flags(Output)
enable_thirdparty (Output)