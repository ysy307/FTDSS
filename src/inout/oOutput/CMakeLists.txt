add_library(Output OBJECT
    Output.F90
    Base.F90
    Observation.F90
    Overall.F90
    system_logger.F90
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