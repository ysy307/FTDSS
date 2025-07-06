add_library(Output OBJECT
    # SystemInfo.c
    Output.F90
    Base.F90
    Observation.F90
    Overall.F90
    SystemLogger.F90
)
target_link_libraries(Output PRIVATE
    Input
    Core
    Thermal
    Domain
    Properties
    Solver
    Time
)
enable_build_flags(Output)
enable_thirdparty (Output)