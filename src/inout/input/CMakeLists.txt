add_library(INPUT OBJECT
    input_interface.F90
    input_basic.F90
    input_conditions.F90
    input_output.F90
    input_geometry.F90
    input.F90
)
target_link_libraries(INPUT PRIVATE
    CORE
    INOUT
)
enable_build_flags(INPUT)
enable_thirdparty (INPUT)