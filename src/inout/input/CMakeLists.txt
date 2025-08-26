add_library(INPUT STATIC
    input_interface.F90
    input_basic.F90
    input_conditions.F90
    input_output.F90
    input_geometry.F90
    input.F90
)
target_include_directories(INPUT PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
target_link_libraries(INPUT PUBLIC
    CORE
    INOUT
)
enable_build_flags(INPUT)
enable_thirdparty (INPUT)