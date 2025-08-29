# src/input/CMakeLists.txt

set(INPUT_SOURCES
    input_interface.F90
    input_basic.F90
    input_conditions.F90
    input_output.F90
    input_geometry.F90
    input.F90
    project_settings.F90 # project_settingsもinputの責務とする
)
add_library(Input_Main STATIC ${INPUT_SOURCES})
target_include_directories(Input_Main PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

target_link_libraries(Input_Main PUBLIC
    Solver::Core
)
enable_build_flags(Input_Main)
enable_thirdparty(Input_Main)

add_library(Input_Interface INTERFACE)
target_link_libraries(Input_Interface INTERFACE Input_Main)
add_library(Solver::Input ALIAS Input_Interface)