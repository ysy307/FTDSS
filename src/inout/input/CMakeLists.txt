# src/input/CMakeLists.txt

set(INPUT_SOURCES
    input_base.F90
    basic/basic_interface.F90
    basic/basic_simulation_setting.F90
    basic/basic_analysis_controls.F90
    basic/basic_geometry_settings.F90
    basic/basic_materials.F90
    basic/basic_solver_settings.F90
    conditions/conditions_interface.F90
    conditions/conditions_time_controls.F90
    conditions/conditions_boundary.F90
    conditions/conditions_initial.F90
    output/output_interface.F90
    output/output_fields.F90
    output/output_history.F90
    output/output_standard.F90
    geometry/geometry_interface.F90
    input_interface.F90
    input.F90
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