# src/output/CMakeLists.txt
set(INOUT_OUTPUT_SOURCES
    output_interface.F90
    output_base.F90
    output_observation.F90
    output_overall_base.F90
    output_overall_vtk.F90
    output_overall_vtu.F90
    output_system_logger.F90
    output.F90
)
add_library(Output_Main STATIC ${INOUT_OUTPUT_SOURCES})
target_include_directories(Output_Main PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

# Outputは、データを書き出すためにCore, Domain, Physics等を知る必要がある
target_link_libraries(Output_Main PUBLIC
    Solver::Core
    Solver::Input
    Solver::Domain
    Solver::Physics
    Solver::Control
)
enable_build_flags(Output_Main)
enable_thirdparty(Output_Main)

add_library(Output_Interface INTERFACE)
target_link_libraries(Output_Interface INTERFACE Output_Main)
add_library(Solver::Output ALIAS Output_Interface)