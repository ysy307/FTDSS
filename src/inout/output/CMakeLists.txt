add_library(OUTPUT STATIC
    output_interface.F90
    output_base.F90
    output_observation.F90
    output_overall_base.F90
    output_overall_vtk.F90
    output_overall_vtu.F90
    output_system_logger.F90
    output.F90
)
target_include_directories(OUTPUT PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
target_link_libraries(OUTPUT PUBLIC
    CORE
    INOUT
    INPUT
    THERMAL
    DOMAIN
    PROPERTIES
    SOLVER
    CONTROL
)
enable_build_flags(OUTPUT)
enable_thirdparty (OUTPUT)