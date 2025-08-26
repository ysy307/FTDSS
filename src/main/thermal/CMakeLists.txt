add_library(THERMAL STATIC
    thermal_assemble.F90
    thermal_interface.F90
    thermal_crs.F90
    thermal.F90

)
target_include_directories(THERMAL PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
target_link_libraries(THERMAL PUBLIC
    CORE
    INPUT
    DOMAIN
    CALCULATE
    PROPERTIES
    MATRIX
    BOUNDARY
    INITIAL
    SOLVER
)
enable_build_flags(THERMAL)