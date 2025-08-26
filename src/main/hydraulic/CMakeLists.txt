add_library(HYDRAULIC STATIC
    hydraulic_assemble.F90
    hydraulic_interface.F90
    hydraulic_crs.F90
    hydraulic.F90

)
target_include_directories(HYDRAULIC PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
target_link_libraries(HYDRAULIC PUBLIC
    CORE
    INPUT
    DOMAIN
    PROPERTIES
    BOUNDARY
    INITIAL
    SOLVER
)
enable_build_flags(HYDRAULIC)