add_library(BOUNDARY STATIC
    boundary_interface.F90
    boundary_base.F90
    boundary_adiabatic.F90
    boundary_dirichlet.F90
    boundary_manager.F90
    boundary.F90
)
target_include_directories(BOUNDARY PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
target_link_libraries(BOUNDARY PUBLIC
    CORE
    DOMAIN
    MATRIX
    INPUT
)
enable_build_flags(BOUNDARY)