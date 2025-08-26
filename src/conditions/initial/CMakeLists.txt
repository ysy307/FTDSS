add_library(INITIAL STATIC
    initial_interface.F90
    initial_uniform.F90
    initial_laplace.F90
    initial_manager.F90
    initial.F90
)
target_include_directories(INITIAL PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
target_link_libraries(INITIAL PUBLIC
    CORE
    DOMAIN
    INPUT
    BOUNDARY
)
enable_build_flags(INITIAL)