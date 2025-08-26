add_library(CONTROL STATIC
    time.F90
    iteration.F90
    openmp.F90
    control.F90
)
target_include_directories(CONTROL PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
target_link_libraries(CONTROL PUBLIC
    CORE
    INPUT
    CALC
)
enable_build_flags(CONTROL)