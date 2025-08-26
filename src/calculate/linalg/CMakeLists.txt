add_library(CALC_LINALG STATIC
    matrix_ops.F90
    vector_ops.F90
    matvec.F90
    linalg.F90
)
target_link_libraries(CALC_LINALG PUBLIC
    CORE
)
target_include_directories(CALC_LINALG PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
enable_build_flags(CALC_LINALG)