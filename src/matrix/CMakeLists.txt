add_library(MATRIX STATIC
    matrix_base.F90
    matrix_dense.F90
    matrix_coo.F90
    matrix_crs.F90
    matrix.F90
)
target_include_directories(MATRIX PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
target_link_libraries(MATRIX PUBLIC
    CORE
    DOMAIN
    CALC
)
enable_build_flags(MATRIX)
enable_thirdparty (MATRIX)