add_library(CALC_BLAS STATIC
    blas.F90
)
target_include_directories(CALC_BLAS PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
enable_build_flags(CALC_BLAS)