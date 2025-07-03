add_library(Calc_BLAS  OBJECT
    BLAS.F90
)
target_link_libraries(Calc_BLAS PRIVATE
    Matrix
)
enable_build_flags(Calc_BLAS)