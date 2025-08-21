add_library(Matrix OBJECT
matrix_base.F90
    matrix_dense.F90
    matrix_coo.F90
    matrix_crs.F90
    matrix.F90
)
target_link_libraries(Matrix PRIVATE
    Core
    Domain
    Calc
)
enable_build_flags(Matrix)
enable_thirdparty (Matrix)