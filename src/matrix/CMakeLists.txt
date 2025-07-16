add_library(Matrix OBJECT
    coo.F90
    crs.F90
    matrix.F90
)
target_link_libraries(Matrix PRIVATE
    Core
    Domain
)
enable_build_flags(Matrix)
enable_thirdparty (Matrix)