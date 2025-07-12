add_library(Matrix OBJECT
    # adjacency.F90
    # multicoloring.F90
    coo.F90
    crs.F90
    # RCM.F90
    # Assemble.F90
)
target_link_libraries(Matrix PRIVATE
    Core
    Domain
)
enable_build_flags(Matrix)
enable_thirdparty (Matrix)