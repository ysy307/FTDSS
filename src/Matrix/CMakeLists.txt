add_library(Matrix OBJECT
    CRS.F90 
    Assemble.F90
)
target_link_libraries(Matrix PRIVATE
    Core
    Domain
    Properties
)
enable_build_flags(Matrix)