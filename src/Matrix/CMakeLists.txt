add_library(Matrix 
    CRS.F90 
    Assemble.F90
)
target_link_libraries(Matrix PRIVATE Core)
enable_build_flags(Core)