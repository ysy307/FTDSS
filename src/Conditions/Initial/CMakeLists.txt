add_library(Initial OBJECT
    Interface.F90
    # Base.F90
    # CRS.F90
    IC_Uniform.F90
    IC_Laplace.F90
    IC_Manager.F90    
)
target_link_libraries(Initial PRIVATE
    Core
    # Matrix
    Input
    Domain
    Boundary
)
enable_build_flags(Initial)