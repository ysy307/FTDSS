add_library(Conditions
    Fix_Boundary_Conditions.F90
    Fix_Initial_Conditions.F90
)
target_link_libraries(Conditions PRIVATE
    Core
    Matrix
)
enable_build_flags(Conditions)