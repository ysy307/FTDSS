add_library(Input
    SetProjectPath.F90
    Input.F90
)
target_link_libraries(Input PRIVATE
    Core
    Conditions
)
enable_build_flags(Input)
enable_thirdparty (Input)