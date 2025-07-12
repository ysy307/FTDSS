add_library(Input OBJECT
    # SetProjectPath.F90
    Input.F90
)
target_link_libraries(Input PRIVATE
    Core
    Inout
)
enable_build_flags(Input)
enable_thirdparty (Input)