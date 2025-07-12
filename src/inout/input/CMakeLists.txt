add_library(Input OBJECT
    input_interface.F90
    input_basic.F90
)
target_link_libraries(Input PRIVATE
    Core
    Inout
)
enable_build_flags(Input)
enable_thirdparty (Input)