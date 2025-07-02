add_library(Time OBJECT
    Time.F90
)
target_link_libraries(Time PRIVATE
    Core
    Input
)
enable_build_flags(Time)