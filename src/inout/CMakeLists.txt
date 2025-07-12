add_library(Inout OBJECT
    project_settings.F90
)
target_link_libraries(Inout PRIVATE
    Core
)