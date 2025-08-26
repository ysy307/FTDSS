add_library(INOUT STATIC
    project_settings.F90
)
target_include_directories(INOUT PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
target_link_libraries(INOUT PRIVATE
    CORE
)