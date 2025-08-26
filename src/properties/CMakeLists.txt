add_library(PROPERTIES STATIC
    materials_manager.F90
    properties_manager.F90
    properties.F90

)
target_include_directories(PROPERTIES PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
target_link_libraries(PROPERTIES PUBLIC
    CORE
    INPUT
    CALC
    CONTROL
)
enable_build_flags(PROPERTIES)