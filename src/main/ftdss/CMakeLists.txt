add_library(FTDSS STATIC
    FTDSS.F90
)
target_include_directories(FTDSS PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
target_link_libraries(FTDSS PUBLIC
    CORE
    INPUT
    THERMAL
    HYDRAULIC
    CONTROL
    OUTPUT
)
enable_build_flags(FTDSS)