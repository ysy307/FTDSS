add_library(fdtss OBJECT
    FTDSS.F90
)
target_include_directories(fdtss PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
target_link_libraries(fdtss PUBLIC
    CORE
    INPUT
    THERMAL
    # HYDRAULIC
    CONTROL
    OUTPUT
)
enable_build_flags(fdtss)