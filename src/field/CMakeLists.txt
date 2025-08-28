add_library(FIELD OBJECT
    jacobian_matrix.F90
    residual_vector.F90
    field.F90
)
target_include_directories(FIELD PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
target_link_libraries(FIELD PUBLIC
    CORE
    # INPUT
    # MESH
)
enable_build_flags(FIELD)
enable_thirdparty (FIELD)