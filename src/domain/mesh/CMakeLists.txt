add_library(MESH STATIC
    mesh_interface.F90
    element/element_interface.F90
    element/element_triangle_first.F90
    element/element_triangle_second.F90
    element/element_square_first.F90
    element/element_square_second.F90
    element/element_factory.F90
    element/element.F90
    side/side_interface.F90
    side/side_first.F90
    side/side_second.F90
    side/side_factory.F90
    side/side.F90
    mesh.F90
)
target_include_directories(MESH PUBLIC
    ${CMAKE_Fortran_MODULE_DIRECTORY}
)
target_link_libraries(MESH PUBLIC
    CORE
    INPUT
)
enable_build_flags(MESH)
enable_thirdparty (MESH)