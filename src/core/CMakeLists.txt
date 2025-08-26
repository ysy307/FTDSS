# --------------------------------------------------------------------
# C++/C VTKラッパーライブラリの作成
# --------------------------------------------------------------------
add_library(VTK_C_WRAPPER STATIC
    vtk/reader_vtk.cpp
    vtk/reader_vtu.cpp
    vtk/c_wrapper.cpp
)
enable_build_flags(VTK_C_WRAPPER)
target_link_libraries(VTK_C_WRAPPER PUBLIC
    VTK::IOXML
    VTK::CommonCore
    VTK::CommonDataModel
    VTK::IOLegacy
)
set_target_properties(VTK_C_WRAPPER PROPERTIES CXX_STANDARD 17)

# --------------------------------------------------------------------
# Fortran/C Core STATICライブラリ
# C と Fortran をまとめてコンパイル
# --------------------------------------------------------------------
add_library(CORE_UTILS OBJECT
    c_utils/signal_flag.F90
    c_utils/system_info.F90
    c_utils/memory_stats.F90
    c_utils/c_utils.F90
    fortran_utils/system_info_wrapper.F90
    fortran_utils/memory_stats_wrapper.F90  
    fortran_utils/signal_flag_wrapper.F90
    fortran_utils/fortran_utils.F90
    error.F90
    check_nan.F90
    check_range.F90
)
target_include_directories(CORE_UTILS PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
enable_build_flags(CORE_UTILS)
enable_thirdparty(CORE_UTILS)

add_library(CORE_TYPES OBJECT
    allocate.F90
    deallocate.F90
    string_utils.F90
    types/vector.F90
    types/array.F90
    types/pointer.F90
    types/variable.F90
    types/gauss.F90
    types/types.F90
)
target_include_directories(CORE_TYPES PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
enable_build_flags(CORE_TYPES)
enable_thirdparty(CORE_TYPES)


add_library(CORE_VTK OBJECT
    vtk/vtk_wrapper.F90
    vtk/vtu_wrapper.F90
    vtk/vtk_constants.F90
    vtk/vtk.F90
)
target_include_directories(CORE_VTK PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
target_link_libraries(CORE_VTK PRIVATE VTK_C_WRAPPER)
enable_build_flags(CORE_VTK)
enable_thirdparty(CORE_VTK)


add_library(CORE STATIC
    $<TARGET_OBJECTS:CORE_UTILS>
    $<TARGET_OBJECTS:CORE_TYPES>
    $<TARGET_OBJECTS:CORE_VTK>
    unique.F90
    core.F90
)

target_include_directories(CORE PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
target_link_libraries(CORE PRIVATE VTK_C_WRAPPER)
enable_build_flags(CORE)
enable_thirdparty(CORE)
