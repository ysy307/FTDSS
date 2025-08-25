# --------------------------------------------------------------------
# C++/C VTKラッパーライブラリの作成
#    VTKへの依存をこのライブラリに閉じ込めます。
# --------------------------------------------------------------------
add_library(VTK_C_WRAPPER STATIC
    vtk/reader_vtk.cpp
    vtk/reader_vtu.cpp
    vtk/c_wrapper.cpp
)
# VTKのインクルードディレクトリとライブラリをリンク
enable_build_flags(VTK_C_WRAPPER)

# <--- [新規追加] C++ラッパーが公式VTKライブラリに依存することを明記します
target_link_libraries(VTK_C_WRAPPER PUBLIC
    VTK::IOXML
    VTK::CommonCore
    VTK::CommonDataModel
    VTK::IOLegacy
)
set_target_properties(VTK_C_WRAPPER PROPERTIES CXX_STANDARD 17)
# --------------------------------------------------------------------
# Fortran/C Core OBJECTライブラリ
# --------------------------------------------------------------------
add_library(CORE OBJECT
    c_utils/signal_flag.c
    c_utils/system_info.c
    c_utils/memory_stats.c
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
    allocate.F90
    deallocate.F90
    string_utils.F90
    types/vector.F90
    types/array.F90
    types/pointer.F90
    types/variable.F90
    types/gauss.F90
    types/types.F90
    vtk/vtk_wrapper.F90
    vtk/vtu_wrapper.F90
    vtk/vtk_constants.F90
    vtk/vtk.F90
    unique.F90
    core.F90
)
target_link_libraries(CORE PRIVATE VTK_C_WRAPPER)
enable_build_flags(CORE)
enable_thirdparty (CORE)