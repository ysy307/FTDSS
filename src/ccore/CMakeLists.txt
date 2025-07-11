# --------------------------------------------------------------------
# C++/C VTKラッパーライブラリの作成
#    VTKへの依存をこのライブラリに閉じ込めます。
# --------------------------------------------------------------------
add_library(vtk_c_wrapper STATIC
    # C++ソースファイルはここに記述します。
    # パスはプロジェクトの構成に合わせてください。
    vtk/reader.cpp
    vtk/c_wrapper.cpp
)
# VTKのインクルードディレクトリとライブラリをリンク
enable_build_flags(vtk_c_wrapper)

# <--- [新規追加] C++ラッパーが公式VTKライブラリに依存することを明記します
target_link_libraries(vtk_c_wrapper PUBLIC
    VTK::CommonCore
    VTK::IOLegacy
)
set_target_properties(vtk_c_wrapper PROPERTIES CXX_STANDARD 14)
# --------------------------------------------------------------------
# Fortran/C Core OBJECTライブラリ
# --------------------------------------------------------------------
add_library(Core OBJECT
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
    check_nan.f90
    check_range.F90
    allocate.F90
    deallocate.F90
    types/vector.F90
    types/array.F90
    types/pointer.F90
    types/variable.F90
    types/gauss.F90
    types/types.F90
    vtk/vtk_wrapper.F90
    vtk/vtk_constants.F90
    vtk/vtk.F90
    unique.F90
    core.F90
)
target_link_libraries(Core PRIVATE vtk_c_wrapper)
enable_build_flags(Core)
enable_thirdparty (Core)