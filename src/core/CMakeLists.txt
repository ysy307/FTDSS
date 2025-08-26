# --------------------------------------------------------------------
# C++/C VTKラッパーライブラリの作成 (変更なし)
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
# Fortran/C Core STATICライブラリ (すべてを一つに統合)
# --------------------------------------------------------------------
add_library(CORE STATIC
    # CORE_UTILS sources
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
    unique.F90

    # CORE_TYPES sources
    allocate.F90
    deallocate.F90
    string_utils.F90
    types/vector.F90
    types/array.F90
    types/pointer.F90
    types/variable.F90
    types/gauss.F90
    types/types.F90
    types/matrix/matrix_interface.F90
    types/matrix/matrix_dense.F90
    types/matrix/matrix_coo.F90
    types/matrix/matrix_crs.F90
    types/matrix/matrix.F90

    # CORE_VTK sources
    vtk/vtk_wrapper.F90
    vtk/vtu_wrapper.F90
    vtk/vtk_constants.F90
    vtk/vtk.F90

    # Main CORE source
    core.F90
)

# 必要なプロパティをすべてCOREターゲットに設定
target_include_directories(CORE PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR} # Fortranモジュール出力先
)

target_link_libraries(CORE PUBLIC
    VTK_C_WRAPPER
)

enable_build_flags(CORE)
enable_thirdparty(CORE)