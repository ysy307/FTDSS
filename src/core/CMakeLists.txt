# CMakeLists.txt for src/core

# ソースファイルをまとめるための変数を初期化
set(CORE_SOURCES "")

# ====================================================================
# 1. Foundational & Dependent Sources (Constants, Validation, Memory, etc.)
# ====================================================================

# --- Constants ---
set(CORE_CONSTANTS_FYPP
    constants/constans_ids.fypp
    constants/constans_values.fypp
    constants/constans_error.fypp
)
# 生成されたソースを変数に格納
fypp_F90("" "${CORE_CONSTANTS_FYPP}" CORE_CONSTANTS_GENERATED)
list(APPEND CORE_SOURCES
    constants/constants_base.F90
    constants/constants_solver.F90
    constants/constants_parallel.F90
    constants/constants_utils.F90
    constants/constants_physical.F90
    constants/constants.F90
    ${CORE_CONSTANTS_GENERATED}
)

# --- Validation ---
set(CORE_VALIDATION_FYPP
    validation/check_length.fypp
    validation/check_nan.fypp
    validation/check_range.fypp
)
fypp_F90("" "${CORE_VALIDATION_FYPP}" CORE_VALIDATION_GENERATED)
list(APPEND CORE_SOURCES
    validation/error.F90
    validation/validation.F90
    ${CORE_VALIDATION_GENERATED}
)

# --- Memory ---
set(CORE_MEMORY_FYPP
    memory/allocate.fypp
    memory/deallocate.fypp
)
fypp_F90("" "${CORE_MEMORY_FYPP}" CORE_MEMORY_GENERATED)
list(APPEND CORE_SOURCES
    memory/memory.F90
    ${CORE_MEMORY_GENERATED}
)

# --- Utils ---
set(CORE_UTILS_FYPP
    utils/unique.fypp
    utils/findings.fypp
)
fypp_F90("" "${CORE_UTILS_FYPP}" CORE_UTILS_GENERATED)
list(APPEND CORE_SOURCES
    utils/string_utils.F90
    utils/system_env.F90
    utils/utils.F90
    ${CORE_UTILS_GENERATED}
)

# --- Types ---
set(CORE_TYPES_FYPP
    types/variable.fypp
)
fypp_F90("" "${CORE_TYPES_FYPP}" CORE_TYPES_GENERATED)
list(APPEND CORE_SOURCES
    types/types.F90
    types/coordinate_array.F90
    types/coordinate.F90
    types/pointer.F90
    types/graph.F90
    types/datetime.F90
    types/solar.F90
    types/physics/state.F90
    types/physics/physics_info.F90
    types/physics/meteorology.F90
    types/math/gauss.F90
    types/matrix/matrix_interface.F90
    types/matrix/matrix_dense.F90
    types/matrix/matrix_coo.F90
    types/matrix/matrix_csr.F90
    types/matrix/matrix_bsr.F90
    types/matrix/matrix_dia.F90
    types/matrix/matrix_factory.F90
    types/matrix/matrix.F90
    types/vector/vector_interface.F90
    types/vector/vector_dp.F90
    types/vector/vector_int.F90
    types/vector/vector.F90
    ${CORE_TYPES_GENERATED}
)


# ====================================================================
# 2. Interop Sources (C/C++ & Fortran wrappers)
# ====================================================================

# --- Utils ---
list(APPEND CORE_SOURCES interop/interop_utils.F90)

# --- Memory Profiling ---
list(APPEND CORE_SOURCES
    interop/memory_profiling/memory_stats.c
    interop/memory_profiling/memory_stats.F90
    interop/memory_profiling/memory_stats_wrapper.F90
)

# --- Signal Handling ---
list(APPEND CORE_SOURCES
    interop/signal_handling/signal_flag.c
    interop/signal_handling/signal_flag.F90
    interop/signal_handling/signal_flag_wrapper.F90
)

# --- System Info ---
list(APPEND CORE_SOURCES
    interop/system_info/system_info.c
    interop/system_info/system_info.F90
    interop/system_info/system_info_wrapper.F90
)

# --- VTK (C++ & Fortran) ---
# C++ sources
list(APPEND CORE_SOURCES
    interop/vtk/reader_vtk.cpp
    interop/vtk/reader_vtu.cpp
    interop/vtk/c_wrapper.cpp
)
# Fortran sources
list(APPEND CORE_SOURCES
    interop/vtk/vtk_wrapper.F90
    interop/vtk/vtu_wrapper.F90
    interop/vtk/vtk_constants.F90
    interop/vtk/vtk_initialize.F90
    interop/vtk/vtu_initialize.F90
    interop/vtk/vtk_base.F90
    interop/vtk/vtk.F90
    interop/interop.F90
)

# ====================================================================
# 3. Main Aggregator & Library Definition
# ====================================================================

# Core entry point
list(APPEND CORE_SOURCES core.F90)

# --------------------------------------------------------
# Define the SINGLE library
# --------------------------------------------------------
add_library(Solvers_Core STATIC ${CORE_SOURCES})

# インクルードディレクトリの設定
target_include_directories(Solvers_Core PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

# C++ (VTK) の設定
set_target_properties(Solvers_Core PROPERTIES CXX_STANDARD 17)

# 外部ライブラリのリンク (VTK等)
# ここでまとめてリンクする
target_link_libraries(Solvers_Core PUBLIC
    VTK::IOXML
    VTK::CommonCore
    VTK::CommonDataModel
    VTK::IOLegacy
)

# コンパイルフラグの適用
enable_build_flags(Solvers_Core)
enable_thirdparty(Solvers_Core)

# Aliasを作成（外部から使いやすくするため）
add_library(Solver::Core ALIAS Solvers_Core)