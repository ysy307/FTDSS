# CMakeLists.txt for src/core

# ====================================================================
# 1. Foundational Libraries
# ====================================================================

# --- Validation Library (依存なし) ---
set(CORE_VALIDATION_SOURCES
    validation/error.F90
    validation/check_nan.F90
    validation/check_range.F90
)
add_library(Core_Validation STATIC ${CORE_VALIDATION_SOURCES})
target_include_directories(Core_Validation PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
enable_build_flags(Core_Validation)
enable_thirdparty(Core_Validation)


# ====================================================================
# 2. Dependent Libraries
# ====================================================================

# --- Memory Library (Validationに依存) ---
set(CORE_MEMORY_SOURCES
    memory/allocate.F90
    memory/deallocate.F90
)
add_library(Core_Memory STATIC ${CORE_MEMORY_SOURCES})
target_include_directories(Core_Memory PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
target_link_libraries(Core_Memory PUBLIC Core_Validation)
enable_build_flags(Core_Memory)

# --- Utils Library (Memoryに依存) ---
set(CORE_UTILS_SOURCES
    utils/string_utils.F90
    utils/system_env.F90
    utils/unique.F90
)
add_library(Core_Utils STATIC ${CORE_UTILS_SOURCES})
target_include_directories(Core_Utils PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
target_link_libraries(Core_Utils PUBLIC Core_Memory)
enable_build_flags(Core_Utils)

# --- Types Library (Memoryに依存) ---
set(CORE_TYPES_SOURCES
    types/types.F90
    types/array.F90
    types/vector.F90
    types/pointer.F90
    types/variable.F90
    types/gauss.F90
    types/matrix/matrix_interface.F90
    types/matrix/matrix_dense.F90
    types/matrix/matrix_coo.F90
    types/matrix/matrix_crs.F90
    types/matrix/matrix.F90
)
add_library(Core_Types STATIC ${CORE_TYPES_SOURCES})
target_include_directories(Core_Types PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
target_link_libraries(Core_Types PUBLIC Core_Memory)
enable_build_flags(Core_Types)


# ====================================================================
# 3. Interop Libraries
# ====================================================================
set(CORE_INTEROP_BASE_DEPS Core_Types Core_Memory Core_Utils)

# --- Interop Utils (他のラッパーから使われる) ---
set(CORE_INTEROP_UTILS_SOURCES interop/interop_utils.F90)
add_library(Core_Interop_Utils STATIC ${CORE_INTEROP_UTILS_SOURCES})
target_include_directories(Core_Interop_Utils PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
target_link_libraries(Core_Interop_Utils PUBLIC ${CORE_INTEROP_BASE_DEPS})

# --- Memory Profiling Component ---
# Part 1: C implementation + Fortran interface definition
add_library(Core_Interop_MemoryProfiling_Impl STATIC
    interop/memory_profiling/memory_stats.c
    interop/memory_profiling/memory_stats.F90
)
target_include_directories(Core_Interop_MemoryProfiling_Impl PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
# Part 2: High-level Fortran wrapper
add_library(Core_Interop_MemoryProfiling STATIC
    interop/memory_profiling/memory_stats_wrapper.F90
)
target_include_directories(Core_Interop_MemoryProfiling PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
target_link_libraries(Core_Interop_MemoryProfiling PUBLIC
    Core_Interop_MemoryProfiling_Impl
    Core_Interop_Utils
    ${CORE_INTEROP_BASE_DEPS}
)
enable_build_flags(Core_Interop_MemoryProfiling)

# --- Signal Handling Component ---
add_library(Core_Interop_SignalHandling_Impl STATIC
    interop/signal_handling/signal_flag.c
    interop/signal_handling/signal_flag.F90
)
target_include_directories(Core_Interop_SignalHandling_Impl PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
add_library(Core_Interop_SignalHandling STATIC
    interop/signal_handling/signal_flag_wrapper.F90
)
target_include_directories(Core_Interop_SignalHandling PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
target_link_libraries(Core_Interop_SignalHandling PUBLIC
    Core_Interop_SignalHandling_Impl
    Core_Interop_Utils
    ${CORE_INTEROP_BASE_DEPS}
)
enable_build_flags(Core_Interop_SignalHandling)
enable_thirdparty(Core_Interop_SignalHandling)


# --- System Info Component ---
add_library(Core_Interop_SystemInfo_Impl STATIC
    interop/system_info/system_info.c
    interop/system_info/system_info.F90
)
target_include_directories(Core_Interop_SystemInfo_Impl PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
add_library(Core_Interop_SystemInfo STATIC
    interop/system_info/system_info_wrapper.F90
)
target_include_directories(Core_Interop_SystemInfo PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
target_link_libraries(Core_Interop_SystemInfo PUBLIC
    Core_Interop_SystemInfo_Impl
    Core_Interop_Utils
    ${CORE_INTEROP_BASE_DEPS}
)
enable_build_flags(Core_Interop_SystemInfo)

# --- VTK Libraries ---
set(CORE_INTEROP_VTK_CXX_SOURCES
    interop/vtk/reader_vtk.cpp
    interop/vtk/reader_vtu.cpp
    interop/vtk/c_wrapper.cpp
)
add_library(Core_Interop_VTK_CXX STATIC ${CORE_INTEROP_VTK_CXX_SOURCES})
target_link_libraries(Core_Interop_VTK_CXX PUBLIC
    VTK::IOXML VTK::CommonCore VTK::CommonDataModel VTK::IOLegacy
)
set_target_properties(Core_Interop_VTK_CXX PROPERTIES CXX_STANDARD 17)
enable_build_flags(Core_Interop_VTK_CXX)

set(CORE_INTEROP_VTK_FORT_SOURCES
    interop/vtk/vtk_wrapper.F90
    interop/vtk/vtu_wrapper.F90
    interop/vtk/vtk_constants.F90
    interop/vtk/vtk_initialize.F90
    interop/vtk/vtu_initialize.F90
    interop/vtk/vtk_base.F90
    interop/vtk/vtk.F90
)
add_library(Core_Interop_VTK_Fortran STATIC ${CORE_INTEROP_VTK_FORT_SOURCES})
target_include_directories(Core_Interop_VTK_Fortran PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
target_link_libraries(Core_Interop_VTK_Fortran PUBLIC
    Core_Interop_VTK_CXX
    Core_Interop_Utils
    ${CORE_INTEROP_BASE_DEPS}
)


# ====================================================================
# 4. Aggregator Interface Library
# ====================================================================
# ====================================================================
# 3. Aggregator Interface Library
# ====================================================================

# --- Main Aggregator Library ---
# core.F90は他の全てのCoreコンポーネントに依存する
set(CORE_MAIN_SOURCE core.F90)
add_library(Core_Main STATIC ${CORE_MAIN_SOURCE})
target_include_directories(Core_Main PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
# ★ Core_Mainが他の全ての部品ライブラリに依存することを明記 ★
target_link_libraries(Core_Main PUBLIC
    Core_Types
    Core_Memory
    Core_Validation
    Core_Utils
    Core_Interop_MemoryProfiling
    Core_Interop_SignalHandling
    Core_Interop_SystemInfo
    Core_Interop_VTK_Fortran
    Core_Interop_Utils
)

# --- Public Facing Interface Library ---
add_library(Core_Interface INTERFACE)

# Core_Mainが全てに依存するため、InterfaceはCore_Mainにリンクするだけで良い
target_link_libraries(Core_Interface INTERFACE
    Core_Main
)

add_library(Solver::Core ALIAS Core_Interface)